{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Slot
  ( syncProgress

  , Percentage(..)
  , SyncProgress(..)
  , SyncTolerance(..)
  , TimeInterpreter(..)

  , getSyncProgress
  , mkTimeInterpreter
  , neverFails
  ) where

import           Prelude

import           Cardano.Slotting.Slot (EpochSize (..), SlotNo)
import           Control.DeepSeq (NFData)
import           Control.Exception (throwIO)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import           Data.ByteString (ByteString)
import           Data.Either (fromRight)
import           Data.Hashable (Hashable)
import           Data.Kind (Type)
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           GHC.TypeLits (Symbol)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   SystemStart (..), diffRelTime)
import           Ouroboros.Consensus.HardFork.History.Qry (Interpreter, PastHorizonException (..),
                   mkInterpreter, slotToWallclock)
import           Ouroboros.Consensus.HardFork.History.Summary (neverForksSummary)
import           Quiet (Quiet (..))

import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as Cardano
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF

data TimeInterpreterLog = MsgInterpreterPastHorizon
  (Maybe String) -- ^ Reason for why the failure should be impossible
  StartTime
  PastHorizonException
  deriving (Show)

-- | Number of slots in a single epoch
newtype EpochLength = EpochLength { unEpochLength :: Word32 }
    deriving (Show, Eq, Generic)

-- | Duration of a single slot.
newtype SlotLength = SlotLength { unSlotLength :: NominalDiffTime }
    deriving (Show, Eq, Generic)

-- | Blockchain start time
newtype StartTime = StartTime UTCTime
    deriving (Show, Eq, Ord, Generic)

-- | A @TimeInterpreter@ is a way for the wallet to run things of type @Qry a@,
-- with a system start time as context.
data TimeInterpreter m = forall eras. TimeInterpreter
  { interpreter :: m (Interpreter eras)
  , blockchainStartTime :: StartTime
  }

newtype Hash (tag :: Symbol) = Hash { getHash :: ByteString }
  deriving stock (Generic, Eq, Ord)
  deriving (Read, Show) via (Quiet (Hash tag))
  deriving anyclass (NFData, Hashable)

newtype SyncTolerance = SyncTolerance NominalDiffTime
  deriving stock (Generic, Eq, Show)

data SyncProgress
  = Ready
  | Syncing Percentage
  | NotResponding
  deriving (Generic, Eq, Show)

newtype Percentage = Percentage
  { getPercentage :: Rational
  }
  deriving stock (Generic, Show, Eq, Ord)

-- | A query for time, slot and epoch conversions. Can be interpreted using
-- @interpretQuery@.
--
-- == Differences to the underlying consensus 'Ouroboros.Consensus.HardFork.History.Qry.Qry'
--
-- @HF.Qry@ can only be interpreted in a
-- single era. If you have
--
-- @
--     q1 = epochOf someSlotInByron
--     q2 = epochOf someSlotInShelley
-- @
--
-- @HF.interpretQuery@ could interpret both individually, but
--
-- @
--    q3 = q1 >> q2
-- @
--
-- would fail.
--
-- This wrapper exists to fix this.
--
-- We also provide @QStartTime@.
--
data Qry :: Type -> Type where
  -- | A @HF.Qry@ can only be run inside a single era.
  EraContainedQry :: HF.Qry a -> Qry a
  QStartTime :: Qry StartTime
  QPure :: a -> Qry a
  QBind :: Qry a -> (a -> Qry b) -> Qry b

data MkPercentageError = PercentageOutOfBoundsError deriving (Show, Eq)

-- | Query the relative time at which a slot starts.
slotToRelTime :: SlotNo -> Qry RelativeTime
slotToRelTime = EraContainedQry . fmap fst . slotToWallclock

-- | Safe constructor for 'Percentage'
--
-- Takes an input in the range [0, 1].
mkPercentage :: Rational -> Either MkPercentageError Percentage
mkPercentage r
  | r < 0 = Left PercentageOutOfBoundsError
  | r > 1 = Left PercentageOutOfBoundsError
  | otherwise = pure . Percentage $ r

-- | Estimate restoration progress based on:
--
-- - The current local tip
-- - The last slot
--
-- For the sake of this calculation, we are somewhat conflating the definitions
-- of slots and block height. Because we can't reliably _trust_ that the current
-- node is actually itself synced with the network. So, we compute the progress
-- as:
--
-- @
-- p = h / (h + X)
-- @
--
-- Where:
--
-- - @h@: the number of blocks we have ingested so far.
-- - @X@: the estimatd remaining slots to reach the network tip.
--
-- Initially, `X` gives a relatively poor estimation of the network height, as
-- it assumes that every next slot will be a block. But, as we ingest blocks,
-- `h` becomes bigger and `X` becomes smaller making the progress estimation
-- better and better. At some point, `X` is null, and we have `p = h / h`
syncProgress
  :: (HasCallStack, Monad m)
  => SyncTolerance
  -- ^ A time tolerance inside which we consider ourselves synced
  -> TimeInterpreter m
  -- ^ Converts slots to actual time.
  -> SlotNo
  -- ^ Slot number of tip
  -> RelativeTime
  -- ^ Current Time
  -> m (Either PastHorizonException SyncProgress)
syncProgress (SyncTolerance tolerance) ti tipSlotNo now = do
  timeCoveredResult <- interpretQuery ti $ slotToRelTime tipSlotNo
  case timeCoveredResult of
    Right timeCovered -> do
      let progress
            | now == start = 0
            | otherwise = convert timeCovered % convert now

      if withinTolerance timeCovered now then
        return (Right Ready)
      else
        return
          . Right
          . Syncing
          . fromRight (error (errMsg progress))
          . mkPercentage
          . toRational
          $ progress
    Left e -> return (Left e)
  where
    start = RelativeTime 0

    convert :: RelativeTime -> Int
    convert = round . (* 1000) . getRelativeTime

    withinTolerance a b =  b `diffRelTime` a <= tolerance

    errMsg x = "syncProgress: " ++ show x ++ " is out of bounds"

-- | Run a query.
interpretQuery
  :: HasCallStack
  => Monad m
  => TimeInterpreter m
  -> Qry a
  -> m (Either PastHorizonException a)
interpretQuery (TimeInterpreter getI start) qry = do
  i <- getI
  return (runQuery start i qry)

runQuery
  :: HasCallStack
  => StartTime
  -> Interpreter xs
  -> Qry a
  -> Either HF.PastHorizonException a
runQuery startTime int = go
  where
    go :: Qry a -> Either HF.PastHorizonException a
    go (EraContainedQry q) = HF.interpretQuery int q
    go (QPure a) = return a
    go (QBind x f) = do go x >>= go . f
    go QStartTime = return startTime

getSyncProgress
    :: HasCallStack
    => SyncTolerance
    -> SlotNo
    -> TimeInterpreter (ExceptT PastHorizonException IO)
    -> IO (Either PastHorizonException SyncProgress)
getSyncProgress st tipSlotNo timeInterpreter  = do
  now <- currentRelativeTime ti
  syncProgress
    st
    (neverFails timeInterpreter)
    tipSlotNo
    now
  where
    ti :: TimeInterpreter (MaybeT IO)
    ti = hoistTimeInterpreter exceptToMaybeT timeInterpreter

-- | The current system time, compared to the blockchain start time of the given
-- 'TimeInterpreter'.
--
-- If the current time is before the system start (this would only happen when
-- launching testnets), the relative time is reported as 0.
currentRelativeTime :: MonadIO m => TimeInterpreter n -> m RelativeTime
currentRelativeTime = liftIO . getCurrentTimeRelativeFromStart . blockchainStartTime

-- | Takes a motivation of why @TimeInterpreter@ shouldn't fail interpreting
-- queries.
--
-- Unexpected @PastHorizonException@s will be thrown in IO, and traced with
-- Error severity along with the provided motivation.
neverFails
  :: TimeInterpreter (ExceptT PastHorizonException IO)
  -> TimeInterpreter IO
neverFails = f . hoistTimeInterpreter (runExceptT >=> eitherToIO)
  where
    eitherToIO (Right x) = pure x
    eitherToIO (Left e) = throwIO e

    f (TimeInterpreter getI ss) = TimeInterpreter
      { interpreter = getI
      , blockchainStartTime = ss
      }

-- | Change the underlying monad of the TimeInterpreter with a natural
-- transformation.
hoistTimeInterpreter
  :: (forall a. m a -> n a)
  -> TimeInterpreter m
  -> TimeInterpreter n
hoistTimeInterpreter f (TimeInterpreter getI ss) = TimeInterpreter
  { interpreter = f getI
    -- NOTE: interpreter ti cannot throw PastHorizonException, but
    -- this way we don't have to carry around yet another type parameter.
  , blockchainStartTime = ss
  }

-- | The current system time, compared to the given blockchain start time.
--
-- If the current time is before the system start (this would only happen when
-- launching testnets), let's just say we're in epoch 0.
--
-- TODO: Use io-sim-classes for easier testing.
getCurrentTimeRelativeFromStart :: StartTime -> IO RelativeTime
getCurrentTimeRelativeFromStart start = toRelativeTimeOrZero start <$> getCurrentTime

-- | Convert an absolute time to a relative time. If the absolute time is before
-- the system start, consider the relative time to be the system start
-- time. This function can never fail.
toRelativeTimeOrZero :: StartTime -> UTCTime -> RelativeTime
toRelativeTimeOrZero start = fromMaybe (RelativeTime 0) . toRelativeTime start

-- | Same as 'Cardano.toRelativeTime', but has error handling for times before
-- the system start. No other functions in this module will accept UTC times.
toRelativeTime :: StartTime -> UTCTime -> Maybe RelativeTime
toRelativeTime (StartTime start) utc
  | utc < start = Nothing
  | otherwise = Just $ Cardano.toRelativeTime (SystemStart start) utc

-- | Set up a 'TimeInterpreter' for a given start time, and an 'Interpreter'
-- queried from the ledger layer.
mkTimeInterpreter
    :: Monad m
    => StartTime
    -> EpochSize
    -> Cardano.SlotLength
    -> TimeInterpreter (ExceptT PastHorizonException m)
mkTimeInterpreter start sz len = TimeInterpreter
    { interpreter = pure (mkInterpreter (neverForksSummary sz len))
    , blockchainStartTime = start
    }
