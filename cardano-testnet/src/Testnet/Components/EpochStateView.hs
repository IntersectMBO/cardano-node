{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

{- | Shared epoch state view for integration tests.

A background thread subscribes to the node via chain-sync (through 'foldEpochState')
and pushes the latest ledger state into a pair of 'TVar's each time a new block
arrives. Test code reads this view to inspect the chain state and polls for
conditions to become true.

== How the pieces fit together

'EpochStateView' holds two 'TVar's (private to this module):

* The current @(AnyNewEpochState, SlotNo, BlockNo)@, or a status value while the
  view is initialising or after the background thread has errored.
* A monotonically-increasing @Word64@ version counter, bumped on every write to
  the state 'TVar'. This counter is the synchronisation channel between the
  writer thread and any waiter: a caller records the current version, performs
  its check, and then blocks on STM until the version differs.

The writer is set up by 'getEpochStateView'. It launches a long-lived fold (via
'asyncRegister_') that runs the chain-sync client against the node. For every
block the node streams, it writes the derived @NewEpochState@ into the state
'TVar' and bumps the version. If the fold terminates with an error, it writes
the error status and bumps the version so any waiting threads can observe the
failure.

== Retry loop

'retryUntilRightM' is the core retry primitive. It takes an action that returns
@Either e a@ and a 'TestnetWaitPeriod' deadline expressed in chain units
(epochs\/blocks\/slots). Each iteration:

1. Samples the current version /before/ running the action, so that updates
   landing during the action are not missed.
2. Runs the action. On 'Right' it returns immediately.
3. On 'Left' it checks whether the chain-unit deadline has been exceeded; if so
   it returns the last 'Left'.
4. Otherwise it blocks on STM until the version advances past the sampled
   value, then loops.

The STM block is performed by an internal helper that combines a fast path
(return immediately if the version already differs — common when the action
took long enough that a block landed during it) with an awaited path (register
a fallback timer and block on STM until either the version advances or the
timer fires). The fallback is a stall-detection heartbeat, not a wait duration
— its only job is to guarantee no single STM transaction blocks indefinitely.
What actually terminates the loop is either a successful action or the
chain-unit deadline.

'retryUntilJustM', 'retryUntilM', 'waitForEpochs', and 'waitForBlocks' are thin
wrappers around 'retryUntilRightM'.
-}
module Testnet.Components.EpochStateView
  ( -- * Shared epoch state
    EpochStateView
  , getEpochStateView

    -- * Reading the state
  , getEpochState
  , getEpochStateDetails
  , getSlotNumber
  , getBlockNumber
  , getCurrentEpochNo

    -- * Waiting for state changes
  , TestnetWaitPeriod (..)
  , waitForEpochs
  , waitForBlocks
  , retryUntilRightM
  , retryUntilJustM
  , retryUntilM
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (EpochInterval (..))

import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Control.Applicative ((<|>))
import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import qualified Control.Concurrent.STM as STM
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT, runMaybeT)
import           Control.Monad.Trans.Resource
import qualified Data.Time.Clock as DTC
import           Data.Word (Word64)
import           GHC.Stack
import           Lens.Micro ((^.))

import           Testnet.Process.RunIO (liftIOAnnotated)
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H

import           UnliftIO.STM (atomically, readTVarIO, registerDelay)

-- | Wait for the number of epochs
waitForEpochs
  :: HasCallStack
  => MonadTest m
  => MonadAssertion m
  => MonadIO m
  => EpochStateView
  -> EpochInterval  -- ^ Number of epochs to wait
  -> m EpochNo -- ^ The epoch number reached
waitForEpochs epochStateView interval = withFrozenCallStack $ do
  void . retryUntilRightM epochStateView (WaitForEpochs interval) . pure $ Left ()
  getCurrentEpochNo epochStateView

-- | Wait for the requested number of blocks
waitForBlocks
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> Word64 -- ^ Number of blocks to wait
  -> m BlockNo -- ^ The block number reached
waitForBlocks epochStateView numberOfBlocks = withFrozenCallStack $ do
  BlockNo startingBlockNumber <- getBlockNumber epochStateView
  H.note_ $ "Current block number: " <> show startingBlockNumber <> ". "
    <> "Waiting for " <> show numberOfBlocks <> " blocks"
  void . retryUntilRightM epochStateView (WaitForBlocks numberOfBlocks) . pure $ Left ()
  getBlockNumber epochStateView

-- | Deadline for 'retryUntilRightM' and its wrappers, expressed in chain units
-- rather than wall-clock time. Each iteration of the loop only advances when the
-- chain advances, so the deadline measures how much chain progress the caller is
-- willing to wait for.
data TestnetWaitPeriod
  = WaitForEpochs EpochInterval
  | WaitForBlocks Word64
  | WaitForSlots Word64
  deriving Eq

instance Show TestnetWaitPeriod where
  show = \case
    WaitForEpochs (EpochInterval n) -> "WaitForEpochs " <> show n
    WaitForBlocks n -> "WaitForBlocks " <> show n
    WaitForSlots n -> "WaitForSlots " <> show n

-- | Core retry loop. Returns early on 'Right'; on 'Left', blocks via STM until
-- the 'EpochStateView' is updated (with a safety fallback timeout) and retries.
-- Gives up and returns the last 'Left' once the 'TestnetWaitPeriod' deadline is
-- exceeded.
retryUntilRightM
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> TestnetWaitPeriod
  -> m (Either e a)
  -> m (Either e a)
retryUntilRightM esv timeout act = withFrozenCallStack $ do
  startingValue <- getCurrentValue
  go $ startingValue + timeoutW64
  where
    go deadline = do
      -- Sample the version before running 'act' so that any update landing during 'act'
      -- makes 'awaitStateUpdateTimeout' return without blocking, rather than waiting for
      -- the next update and adding a block/epoch of latency.
      versionBeforeAct <- readTVarIO $ epochStateVersion esv
      act >>= \case
        r@(Right _) -> pure r
        l@(Left _) -> do
          cv <- getCurrentValue
          if cv > deadline
            then pure l
            else awaitStateUpdateTimeout esv stallHeartbeatSeconds versionBeforeAct *> go deadline

    (getCurrentValue, timeoutW64) = case timeout of
      WaitForEpochs (EpochInterval n) -> (unEpochNo <$> getCurrentEpochNo esv, fromIntegral n)
      WaitForSlots n                  -> (unSlotNo <$> getSlotNumber esv, n)
      WaitForBlocks n                 -> (unBlockNo <$> getBlockNumber esv, n)

    -- | Stall-detection heartbeat, not a wait duration. Releases the STM transaction at
    -- most this often so the loop can re-check the chain-unit deadline; in normal
    -- operation a block update lands first and the heartbeat never fires.
    stallHeartbeatSeconds :: DTC.NominalDiffTime
    stallHeartbeatSeconds = 300

-- | Retries the action until it returns 'Just' or the timeout is reached
retryUntilJustM
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> TestnetWaitPeriod -- ^ timeout for an operation
  -> m (Maybe a)
  -> m a
retryUntilJustM esv timeout act = withFrozenCallStack $
  retryUntilRightM esv timeout (maybe (Left ()) Right <$> act) >>= \case
    Right a -> pure a
    Left () -> do
      H.note_ $ "Action did not result in 'Just' - waited for: " <> show timeout
      H.failure

-- | Like 'retryUntilJustM' but takes a plain action and a predicate instead of
-- an action returning 'Maybe'. On timeout, annotates the last value that failed
-- the predicate. Intermediate attempts produce no annotations.
retryUntilM
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => Show a
  => EpochStateView
  -> TestnetWaitPeriod -- ^ timeout
  -> m a              -- ^ action to retry
  -> (a -> Bool)      -- ^ predicate that must hold
  -> m a
retryUntilM esv timeout act predicate = withFrozenCallStack $
  retryUntilRightM esv timeout ((\r -> if predicate r then Right r else Left r) <$> act) >>= \case
    Right a -> pure a
    Left r -> do
      H.noteShow_ r
      H.note_ $ "Predicate not satisfied after: " <> show timeout
      H.failure

-- | Status of the 'EpochStateView' background thread when epoch state is not yet available
data EpochStateStatus
  = EpochStateNotInitialised
  -- ^ The background thread has not yet received any epoch state from the node
  | EpochStateFoldError !FoldBlocksError
  -- ^ The background thread encountered an error while folding blocks

-- | A read-only handle to an epoch state that is kept fresh by a background thread.
--
-- The constructor is private. Reads go through the accessor functions exported from
-- this module ('getEpochState', 'getBlockNumber', 'getSlotNumber',
-- 'getEpochStateDetails', 'getCurrentEpochNo') so that callers cannot accidentally
-- race against the version-counter synchronisation contract described in the module
-- header.
data EpochStateView = EpochStateView
  { epochStateView :: !(TVar (Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)))
  , epochStateVersion :: !(TVar Word64)
  }

-- | Block until the epoch state version advances past the provided previously sampled
-- version, or until the fallback timeout expires. Returns immediately if the current
-- version already differs, so callers can sample before running an action and avoid
-- missing updates that land during the action. Returns 'Nothing' on timeout.
-- All threads blocked on the same 'EpochStateView' wake up on each update.
awaitStateUpdateTimeout
  :: MonadIO m
  => EpochStateView
  -> DTC.NominalDiffTime -- ^ Fallback timeout
  -> Word64 -- ^ Previously sampled version
  -> m (Maybe (Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)))
awaitStateUpdateTimeout EpochStateView{epochStateVersion, epochStateView} timeout sinceVersion = runMaybeT $ fastResult <|> awaitedResult
  where
    -- Fast path: if the version already differs, read state and version atomically and return
    -- without allocating a 'registerDelay' timer. This avoids accumulating timer-queue entries
    -- when callers sample a stale version and an update has already landed.
    fastResult = mapMaybeT atomically $ do
      v <- lift $ readTVar epochStateVersion
      guard $ v /= sinceVersion
      lift $ readTVar epochStateView

    awaitedResult = MaybeT $ do
      timedOutVar <- registerDelay . ceiling $ timeout * 1_000_000
      atomically $ do
        v <- readTVar epochStateVersion
        timedOut <- readTVar timedOutVar
        case (v /= sinceVersion, timedOut) of
          (True, _) -> Just <$> readTVar epochStateView
          (_, True) -> pure Nothing
          _ -> STM.retry

-- | Get epoch state from the view. If the state isn't available, retry waiting up to 25 seconds. Fails
-- immediately if the background thread encountered an error, or after 25 seconds if not yet initialised.
getEpochState
  :: HasCallStack
  => MonadTest m
  => MonadAssertion m
  => MonadIO m
  => EpochStateView
  -> m AnyNewEpochState
getEpochState epochStateView =
  withFrozenCallStack $ (\(nes, _, _) -> nes) <$> getEpochStateDetails epochStateView

getBlockNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m BlockNo -- ^ The number of last produced block
getBlockNumber epochStateView =
  withFrozenCallStack $ (\(_, _, blockNumber) -> blockNumber) <$> getEpochStateDetails epochStateView

getSlotNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m SlotNo -- ^ The current slot number
getSlotNumber epochStateView =
  withFrozenCallStack $ (\(_, slotNumber, _) -> slotNumber) <$> getEpochStateDetails epochStateView

-- | Access the current epoch state. Returns immediately if state is already available.
-- Blocks up to 25 seconds waiting for initialisation if the background thread has not yet
-- received any epoch state. Fails immediately if the background thread encountered an error.
getEpochStateDetails
  :: HasCallStack
  => MonadAssertion m
  => MonadTest m
  => MonadIO m
  => EpochStateView
  -> m (AnyNewEpochState, SlotNo, BlockNo)
getEpochStateDetails EpochStateView{epochStateView} = withFrozenCallStack $
  -- Fast path: read the TVar outside STM block so we don't register a pointless
  -- 'initTimeoutSeconds' timer on every call. These getters run inside tight
  -- retry loops, and the unused timer-queue entries would otherwise accumulate.
  readTVarIO epochStateView
    >>= awaitForState
    >>= failEpochStateFoldError
  where
    initTimeoutSeconds :: Int
    initTimeoutSeconds = 25

    awaitForState
      :: MonadIO n
      => Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)
      -> n (Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo))
    awaitForState = \case
      Left EpochStateNotInitialised -> do
        -- register delay only when we're starting to retry
        timedOutVar <- registerDelay $ initTimeoutSeconds * 1_000_000
        atomically $ do
          state' <- readTVar epochStateView
          state' <$ case state' of
              -- retry until timeout
              Left EpochStateNotInitialised -> readTVar timedOutVar >>= guard
              _ -> pure ()
      state -> pure state

    failEpochStateFoldError
      :: (HasCallStack, MonadTest n)
      => Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)
      -> n (AnyNewEpochState, SlotNo, BlockNo)
    failEpochStateFoldError = \case
      Right details -> pure details
      Left (EpochStateFoldError err) -> do
        H.note_ $ "EpochStateView background thread failed: " <> docToString (prettyError err)
        H.failure
      Left EpochStateNotInitialised -> do
        H.note_ $ "EpochStateView has not been initialised within " <> show initTimeoutSeconds <> " seconds"
        H.failure


-- | Create a background thread listening for new epoch states. New epoch states are available to access
-- through 'EpochStateView', using query functions.
-- The background thread captures any 'FoldBlocksError' into the shared state, so that consumers
-- (e.g. 'getEpochStateDetails') can fail immediately with a meaningful error message instead of
-- waiting for the full timeout.
getEpochStateView
  :: HasCallStack
  => MonadResource m
  => MonadTest m
  => NodeConfigFile In -- ^ node Yaml configuration file path
  -> SocketPath -- ^ node socket path
  -> m EpochStateView
getEpochStateView nodeConfigFile socketPath = withFrozenCallStack $ do
  epochStateView <- H.evalIO $ newTVarIO $ Left EpochStateNotInitialised
  epochStateVersion <- H.evalIO $ newTVarIO 0
  void . asyncRegister_ $ do
    result <- runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation (EpochNo maxBound) ()
      $ \epochState slotNumber blockNumber -> do
          liftIOAnnotated . atomically $ do
            writeTVar epochStateView $ Right (epochState, slotNumber, blockNumber)
            modifyTVar' epochStateVersion (+ 1)
          pure ConditionNotMet
    case result of
      Left err -> atomically $ do
        writeTVar epochStateView $ Left $ EpochStateFoldError err
        modifyTVar' epochStateVersion (+ 1)
      Right _ -> pure ()
  pure $ EpochStateView epochStateView epochStateVersion

-- | Return current-ish epoch number.
-- Because we're using Ledger's 'NewEpochState', the returned epoch number won't be reflecting the current
-- epoch number during the transiontion between the epochs. In other cases it will be the true number of the
-- current epoch.
getCurrentEpochNo
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> m EpochNo
getCurrentEpochNo epochStateView = withFrozenCallStack $ do
  AnyNewEpochState _ newEpochState _ <- getEpochState epochStateView
  pure $ newEpochState ^. L.nesELL
