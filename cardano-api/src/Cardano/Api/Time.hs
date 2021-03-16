module Cardano.Api.Time (
    RelativeTime (..),
    EpochSize (..),
    relativeTimeToSlot,
    slotToRelativeTime,
    slotToEpoch,
    slotInEpoch,
    epochToSlot,
    epochSize
  ) where

-- import           Cardano.Api.Block
import           Cardano.Api.Block
import           Cardano.Api.NewApiStuff
import           Cardano.Api.ProtocolParameters (EpochSize (..))
import           Cardano.Prelude
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..))
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

-- TODO Remove this comment (move to PR comment)
-- from Qry module:
-- Which parts of the return value are actually used (excluding tests)
-- * wallclockToSlot
--    * SlotNo
-- * slotToWallclock
--    * RelativeTime
-- * slotToEpoch'
--    * EpochNo
--    * Word64: Slot within the epoch
-- * slotToEpoch
--    * NOT USED
-- * epochToSlot'
--    * SlotNo (the only returned value)
-- * epochToSlot
--    * NOT USED
-- * epochToSize
--    * EpochSize (the only returned value)
--
-- So let's just make a function for each of these points and return only a
-- single value.

-- | Convert wall clock time (@RelativeTime@) to @Slot@. Returns Nothing if
-- the @Slot@ is in the future relative to the @LedgerState@.
relativeTimeToSlot :: Env -> LedgerState -> RelativeTime -> Maybe SlotNo
relativeTimeToSlot env ledgerState
  = fmap (\(slot, _, _) -> slot)
  . runQuery env ledgerState
  . Qry.wallclockToSlot


slotToRelativeTime :: Env -> LedgerState -> SlotNo -> Maybe RelativeTime
slotToRelativeTime env ledgerState
  = fmap fst
  . runQuery env ledgerState
  . Qry.slotToWallclock

slotToEpoch :: Env -> LedgerState -> SlotNo -> Maybe EpochNo
slotToEpoch env ledgerState
  = fmap fst
  . runQuery env ledgerState
  . Qry.slotToEpoch'

slotInEpoch :: Env -> LedgerState -> SlotNo -> Maybe Word64
slotInEpoch env ledgerState
  = fmap snd
  . runQuery env ledgerState
  . Qry.slotToEpoch'

epochToSlot :: Env -> LedgerState -> EpochNo -> Maybe SlotNo
epochToSlot env ledgerState
  = runQuery env ledgerState
  . Qry.epochToSlot'

epochSize :: Env -> LedgerState -> EpochNo -> Maybe EpochSize
epochSize env ledgerState
  = runQuery env ledgerState
  . Qry.epochToSize

runQuery :: Env -> LedgerState -> Consensus.Qry a -> Maybe a
runQuery
  (Env{envLedgerConfig = ledgerConfig})
  (LedgerState ledgerState)
  query
  = case runQuery' ledgerConfig ledgerState query of
     Left Consensus.PastHorizon{} -> Nothing
     Right a -> Just a

runQuery'
  :: Consensus.HardForkLedgerConfig (Consensus.CardanoEras Consensus.StandardCrypto)
  -> Consensus.LedgerState (Consensus.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto))
  -> Consensus.Qry a
  -> Either Consensus.PastHorizonException a
runQuery' ledgerConfig ledgerState qry = Consensus.runQuery qry summary
  where
    summary = Ouroboros.Consensus.HardFork.Abstract.hardForkSummary ledgerConfig ledgerState
