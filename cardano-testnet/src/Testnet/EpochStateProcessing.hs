{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  , waitForGovActionVotes
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (EpochInterval (..), GovActionId (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.API as L
import           Cardano.Ledger.Shelley.LedgerState (newEpochStateGovStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Control.Monad
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Word (Word16)
import           GHC.Exts (IsList (toList), toList)
import           GHC.Stack
import           Lens.Micro (to, (^.))

import           Testnet.Components.Query (EpochStateView, watchEpochStateUpdate)

import           Hedgehog
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H

maybeExtractGovernanceActionIndex
  :: HasCallStack
  => TxId -- ^ transaction id searched for
  -> AnyNewEpochState
  -> Maybe Word16
maybeExtractGovernanceActionIndex txid (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "Governance actions only available in Conway era onwards")
    (\ceo -> conwayEraOnwardsConstraints ceo $ do
        let proposals = newEpochState ^. L.newEpochStateGovStateL . L.proposalsGovStateL
        Map.foldlWithKey' (compareWithTxId txid) Nothing (L.proposalsActionsMap proposals)
    )
    sbe
  where
    compareWithTxId (TxId ti1) Nothing (GovActionId (L.TxId ti2) (L.GovActionIx gai)) _
      | ti1 == L.extractHash ti2 = Just gai
    compareWithTxId _ x _ _ = x

-- | Wait for the last gov action proposal in the list to have DRep or SPO votes.
waitForGovActionVotes
  :: forall m. HasCallStack
  => MonadAssertion m
  => MonadTest m
  => MonadIO m
  => EpochStateView -- ^ Current epoch state view. It can be obtained using the 'getEpochStateView' function.
  -> EpochInterval -- ^ The maximum wait time in epochs.
  -> m ()
waitForGovActionVotes epochStateView maxWait = withFrozenCallStack $ do
  mResult <- watchEpochStateUpdate epochStateView maxWait checkForVotes
  when (isNothing mResult) $
    H.failMessage callStack "waitForGovActionVotes: No votes appeared before timeout."
  where
    checkForVotes
      :: HasCallStack
      => (AnyNewEpochState, SlotNo, BlockNo)
      -> m (Maybe ())
    checkForVotes (AnyNewEpochState actualEra newEpochState, _, _) = withFrozenCallStack $ do
      caseShelleyToBabbageOrConwayEraOnwards
        (const $ H.note_ "Only Conway era onwards is supported" >> failure)
        (\ceo -> do
          let govState = conwayEraOnwardsConstraints ceo $ newEpochState ^. newEpochStateGovStateL
              proposals = govState ^. L.cgsProposalsL . L.pPropsL . to toList
          if null proposals
            then pure Nothing
            else do
              let lastProposal = last proposals
                  gaDRepVotes = lastProposal ^. L.gasDRepVotesL . to toList
                  gaSpoVotes = lastProposal ^. L.gasStakePoolVotesL . to toList
              if null gaDRepVotes && null gaSpoVotes
              then pure Nothing
              else pure $ Just ()
        )
        actualEra

