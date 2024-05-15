{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  , waitForGovActionVotes
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (EpochInterval, GovActionId (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (ShelleyLedgerEra)

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.API as L
import           Cardano.Ledger.Shelley.LedgerState (newEpochStateGovStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Data.Data ((:~:) (..))
import qualified Data.Map as Map
import           Data.Word (Word32)
import           GHC.Exts (IsList (toList), toList)
import           GHC.Stack
import           Lens.Micro (to, (^.))

import           Testnet.Components.Query (EpochStateView, watchEpochStateView)
import           Testnet.Property.Assert (assertErasEqual)

import           Hedgehog (MonadTest)
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H

maybeExtractGovernanceActionIndex
  :: HasCallStack
  => TxId -- ^ transaction id searched for
  -> AnyNewEpochState
  -> Maybe Word32
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
  :: forall m era.
     (MonadAssertion m, MonadTest m, MonadIO m, HasCallStack)
  => EpochStateView -- ^ Current epoch state view. It can be obtained using the 'getEpochStateView' function.
  -> ConwayEraOnwards era -- ^ The ConwayEraOnwards witness for current era.
  -> EpochInterval -- ^ The maximum wait time in epochs.
  -> m ()
waitForGovActionVotes epochStateView ceo maxWait = withFrozenCallStack $ do
  mResult <- watchEpochStateView epochStateView getFromEpochState maxWait
  case mResult of
    Just () -> pure ()
    Nothing -> H.failMessage callStack "waitForGovActionVotes: No votes appeared before timeout."
  where
    getFromEpochState :: HasCallStack
      => AnyNewEpochState -> m (Maybe ())
    getFromEpochState (AnyNewEpochState actualEra newEpochState) = do
      let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      Refl <- H.leftFail $ assertErasEqual sbe actualEra
      let govState :: L.ConwayGovState (ShelleyLedgerEra era) = conwayEraOnwardsConstraints ceo $ newEpochState ^. newEpochStateGovStateL
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
