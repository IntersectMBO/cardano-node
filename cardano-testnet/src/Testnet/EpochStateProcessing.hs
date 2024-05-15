{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (GovActionId (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.API as L
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import qualified Data.Map as Map
import           Data.Word (Word32)
import           GHC.Stack
import           Lens.Micro ((^.))



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

