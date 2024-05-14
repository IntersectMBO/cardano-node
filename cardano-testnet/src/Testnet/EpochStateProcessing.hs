{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  , findCondition
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (GovActionId (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.API as L
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Control.Monad.State.Strict (MonadState (put), StateT)
import qualified Data.Map as Map
import           Data.Word (Word32)
import           GHC.Stack
import           Lens.Micro ((^.))

import           Hedgehog

findCondition
  :: HasCallStack
  => MonadTest m
  => MonadIO m
  => (AnyNewEpochState -> Maybe a)
  -> NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the condition must be found *before* this epoch
  -> m (Either FoldBlocksError (Maybe a))
findCondition epochStateFoldFunc configurationFile socketPath maxEpochNo = withFrozenCallStack $ evalIO . runExceptT $ do
  result <-
    foldEpochState
      configurationFile
      socketPath
      FullValidation
      maxEpochNo
      Nothing
      (\epochState _ _ -> go epochStateFoldFunc epochState)
  pure $ case result of
    (ConditionMet, Just x) -> Just x
    _                      -> Nothing

  where
    go :: (AnyNewEpochState -> Maybe a) -> AnyNewEpochState -> StateT (Maybe a) IO LedgerStateCondition
    go f epochState = do
      case f epochState of
        Just x -> put (Just x) >> pure ConditionMet
        Nothing -> pure ConditionNotMet

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

