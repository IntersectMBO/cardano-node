{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  , findCondition
  ) where

import           Cardano.Api (AnyNewEpochState (..), ConwayEra, EpochNo, File (File),
                   FoldBlocksError, LedgerStateCondition (..), MonadIO, ShelleyBasedEra,
                   ValidationMode (FullValidation), foldEpochState, runExceptT,
                   shelleyBasedEraConstraints)
import qualified Cardano.Api as Api
import           Cardano.Api.Ledger (GovActionId (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import           Cardano.Ledger.Shelley.API (TxId (..))
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Control.Monad.State.Strict (MonadState (put), StateT)
import           Data.Data ((:~:) (..))
import qualified Data.Map as Map
import           Data.Type.Equality (TestEquality (..))
import           Data.Word (Word32)
import           GHC.Stack
import           Lens.Micro ((^.))

import           Hedgehog

findCondition
  :: HasCallStack
  => MonadTest m
  => MonadIO m
  => (AnyNewEpochState -> Maybe a)
  -> FilePath
  -> FilePath
  -> EpochNo -- ^ The termination epoch: the condition must be found *before* this epoch
  -> m (Either FoldBlocksError (Maybe a))
findCondition epochStateFoldFunc configurationFile socketPath maxEpochNo = withFrozenCallStack $ evalIO . runExceptT $ do
  result <-
    foldEpochState
      (File configurationFile)
      (File socketPath)
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

maybeExtractGovernanceActionIndex :: ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> Api.TxId
  -> AnyNewEpochState
  -> Maybe Word32
maybeExtractGovernanceActionIndex sbe txid (AnyNewEpochState actualEra newEpochState) =
  case testEquality sbe actualEra of
          Just Refl -> do
            let proposals = shelleyBasedEraConstraints sbe newEpochState
                          ^. L.newEpochStateGovStateL
                           . L.proposalsGovStateL
            Map.foldlWithKey' (compareWithTxId txid) Nothing (L.proposalsActionsMap proposals)
          Nothing -> do
            error $ "Eras mismatch! expected: " <> show sbe <> ", actual: " <> show actualEra
  where
    compareWithTxId (Api.TxId ti1) Nothing (GovActionId (TxId ti2) (L.GovActionIx gai)) _
      | ti1 == L.extractHash ti2 = Just gai
    compareWithTxId _ x _ _ = x
