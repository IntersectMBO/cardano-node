{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  , findCondition
  , watchEpochStateView
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (EpochInterval (..), GovActionId (..))
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

import           Testnet.Components.Query (EpochStateView, getEpochState)

import           Hedgehog
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H

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

-- | Watch the epoch state view until the guard function returns 'Just' or the timeout epoch is reached.
-- Wait for at most @maxWait@ epochs.
-- The function will return the result of the guard function if it is met, otherwise it will return @Nothing@.
watchEpochStateView
  :: forall m a. (HasCallStack, MonadIO m, MonadTest m, MonadAssertion m)
  => EpochStateView -- ^ The info to access the epoch state
  -> (AnyNewEpochState -> m (Maybe a)) -- ^ The guard function (@Just@ if the condition is met, @Nothing@ otherwise)
  -> EpochInterval -- ^ The maximum number of epochs to wait
  -> m (Maybe a)
watchEpochStateView epochStateView f (EpochInterval maxWait) = withFrozenCallStack $ do
  AnyNewEpochState _ newEpochState <- getEpochState epochStateView
  let EpochNo currentEpoch = L.nesEL newEpochState
  go (EpochNo $ currentEpoch + fromIntegral maxWait)
    where
      go :: EpochNo -> m (Maybe a)
      go (EpochNo timeout) = do
        epochState@(AnyNewEpochState _ newEpochState') <- getEpochState epochStateView
        let EpochNo currentEpoch = L.nesEL newEpochState'
        condition <- f epochState
        case condition of
          Just result -> pure (Just result)
          Nothing -> do
            if currentEpoch > timeout
              then pure Nothing
              else do
                H.threadDelay 100_000
                go (EpochNo timeout)

