{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Components.Query
  ( -- * Epoch state view (re-exported from "Testnet.Components.EpochStateView")
    EpochStateView
  , getEpochStateView
  , getEpochState
  , getSlotNumber
  , getBlockNumber
  , getEpochStateDetails
  , getCurrentEpochNo
  , TestnetWaitPeriod (..)
  , waitForEpochs
  , waitForBlocks
  , retryUntilJustM
  , retryUntilM

  , getMinDRepDeposit
  , getMinGovActionDeposit
  , getGovState
  , getTreasuryValue

  , waitUntilEpoch

  , findAllUtxos
  , findUtxosWithAddress
  , findLargestUtxoWithAddress
  , findLargestMultiAssetUtxoWithAddress
  , findLargestUtxoForPaymentKey

  , checkDRepsNumber
  , checkDRepState
  , assertNewEpochState
  , getProtocolParams
  , getGovActionLifetime
  , getKeyDeposit
  , getAccountsStates
  , getTxIx
  ) where

import           Cardano.Api as Api hiding (txId)
import           Cardano.Api.Ledger (Credential, DRepState, EpochInterval (..), KeyRole (DRepRole))
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.UTxO as Utxo

import           Cardano.Ledger.Api (ConwayGovState)
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Api.State.Query as SQ
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.State as L

import           Prelude

import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Type.Equality
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro (Lens', to, (^.))

import           Testnet.Components.EpochStateView
import           Testnet.Property.Assert
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H

-- | Block and wait for the desired epoch.
waitUntilEpoch
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ Desired epoch
  -> m EpochNo -- ^ The epoch number reached
waitUntilEpoch nodeConfigFile socketPath desiredEpoch = withFrozenCallStack $ do
  result <- H.evalIO . runExceptT $
    foldEpochState
      nodeConfigFile socketPath QuickValidation desiredEpoch () (\_ _ _ -> pure ConditionNotMet)
  case result of
    Left (FoldBlocksApplyBlockError (TerminationEpochReached epochNo)) ->
      pure epochNo
    Left err -> do
      H.note_ $ "waitUntilEpoch: could not reach termination epoch, " <> docToString (prettyError err)
      H.failure
    Right res -> do
      H.note_ $ "waitUntilEpoch: could not reach termination epoch - no error returned "
        <> "- invalid foldEpochState behaviour, result: " <> show res
      H.failure

-- | Retrieve all UTxOs map from the epoch state view.
findAllUtxos
  :: forall era m. HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> m (UTxO era)
findAllUtxos epochStateView sbe = withFrozenCallStack $ do
  AnyNewEpochState sbe' _ tbs <- getEpochState epochStateView
  Refl <- H.leftFail $ assertErasEqual sbe sbe'
  pure . UTxO $ getLedgerTablesUTxOValues sbe' tbs

-- | Retrieve utxos from the epoch state view for an address.
findUtxosWithAddress
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> Text -- ^ Address
  -> m (UTxO era)
findUtxosWithAddress epochStateView sbe address = withFrozenCallStack $ do
  utxos <- findAllUtxos epochStateView sbe
  H.note_ $ "Finding UTxOs for " <> T.unpack address
  let cEra = toCardanoEra sbe
  -- ledger address
  address' <- H.leftFail $
    anyAddressInEra cEra =<<
      maybeToEither ("Could not deserialize address: " <> T.unpack address)
        (deserialiseAddress AsAddressAny address)

  let utxos' = Utxo.filter (\(TxOut txAddr _ _ _)  -> txAddr == address') utxos
  H.note_ $ unlines (map show $ toList utxos')
  pure utxos'
  where
    maybeToEither e = maybe (Left e) Right

-- | Retrieve a one largest utxo
findLargestUtxoWithAddress
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> Text -- ^ Address
  -> m (Maybe (TxIn, TxOut CtxUTxO era))
findLargestUtxoWithAddress epochStateView sbe address = withFrozenCallStack $ do
  utxos <- toList <$> findUtxosWithAddress epochStateView sbe address
  pure
    . listToMaybe
    $ sortOn (\(_, TxOut _ txOutValue _ _) -> Down $ txOutValueToLovelace txOutValue) utxos

-- | Retrieve the largest utxo with a multi-asset
findLargestMultiAssetUtxoWithAddress
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> Text -- ^ Address
  -> m (Maybe (TxIn, TxOut CtxUTxO era))
findLargestMultiAssetUtxoWithAddress epochStateView sbe address = withFrozenCallStack $ do
  utxos <- toList <$> findUtxosWithAddress epochStateView sbe address
  let sortedUTxOs = sortOn (\(_, TxOut _ txOutValue _ _) -> Down $ txOutValueToLovelace txOutValue) utxos
      utxosWithMas = filter (\(_,TxOut _ txOutValue _ _) -> isMultiAssetPresent txOutValue) sortedUTxOs
  pure $ listToMaybe utxosWithMas

isMultiAssetPresent :: TxOutValue era -> Bool
isMultiAssetPresent v =
  Map.size (valueToPolicyAssets $ txOutValueToValue v) > 0


-- | Retrieve a largest UTxO for a payment key info - a convenience wrapper for
-- 'findLargestUtxoWithAddress'.
findLargestUtxoForPaymentKey
  :: MonadTest m
  => MonadAssertion m
  => MonadIO m
  => HasCallStack
  => EpochStateView
  -> ShelleyBasedEra era
  -> PaymentKeyInfo
  -> m TxIn
findLargestUtxoForPaymentKey epochStateView sbe address =
  withFrozenCallStack $ do
    utxo <- fmap fst
      . H.nothingFailM
      $ findLargestUtxoWithAddress epochStateView sbe (paymentKeyInfoAddr address)
    H.note_ $ "Largest UTxO for " <> T.unpack (paymentKeyInfoAddr address) <> ": " <> show utxo
    pure utxo


-- | @checkDRepsNumber config socket execConfig n@
-- wait for the number of DReps being @n@ for two epochs. If
-- this number is not attained before two epochs, the test is failed.
checkDRepsNumber
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> Int
  -> m ()
checkDRepsNumber epochStateView sbe expectedDRepsNumber = withFrozenCallStack $
  checkDRepState epochStateView sbe $ \dreps ->
    if length dreps == expectedDRepsNumber
       then Just ()
       else Nothing

-- | @checkDRepState sbe configurationFile socketPath execConfig f@
-- This functions helps check properties about the DRep state.
-- It waits up to two epochs for the result of applying @f@ to the DRepState
-- to become 'Just'. If @f@ keeps returning 'Nothing' the test fails.
-- If @f@ returns 'Just', the contents of the 'Just' are returned.
checkDRepState
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> (Map (Credential DRepRole)
          DRepState
      -> Maybe a) -- ^ A function that checks whether the DRep state is correct or up to date
                  -- and potentially inspects it.
  -> m a
checkDRepState epochStateView sbe f = withFrozenCallStack $
  retryUntilRightM epochStateView (WaitForEpochs $ EpochInterval 2) action >>= \case
    Right a -> pure a
    Left () -> do
      H.note_ "checkDRepState: condition not met within 2 epochs. This is likely a test error."
      H.failure
  where
    action = do
      AnyNewEpochState actualEra newEpochState _ <- getEpochState epochStateView
      Refl <- H.leftFail $ assertErasEqual sbe actualEra
      pure . maybe (Left ()) Right . f $ shelleyBasedEraConstraints sbe
        $ SQ.queryDRepState newEpochState Set.empty

-- | Obtain governance state from node (CLI query)
getGovState
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ConwayEraOnwards era
  -> m (L.ConwayGovState (ShelleyLedgerEra era)) -- ^ The governance state
getGovState epochStateView ceo = withFrozenCallStack $ do
  AnyNewEpochState sbe' newEpochState _ <- getEpochState epochStateView
  let sbe = convert ceo
  Refl <- H.leftFail $ assertErasEqual sbe sbe'
  pure $ conwayEraOnwardsConstraints ceo $ newEpochState ^. L.newEpochStateGovStateL

-- | Obtain the current value of the treasury from the node
getTreasuryValue
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> m L.Coin -- ^ The current value of the treasury
getTreasuryValue epochStateView = withFrozenCallStack $ do
  AnyNewEpochState _ newEpochState _ <- getEpochState epochStateView
  pure $ newEpochState ^. L.nesEpochStateL . L.treasuryL

-- | Obtain minimum deposit amount for governance action from node
getMinGovActionDeposit
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ConwayEraOnwards era
  -> m Integer -- ^ The minimum deposit
getMinGovActionDeposit epochStateView ceo = withFrozenCallStack $ do
  govState <- getGovState epochStateView ceo
  pure $ conwayEraOnwardsConstraints ceo $ govState ^. L.cgsCurPParamsL . L.ppGovActionDepositL  . to L.unCoin

-- | Obtain minimum deposit amount for DRep registration from node
getMinDRepDeposit
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ConwayEraOnwards era
  -> m Integer -- ^ The minimum deposit
getMinDRepDeposit epochStateView ceo = withFrozenCallStack $ do
  govState <- getGovState epochStateView ceo
  pure $ conwayEraOnwardsConstraints ceo $ govState ^. L.cgsCurPParamsL . L.ppDRepDepositL . to L.unCoin

-- | Assert that the value pointed by the @lens@ in the epoch state is the same as the @expected@ value
-- or it becomes the same within the @maxWait@ epochs. If the value is not reached within the time frame,
-- the test fails.
assertNewEpochState
  :: forall m era value. HasCallStack
  => Show value
  => Eq value
  => MonadAssertion m
  => MonadTest m
  => MonadIO m
  => EpochStateView -- ^ Current epoch state view. It can be obtained using the 'getEpochStateView' function.
  -> ShelleyBasedEra era -- ^ The ShelleyBasedEra witness for current era.
  -> EpochInterval -- ^ The maximum wait time in epochs.
  -> Lens' (L.NewEpochState (ShelleyLedgerEra era)) value
  -- ^ The lens to access the specific value in the epoch state.
  -> value -- ^ The expected value to check in the epoch state.
  -> m ()
assertNewEpochState epochStateView sbe maxWait lens expected = withFrozenCallStack $
  retryUntilRightM epochStateView (WaitForEpochs maxWait) checkEpochState >>= \case
    Right () -> pure ()
    Left actual -> do
      H.note_ $ unlines
        [ "assertNewEpochState: expected value not reached within " <> show maxWait
        , "Expected: " <> show expected
        , "Actual:   " <> show actual
        ]
      H.failure
  where
    checkEpochState = withFrozenCallStack $ do
      val <- getFromEpochStateForEra
      pure $ if val == expected then Right () else Left val

    getFromEpochStateForEra = withFrozenCallStack $ do
      (AnyNewEpochState actualEra newEpochState _, _, _) <- getEpochStateDetails epochStateView
      Refl <- H.leftFail $ assertErasEqual sbe actualEra
      pure $ newEpochState ^. lens

-- | Return current protocol parameters from the governance state
getProtocolParams :: (H.MonadAssertion m, MonadTest m, MonadIO m)
  => EpochStateView
  -> ConwayEraOnwards era
  -> m (LedgerProtocolParameters era)
getProtocolParams epochStateView ceo = conwayEraOnwardsConstraints ceo $ do
   govState :: ConwayGovState era <- getGovState epochStateView ceo
   pure . LedgerProtocolParameters $ govState ^. L.cgsCurPParamsL


-- | Obtains the @govActionLifetime@ from the protocol parameters.
-- The @govActionLifetime@ or governance action maximum lifetime in epochs is
-- the number of epochs such that a governance action submitted during an epoch @e@
-- expires if it is still not ratified as of the end of epoch: @e + govActionLifetime + 1@.
getGovActionLifetime :: (H.MonadAssertion m, MonadTest m, MonadIO m)
  => EpochStateView
  -> ConwayEraOnwards era
  -> m EpochInterval
getGovActionLifetime epochStateView ceo = conwayEraOnwardsConstraints ceo $ do
   govState :: ConwayGovState era <- getGovState epochStateView ceo
   return $ govState ^. L.cgsCurPParamsL
                      . L.ppGovActionLifetimeL

-- | Obtains the key registration deposit from the protocol parameters.
getKeyDeposit :: (H.MonadAssertion m, MonadTest m, MonadIO m)
  => EpochStateView
  -> ConwayEraOnwards era
  -> m L.Coin
getKeyDeposit epochStateView ceo = conwayEraOnwardsConstraints ceo $ do
   govState :: ConwayGovState era <- getGovState epochStateView ceo
   return $ govState ^. L.cgsCurPParamsL
                      . L.ppKeyDepositL


-- | Returns staking accounts state
getAccountsStates :: (H.MonadAssertion m, MonadTest m, MonadIO m)
  => EpochStateView
  -> ShelleyBasedEra era
  -> m (Map (L.Credential L.Staking) (L.AccountState (ShelleyLedgerEra era)))
getAccountsStates epochStateView sbe' = shelleyBasedEraConstraints sbe' $ do
  AnyNewEpochState sbe newEpochState _ <- getEpochState epochStateView
  Refl <- H.nothingFail $ testEquality sbe sbe'
  pure $ newEpochState
          ^. L.nesEsL
           . L.esLStateL
           . L.lsCertStateL
           . L.certDStateL
           . L.accountsL
           . L.accountsMapL

-- | Returns the transaction index of a transaction with a given amount and ID.
getTxIx :: forall m era. HasCallStack
        => MonadTest m
        => ShelleyBasedEra era
        -> TxId
        -> L.Coin
        -> (AnyNewEpochState, SlotNo, BlockNo)
        -> m (Maybe TxIx)
getTxIx sbe txId amount (AnyNewEpochState sbe' _ tbs, _, _) = do
  Refl <- H.leftFail $ assertErasEqual sbe sbe'
  shelleyBasedEraConstraints sbe' $ do
    return $ Map.foldlWithKey (\acc (TxIn thisTxId thisTxIx) (TxOut _ txOutValue _ _) ->
      case acc of
        Nothing | thisTxId == txId &&
                  txOutValueToLovelace txOutValue == amount -> Just thisTxIx
                | otherwise -> Nothing
        x -> x) Nothing $ getLedgerTablesUTxOValues sbe' tbs
