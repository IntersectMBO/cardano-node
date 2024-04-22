{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Components.Query
  ( EpochStateView
  , checkDRepsNumber
  , checkDRepState
  , getEpochState
  , getMinDRepDeposit
  , getGovState
  , queryTip
  , waitUntilEpoch
  , waitForEpochs
  , getEpochStateView
  , findAllUtxos
  , findUtxosWithAddress
  , findLargestUtxoWithAddress
  , findLargestUtxoForPaymentKey
  , startLedgerNewEpochStateLogging
  , getCurrentEpochNo
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (Credential, DRepState, KeyRole (DRepRole), StandardCrypto)
import           Cardano.Api.Shelley (ShelleyLedgerEra, fromShelleyTxIn, fromShelleyTxOut)

import           Cardano.CLI.Types.Output
import           Cardano.Ledger.BaseTypes (EpochInterval, addEpochInterval)
import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.UTxO as L

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict (put)
import           Data.Aeson as A
import           Data.Aeson.Lens (_Integral, key)
import           Data.Bifunctor (bimap)
import           Data.IORef
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
import           Data.Ord (Down (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Type.Equality
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro ((^.), (^?))

import qualified Testnet.Process.Cli as H
import           Testnet.Property.Assert
import           Testnet.Property.Utils (runInBackground)
import           Testnet.Runtime
import           Testnet.Start.Types (eraToString)

import qualified Hedgehog as H
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H
import           Hedgehog.Extras.Test.Process (ExecConfig)
import           Hedgehog.Internal.Property (MonadTest)

-- | Block and wait for the desired epoch.
waitUntilEpoch
  :: (MonadIO m, MonadTest m, HasCallStack)
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

-- | Wait for the number of epochs
waitForEpochs
  :: MonadTest m
  => MonadCatch m
  => MonadIO m
  => ExecConfig
  -> NodeConfigFile In
  -> SocketPath
  -> EpochInterval  -- ^ Number of epochs to wait
  -> m EpochNo -- ^ The epoch number reached
waitForEpochs execConfig nodeConfigFile socketPath interval = withFrozenCallStack $ do
  currentEpoch <- H.nothingFailM $ mEpoch <$> queryTip execConfig
  waitUntilEpoch nodeConfigFile socketPath $ addEpochInterval currentEpoch interval

-- | Query the tip of the blockchain
queryTip
  :: (MonadCatch m, MonadIO m, MonadTest m, HasCallStack)
  => ExecConfig
  -> m QueryTipLocalStateOutput
queryTip execConfig = withFrozenCallStack $
  H.execCliStdoutToJson execConfig [ "query", "tip" ]

-- | A read-only mutable pointer to an epoch state, updated automatically
newtype EpochStateView = EpochStateView (IORef (Maybe AnyNewEpochState))

-- | Get epoch state from the view. If the state isn't available, retry waiting up to 15 seconds. Fails when
-- the state is not available after 15 seconds.
getEpochState :: MonadTest m
              => MonadAssertion m
              => MonadIO m
              => EpochStateView
              -> m AnyNewEpochState
getEpochState (EpochStateView esv) =
  withFrozenCallStack $
    H.byDurationM 0.5 15 "EpochStateView has not been initialized within 15 seconds" $
      H.evalIO (readIORef esv) >>= maybe H.failure pure


-- | Create a background thread listening for new epoch states. New epoch states are available to access
-- through 'EpochStateView', using query functions.
getEpochStateView
  :: HasCallStack
  => MonadResource m
  => MonadTest m
  => MonadCatch m
  => NodeConfigFile In -- ^ node Yaml configuration file path
  -> SocketPath -- ^ node socket path
  -> m EpochStateView
getEpochStateView nodeConfigFile socketPath = withFrozenCallStack $ do
  epochStateView <- H.evalIO $ newIORef Nothing
  runInBackground . runExceptT . foldEpochState nodeConfigFile socketPath QuickValidation (EpochNo maxBound) Nothing
    $ \epochState _slotNb _blockNb -> do
        liftIO $ writeIORef epochStateView (Just epochState)
        pure ConditionNotMet
  pure . EpochStateView $ epochStateView

-- | Retrieve all UTxOs map from the epoch state view.
findAllUtxos
  :: forall era m. HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> m (Map TxIn (TxOut CtxUTxO era))
findAllUtxos epochStateView sbe = withFrozenCallStack $ do
  AnyNewEpochState sbe' newEpochState <- getEpochState epochStateView
  Refl <- H.leftFail $ assertErasEqual sbe sbe'
  pure $ fromLedgerUTxO $ newEpochState ^. L.nesEsL . L.esLStateL . L.lsUTxOStateL . L.utxosUtxoL
  where
    fromLedgerUTxO
      :: ()
      => L.UTxO (ShelleyLedgerEra era)
      -> Map TxIn (TxOut CtxUTxO era)
    fromLedgerUTxO (L.UTxO utxo) =
      shelleyBasedEraConstraints sbe
        $ Map.fromList
        . map (bimap fromShelleyTxIn (fromShelleyTxOut sbe))
        . Map.toList
        $ utxo

-- | Retrieve utxos from the epoch state view for an address.
findUtxosWithAddress
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ShelleyBasedEra era
  -> Text -- ^ Address
  -> m (Map TxIn (TxOut CtxUTxO era))
findUtxosWithAddress epochStateView sbe address = withFrozenCallStack $ do
  utxos <- findAllUtxos epochStateView sbe
  H.note_ $ "Finding UTxOs for " <> T.unpack address
  let cEra = toCardanoEra sbe
  -- ledger address
  address' <- H.leftFail $
    anyAddressInEra cEra =<<
      maybeToEither ("Could not deserialize address: " <> T.unpack address)
        (deserialiseAddress AsAddressAny address)

  let utxos' = M.filter (\(TxOut txAddr _ _ _)  -> txAddr == address') utxos
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
  utxos <- M.assocs <$> findUtxosWithAddress epochStateView sbe address
  pure
    . listToMaybe
    $ sortOn (\(_, TxOut _ txOutValue _ _) -> Down $ txOutValueToLovelace txOutValue) utxos

-- | Retrieve a largest UTxO for a payment key info - a convenience wrapper for
-- 'findLargestUtxoForPaymentKey'.
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
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> NodeConfigFile 'In
  -> SocketPath
  -> H.ExecConfig
  -> Int
  -> m ()
checkDRepsNumber sbe configurationFile socketPath execConfig expectedDRepsNb = withFrozenCallStack $
  checkDRepState sbe configurationFile socketPath execConfig
    (\m -> if length m == expectedDRepsNb then Just () else Nothing)

-- | @checkDRepState sbe configurationFile socketPath execConfig f@
-- This functions helps check properties about the DRep state.
-- It waits up to two epochs for the result of applying @f@ to the DRepState
-- to become 'Just'. If @f@ keeps returning 'Nothing' the test fails.
-- If @f@ returns 'Just', the contents of the 'Just' are returned.
checkDRepState
  :: HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> NodeConfigFile In
  -> SocketPath
  -> H.ExecConfig
  -> (Map (Credential 'DRepRole StandardCrypto)
          (DRepState StandardCrypto)
      -> Maybe a) -- ^ A function that checks whether the DRep state is correct or up to date
                  -- and potentially inspects it.
  -> m a
checkDRepState sbe configurationFile socketPath execConfig f = withFrozenCallStack $ do
  QueryTipLocalStateOutput{mEpoch} <- queryTip execConfig
  currentEpoch <- H.evalMaybe mEpoch
  let terminationEpoch = succ . succ $ currentEpoch
  result <- H.evalIO . runExceptT $ foldEpochState configurationFile socketPath QuickValidation terminationEpoch Nothing
      $ \(AnyNewEpochState actualEra newEpochState) _slotNb _blockNb -> do
        case testEquality sbe actualEra of
          Just Refl -> do
            let dreps = shelleyBasedEraConstraints sbe newEpochState
                          ^. L.nesEsL
                           . L.esLStateL
                           . L.lsCertStateL
                           . L.certVStateL
                           . L.vsDRepsL
            case f dreps of
              Nothing -> pure ConditionNotMet
              Just a -> do put $ Just a
                           pure ConditionMet
          Nothing -> do
            error $ "Eras mismatch! expected: " <> show sbe <> ", actual: " <> show actualEra
  case result of
    Left (FoldBlocksApplyBlockError (TerminationEpochReached epochNo)) -> do
      H.note_ $ unlines
                  [ "checkDRepState: condition not met before termination epoch: " <> show epochNo
                  , "This is likely an error of this test." ]
      H.failure
    Left err -> do
      H.note_ $ unlines
                  [ "checkDRepState: could not reach termination epoch: " <> docToString (prettyError err)
                  , "This is probably an error unrelated to this test." ]
      H.failure
    Right (_, Nothing) -> do
      H.note_ $ unlines
                  [ "checkDRepState: foldEpochState returned Nothing: "
                  , "This is probably an error related to foldEpochState." ]
      H.failure
    Right (ConditionNotMet, Just _) -> do
      H.note_ $ unlines
                  [ "checkDRepState: foldEpochState returned Just and ConditionNotMet: "
                  , "This is probably an error related to foldEpochState." ]
      H.failure
    Right (ConditionMet, Just val) ->
      return val

-- | Obtain governance state from node (CLI query)
getGovState
  :: HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => H.ExecConfig
  -> ConwayEraOnwards era
  -> m A.Value -- ^ The governance state
getGovState execConfig ceo = withFrozenCallStack $ do
  let eraName = eraToString $ toCardanoEra ceo
  H.execCliStdoutToJson execConfig
      [ eraName, "query", "gov-state" , "--volatile-tip" ]


-- | Obtain minimum deposit amount for DRep registration from node
getMinDRepDeposit
  :: HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => H.ExecConfig
  -> ConwayEraOnwards era
  -> m Integer
getMinDRepDeposit execConfig ceo = withFrozenCallStack $ do
  govState <- getGovState execConfig ceo
  let mMinDRepDeposit :: Maybe Integer
      mMinDRepDeposit = govState ^? key "currentPParams"
                                  . key "dRepDeposit"
                                  . _Integral
  H.evalMaybe mMinDRepDeposit

-- | Obtain current epoch number using 'getEpochState'
getCurrentEpochNo :: (MonadTest m, MonadAssertion m, MonadIO m)
  => EpochStateView
  -> ShelleyBasedEra ConwayEra
  -> m EpochNo
getCurrentEpochNo epochStateView sbe = do
  AnyNewEpochState actualEra newEpochState <- getEpochState epochStateView
  case testEquality sbe actualEra of
    Just Refl -> return $ shelleyBasedEraConstraints sbe newEpochState
                            ^. L.nesELL
    Nothing ->
      error $ "Eras mismatch! expected: " <> show sbe <> ", actual: " <> show actualEra
