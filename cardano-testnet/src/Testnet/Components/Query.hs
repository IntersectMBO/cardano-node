{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Query
  ( QueryTip
  , EpochStateView
  , checkDRepsNumber
  , getEpochState
  , queryTip
  , waitUntilEpoch
  , getEpochStateView
  , findAllUtxos
  , findUtxosWithAddress
  , findLargestUtxoWithAddress
  , findLargestUtxoForPaymentKey
  , startLedgerNewEpochStateLogging
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (StandardCrypto)
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (ShelleyLedgerEra, fromShelleyTxIn, fromShelleyTxOut)

import           Cardano.CLI.Types.Output
import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.UTxO as L

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict (put)
import           Data.Aeson
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
import           GHC.Stack
import           Lens.Micro ((^.))
import           System.Directory (doesFileExist, removeFile)

import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import           Testnet.Property.Assert
import           Testnet.Property.Utils (runInBackground)
import           Testnet.Runtime

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
  -> m EpochNo
waitUntilEpoch nodeConfigFile socketPath desiredEpoch = withFrozenCallStack $ do
  result <- runExceptT $
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

queryTip
  :: (MonadCatch m, MonadIO m, MonadTest m, HasCallStack)
  => File QueryTip Out
  -- ^ Output file
  -> ExecConfig
  -> m QueryTipLocalStateOutput
queryTip (File fp) execConfig = do
  exists <- H.evalIO $ doesFileExist fp
  when exists $ H.evalIO $ removeFile fp

  void $ H.execCli' execConfig
    [ "query",  "tip"
    , "--out-file", fp
    ]

  tipJSON <- H.leftFailM $ H.readJsonFile fp
  H.noteShowM $ H.jsonErrorFail $ fromJSON @QueryTipLocalStateOutput tipJSON

-- | Type level tag for a file storing query tip
data QueryTip

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
      liftIO (readIORef esv) >>= maybe H.failure pure


-- | Create a background thread listening for new epoch states. New epoch states are available to access
-- through 'EpochStateView', using query functions.
getEpochStateView
  :: forall m. HasCallStack
  => MonadResource m
  => MonadTest m
  => MonadCatch m
  => NodeConfigFile In -- ^ node Yaml configuration file path
  -> SocketPath -- ^ node socket path
  -> m EpochStateView
getEpochStateView nodeConfigFile socketPath = withFrozenCallStack $ do
  epochStateView <- liftIO $ newIORef Nothing
  runInBackground . runExceptT . foldEpochState nodeConfigFile socketPath QuickValidation (EpochNo maxBound) Nothing
    $ \epochState _ _ -> do
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
  :: forall era m. HasCallStack
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
  H.note_ $ show utxos'
  pure utxos'
  where
    maybeToEither e = maybe (Left e) Right

-- | Retrieve a one largest utxo
findLargestUtxoWithAddress
  :: forall era m. HasCallStack
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
  => MonadCatch m
  => MonadIO m
  => HasCallStack
  => EpochStateView
  -> ShelleyBasedEra era
  -> PaymentKeyInfo
  -> m TxIn
findLargestUtxoForPaymentKey epochStateView sbe address =
  withFrozenCallStack $
    fmap fst
    . H.noteShowM
    . H.nothingFailM
    $ findLargestUtxoWithAddress epochStateView sbe (paymentKeyInfoAddr address)


-- | @checkDRepsNumber config socket execConfig n@
-- wait for the number of DReps being @n@ for two epochs. If
-- this number is not attained before two epochs, the test is failed.
checkDRepsNumber ::
  (HasCallStack, MonadIO m, MonadCatch m, MonadTest m)
  => ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> NodeConfigFile 'In
  -> SocketPath
  -> H.ExecConfig
  -> Int
  -> m ()
checkDRepsNumber sbe configurationFile socketPath execConfig expectedDRepsNb = do
  QueryTipLocalStateOutput{mEpoch} <- P.execCliStdoutToJson execConfig [ "query", "tip" ]
  currentEpoch <- H.evalMaybe mEpoch
  let terminationEpoch = succ . succ $ currentEpoch
  void $ H.evalMaybeM $ checkDRepsNumber' sbe configurationFile socketPath terminationEpoch expectedDRepsNb

-- | @checkDRepsNumber' config socket terminationEpoch n@
-- wait until @terminationEpoch@ for the number of DReps being @n@. If
-- this number is not attained before @terminationEpoch@, the test is failed.
-- So if you call this function, you are expecting the number of DReps to already
-- be @n@, or to be @n@ before @terminationEpoch@
checkDRepsNumber' ::
  (HasCallStack, MonadIO m, MonadTest m)
  => ShelleyBasedEra ConwayEra -- ^ The era in which the test runs
  -> NodeConfigFile In
  -> SocketPath
  -> EpochNo -- ^ The termination epoch: the constitution proposal must be found *before* this epoch
  -> Int -- ^ The expected numbers of DReps. If this number is not reached until the termination epoch, this function fails the test.
  -> m (Maybe [L.DRepState StandardCrypto]) -- ^ The DReps when the expected number of DReps was attained.
checkDRepsNumber' sbe nodeConfigFile socketPath maxEpoch expectedDRepsNb = do
  result <- runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation maxEpoch Nothing
      $ \(AnyNewEpochState actualEra newEpochState) _ _ -> do
        case testEquality sbe actualEra of
          Just Refl -> do
            let dreps = Map.elems $ shelleyBasedEraConstraints sbe newEpochState
                      ^. L.nesEsL
                      . L.esLStateL
                      . L.lsCertStateL
                      . L.certVStateL
                      . L.vsDRepsL
            if length dreps == expectedDRepsNb then do
                put $ Just dreps
                pure ConditionMet
            else
                pure ConditionNotMet
          Nothing -> do
            error $ "Eras mismatch! expected: " <> show sbe <> ", actual: " <> show actualEra
  case result of
    Left (FoldBlocksApplyBlockError (TerminationEpochReached epochNo)) -> do
      H.note_ $ unlines
                  [ "waitDRepsNumber: drep number did not become " <> show expectedDRepsNb <> " before termination epoch: " <> show epochNo
                  , "This is likely an error of this test." ]
      H.failure
    Left err -> do
      H.note_ $ unlines
                  [ "waitDRepsNumber: could not reach termination epoch: " <> docToString (prettyError err)
                  , "This is probably an error unrelated to this test." ]
      H.failure
    Right (_, val) ->
      return val
