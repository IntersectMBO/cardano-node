{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Components.Query
  ( EpochStateView
  , getEpochStateView
  , getEpochState
  , getSlotNumber
  , getBlockNumber
  , watchEpochStateUpdate

  , getMinDRepDeposit
  , getMinGovActionDeposit
  , getGovState
  , getCurrentEpochNo

  , TestnetWaitPeriod (..)
  , waitForEpochs
  , waitUntilEpoch
  , waitForBlocks
  , retryUntilJustM

  , findAllUtxos
  , findUtxosWithAddress
  , findLargestUtxoWithAddress
  , findLargestUtxoForPaymentKey

  , checkDRepsNumber
  , checkDRepState
  , assertNewEpochState
  , getGovActionLifetime
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (Credential, DRepState, EpochInterval (..), KeyRole (DRepRole),
                   StandardCrypto)
import           Cardano.Api.Shelley (ShelleyLedgerEra, fromShelleyTxIn, fromShelleyTxOut)

import           Cardano.Ledger.Api (ConwayGovState)
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Governance as L
import           Cardano.Ledger.Conway.PParams (ConwayEraPParams)
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.UTxO as L

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict (put)
import           Data.Bifunctor (bimap)
import           Data.IORef
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord (Down (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Type.Equality
import           Data.Word (Word64)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro (Lens', to, (^.))

import           Testnet.Property.Assert
import           Testnet.Types

import qualified Hedgehog as H
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property (MonadTest)

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

-- | Wait for the number of epochs
waitForEpochs
  :: HasCallStack
  => MonadTest m
  => MonadAssertion m
  => MonadIO m
  => EpochStateView
  -> EpochInterval  -- ^ Number of epochs to wait
  -> m EpochNo -- ^ The epoch number reached
waitForEpochs epochStateView interval = withFrozenCallStack $ do
  void $ watchEpochStateUpdate epochStateView interval $ \_ -> pure Nothing
  getCurrentEpochNo epochStateView

-- | Wait for the requested number of blocks
waitForBlocks
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => MonadCatch m
  => EpochStateView
  -> Word64 -- ^ Number of blocks to wait
  -> m BlockNo -- ^ The block number reached
waitForBlocks epochStateView numberOfBlocks = withFrozenCallStack $ do
  BlockNo startingBlockNumber <- getBlockNumber epochStateView
  H.note_ $ "Current block number: " <> show startingBlockNumber <> ". "
    <> "Waiting for " <> show numberOfBlocks <> " blocks"
  H.noteShowM . H.nothingFailM . fmap (fmap BlockNo) $
    watchEpochStateUpdate epochStateView (EpochInterval maxBound) $ \(_, _, BlockNo blockNumber) ->
      pure $
        if blockNumber >= startingBlockNumber + numberOfBlocks
        then Just blockNumber
        else Nothing

data TestnetWaitPeriod
  = WaitForEpochs EpochInterval
  | WaitForBlocks Word64
  | WaitForSlots Word64
  deriving Eq

instance Show TestnetWaitPeriod where
  show = \case
    WaitForEpochs (EpochInterval n) -> "WaitForEpochs " <> show n
    WaitForBlocks n -> "WaitForBlocks " <> show n
    WaitForSlots n -> "WaitForSlots " <> show n

-- | Retries the action until it returns 'Just' or the timeout is reached
retryUntilJustM
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> TestnetWaitPeriod -- ^ timeout for an operation
  -> m (Maybe a)
  -> m a
retryUntilJustM esv timeout act = withFrozenCallStack $ do
  startingValue <- getCurrentValue
  go startingValue
  where
    go startingValue = withFrozenCallStack $ do
      cv <- getCurrentValue
      when (timeoutW64 + startingValue < cv) $ do
        H.note_ $ "Action did not result in 'Just' - waited for: " <> show timeout
        H.failure
      act >>= \case
        Just a -> pure a
        Nothing -> do
          H.threadDelay 300_000
          go startingValue

    getCurrentValue = withFrozenCallStack $
      case timeout of
        WaitForEpochs _ -> unEpochNo <$> getCurrentEpochNo esv
        WaitForSlots _ -> unSlotNo <$> getSlotNumber esv
        WaitForBlocks _ -> unBlockNo <$> getBlockNumber esv

    timeoutW64 =
      case timeout of
        WaitForEpochs (EpochInterval n) -> fromIntegral n
        WaitForSlots n -> n
        WaitForBlocks n -> n

-- | A read-only mutable pointer to an epoch state, updated automatically
data EpochStateView = EpochStateView
  { nodeConfigPath :: !(NodeConfigFile In)
  -- ^ node configuration file path
  , socketPath :: !SocketPath
  -- ^ node socket path, to which foldEpochState is connected to
  , epochStateView :: !(IORef (Maybe (AnyNewEpochState, SlotNo, BlockNo)))
  -- ^ Automatically updated current NewEpochState. Use 'getEpochState', 'getBlockNumber', 'getSlotNumber'
  -- to access the values.
  }

-- | Get epoch state from the view. If the state isn't available, retry waiting up to 15 seconds. Fails when
-- the state is not available after 15 seconds.
getEpochState
  :: HasCallStack
  => MonadTest m
  => MonadAssertion m
  => MonadIO m
  => EpochStateView
  -> m AnyNewEpochState
getEpochState epochStateView =
  withFrozenCallStack $ getEpochStateDetails epochStateView $ \(nes, _, _) -> pure nes

getBlockNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m BlockNo -- ^ The number of last produced block
getBlockNumber epochStateView =
  withFrozenCallStack $ getEpochStateDetails epochStateView $ \(_, _, blockNumber) -> pure blockNumber

getSlotNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m SlotNo -- ^ The current slot number
getSlotNumber epochStateView =
  withFrozenCallStack $ getEpochStateDetails epochStateView $ \(_, slotNumber, _) -> pure slotNumber

-- | Utility function for accessing epoch state in `IORef`
getEpochStateDetails
  :: HasCallStack
  => MonadAssertion m
  => MonadTest m
  => MonadIO m
  => EpochStateView
  -> ((AnyNewEpochState, SlotNo, BlockNo) -> m a)
  -> m a
getEpochStateDetails EpochStateView{epochStateView} f =
  withFrozenCallStack $
    H.byDurationM 0.5 15 "EpochStateView has not been initialized within 15 seconds" $
      H.evalIO (readIORef epochStateView) >>= maybe H.failure f

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
  H.asyncRegister_ . runExceptT . foldEpochState nodeConfigFile socketPath QuickValidation (EpochNo maxBound) Nothing
    $ \epochState slotNumber blockNumber -> do
        liftIO . writeIORef epochStateView $ Just (epochState, slotNumber, blockNumber)
        pure ConditionNotMet
  pure $ EpochStateView nodeConfigFile socketPath epochStateView

-- | Watch the epoch state view until the guard function returns 'Just' or the timeout epoch is reached.
-- Executes the guard function every 300ms. Waits for at most @maxWait@ epochs.
-- The function will return the result of the guard function if it is met within the number of epochs,
-- otherwise it will return @Nothing@.
watchEpochStateUpdate
  :: forall m a. (HasCallStack, MonadIO m, MonadTest m, MonadAssertion m)
  => EpochStateView -- ^ The info to access the epoch state
  -> EpochInterval -- ^ The maximum number of epochs to wait
  -> ((AnyNewEpochState, SlotNo, BlockNo) -> m (Maybe a)) -- ^ The guard function (@Just@ if the condition is met, @Nothing@ otherwise)
  -> m (Maybe a)
watchEpochStateUpdate epochStateView (EpochInterval maxWait) f  = withFrozenCallStack $ do
  AnyNewEpochState _ newEpochState <- getEpochState epochStateView
  let EpochNo currentEpoch = L.nesEL newEpochState
  go $ currentEpoch + fromIntegral maxWait
    where
      go :: Word64 -> m (Maybe a)
      go timeout = do
        newEpochStateDetails@(AnyNewEpochState _ newEpochState', _, _) <- getEpochStateDetails epochStateView pure
        let EpochNo currentEpoch = L.nesEL newEpochState'
        f newEpochStateDetails >>= \case
          Just result -> pure (Just result)
          Nothing
            | currentEpoch > timeout -> pure Nothing
            | otherwise ->  do
              H.threadDelay 300_000
              go timeout

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
  -> (Map (Credential 'DRepRole StandardCrypto)
          (DRepState StandardCrypto)
      -> Maybe a) -- ^ A function that checks whether the DRep state is correct or up to date
                  -- and potentially inspects it.
  -> m a
checkDRepState epochStateView@EpochStateView{nodeConfigPath, socketPath} sbe f = withFrozenCallStack $ do
  currentEpoch <- getCurrentEpochNo epochStateView
  let terminationEpoch = succ . succ $ currentEpoch
  result <- H.evalIO . runExceptT $ foldEpochState nodeConfigPath socketPath QuickValidation terminationEpoch Nothing
      $ \(AnyNewEpochState actualEra newEpochState) _slotNumber _blockNumber -> do
        Refl <- either error pure $ assertErasEqual sbe actualEra
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
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> ConwayEraOnwards era
  -> m (L.ConwayGovState (ShelleyLedgerEra era)) -- ^ The governance state
getGovState epochStateView ceo = withFrozenCallStack $ do
  AnyNewEpochState sbe' newEpochState <- getEpochState epochStateView
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
  Refl <- H.leftFail $ assertErasEqual sbe sbe'
  pure $ conwayEraOnwardsConstraints ceo $ newEpochState ^. L.newEpochStateGovStateL

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

-- | Return current-ish epoch number.
-- Because we're using Ledger's 'NewEpochState', the returned epoch number won't be reflecting the current
-- epoch number during the transiontion between the epochs. In other cases it will be the true number of the
-- current epoch.
getCurrentEpochNo
  :: HasCallStack
  => MonadAssertion m
  => MonadIO m
  => MonadTest m
  => EpochStateView
  -> m EpochNo
getCurrentEpochNo epochStateView = withFrozenCallStack $ do
  AnyNewEpochState _ newEpochState <- getEpochState epochStateView
  pure $ newEpochState ^. L.nesELL

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
assertNewEpochState epochStateView sbe maxWait lens expected = withFrozenCallStack $ do
  mStateView <- watchEpochStateUpdate epochStateView maxWait (const checkEpochState)
  when (isNothing mStateView) $ do
    val <- getFromEpochStateForEra
    -- there's a tiny tiny chance that the value has changed since 'watchEpochStateUpdate'
    -- so check it again
    if val == expected
    then pure ()
    else H.failMessage callStack $ unlines
           [ "assertNewEpochState: expected value not reached within the time frame."
           , "Expected value: " <> show expected
           , "Actual value: " <> show val
           ]
  where
    checkEpochState
      :: HasCallStack
      => m (Maybe ())
    checkEpochState = withFrozenCallStack $ do
      val <- getFromEpochStateForEra
      pure $ if val == expected then Just () else Nothing

    getFromEpochStateForEra
      :: HasCallStack
      => m value
    getFromEpochStateForEra = withFrozenCallStack $ getEpochStateDetails epochStateView $
      \(AnyNewEpochState actualEra newEpochState, _, _) -> do
        Refl <- H.leftFail $ assertErasEqual sbe actualEra
        pure $ newEpochState ^. lens


-- | Obtains the @govActionLifetime@ from the protocol parameters.
-- The @govActionLifetime@ or governance action maximum lifetime in epochs is
-- the number of epochs such that a governance action submitted during an epoch @e@
-- expires if it is still not ratified as of the end of epoch: @e + govActionLifetime + 1@.
getGovActionLifetime :: (ConwayEraPParams (ShelleyLedgerEra era), H.MonadAssertion m, MonadTest m, MonadIO m)
  => EpochStateView
  -> ConwayEraOnwards era
  -> m EpochInterval
getGovActionLifetime epochStateView ceo = do
   govState :: ConwayGovState era <- getGovState epochStateView ceo
   return $ govState ^. L.cgsCurPParamsL
                      . L.ppGovActionLifetimeL
