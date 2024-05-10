{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , waitUntilEpoch
  , waitForEpochs
  , waitForBlocks
  , findAllUtxos
  , findUtxosWithAddress
  , findLargestUtxoWithAddress
  , findLargestUtxoForPaymentKey
  , checkDRepsNumber
  , checkDRepState
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (Credential, DRepState, KeyRole (DRepRole), StandardCrypto)
import           Cardano.Api.Shelley (ShelleyLedgerEra, fromShelleyTxIn, fromShelleyTxOut)

import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.BaseTypes (EpochInterval, addEpochInterval)
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import qualified Cardano.Ledger.UTxO as L

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict (put)
import           Data.Bifunctor (bimap)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
import           Data.Ord (Down (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Type.Equality
import           Data.Word (Word64)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro (to, (^.))

import           Testnet.Property.Assert
import           Testnet.Property.Util (runInBackground)
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
waitForEpochs epochStateView@EpochStateView{nodeConfigPath, socketPath} interval = withFrozenCallStack $ do
  currentEpoch <- getCurrentEpochNo epochStateView
  waitUntilEpoch nodeConfigPath socketPath $ addEpochInterval currentEpoch interval


-- | Wait for the requested number of blocks
waitForBlocks
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> Word64 -- ^ Number of blocks to wait
  -> m BlockNo -- ^ The block number reached
waitForBlocks epochStateView numberOfBlocks = withFrozenCallStack $ do
  BlockNo startingBlockNumber <- getBlockNumber epochStateView
  fmap BlockNo $
    watchEpochStateUpdate epochStateView $ \(_, _, BlockNo blockNumber) ->
      pure $
        if blockNumber >= startingBlockNumber + numberOfBlocks
        then Just blockNumber
        else Nothing

-- | A read-only mutable pointer to an epoch state, updated automatically
data EpochStateView = EpochStateView
  { nodeConfigPath :: !(NodeConfigFile In)
  -- ^ node configuration file path
  , socketPath :: !SocketPath
  -- ^ node socket path, to which foldEpochState is connected to
  , wakeLock :: !(TChan ())
  -- ^ multi-wakeup lock for notifying about epoch state updates. All listeners need to 'dupTChan' and then
  -- 'readTChan' to be notified.
  , epochStateView :: !(TMVar (AnyNewEpochState, SlotNo, BlockNo))
  -- ^ Updated automatically current NewEpochState. Use 'getEpochState' to access the value.
  }

-- | Notify epoch state view listeners about the update of the epoch state
notifyEpochStateViewListeners
  :: MonadIO m
  => EpochStateView
  -> m ()
notifyEpochStateViewListeners EpochStateView{wakeLock} =
  void . liftIO . atomically $ do
    -- Drain the channel first, to not keep anything not used in memory. We only need to store one element in
    -- the channel. This is a safeguard against elements piling up in the channel If there are no listeners.
    _ <- runMaybeT . forever . MaybeT $ tryReadTChan wakeLock
    -- notify all listeners on duplicated channels
    writeTChan wakeLock ()

-- | Watch epoch state view for an update. On every update, the callback function gets executed.
watchEpochStateUpdate
  :: HasCallStack
  => MonadTest m
  => MonadIO m
  => EpochStateView
  -> ((AnyNewEpochState, SlotNo, BlockNo) -> m (Maybe a))
  -- ^ callback function invoked repeatedly. Stops the processing when 'Just a' gets returned
  -> m a -- ^ the result from the callback function
watchEpochStateUpdate EpochStateView{wakeLock, epochStateView} f = withFrozenCallStack $
  -- dupTChan for multi-wakeup
  go =<< (liftIO . atomically $ dupTChan wakeLock)
    where
      go wakeupChannel = do
        newEpochState <- liftIO . atomically $ do
          _ <- readTChan wakeupChannel -- block and wait for update
          readTMVar epochStateView
        f newEpochState >>= \case
          Nothing -> go wakeupChannel
          Just a -> pure a

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
  withFrozenCallStack $ getEpochStateDetails epochStateView $ \(nes, _, _) -> nes

getBlockNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m BlockNo -- ^ The number of last produced block
getBlockNumber epochStateView =
  withFrozenCallStack $ getEpochStateDetails epochStateView $ \(_, _, blockNumber) -> blockNumber

getSlotNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m SlotNo -- ^ The current slot number
getSlotNumber epochStateView =
  withFrozenCallStack $ getEpochStateDetails epochStateView $ \(_, slotNumber, _) -> slotNumber

-- | Utility function for accessing epoch state in `TVar`
getEpochStateDetails
  :: HasCallStack
  => MonadAssertion m
  => MonadTest m
  => MonadIO m
  => EpochStateView
  -> ((AnyNewEpochState, SlotNo, BlockNo) -> a)
  -> m a
getEpochStateDetails EpochStateView{epochStateView} f =
  withFrozenCallStack $
    H.byDurationM 0.5 15 "EpochStateView has not been initialized within 15 seconds" $
      H.evalIO (atomically $ tryReadTMVar epochStateView) >>= maybe H.failure (pure . f)

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
  epochStateView <- H.evalIO newEmptyTMVarIO
  -- we're not using 'newBroadcastTChan' here, because we don't know if we will have any clients here, so we
  -- have to manually read and write a value to the channel, triggering multi-wakeup on listeners on dup-chans
  wakeLock <- H.evalIO newTChanIO
  let esv = EpochStateView nodeConfigFile socketPath wakeLock epochStateView
  runInBackground . runExceptT . foldEpochState nodeConfigFile socketPath QuickValidation (EpochNo maxBound) Nothing
    $ \epochState slotNumber blockNumber -> do
        liftIO . atomically $ writeTMVar epochStateView (epochState, slotNumber, blockNumber)
        notifyEpochStateViewListeners esv
        pure ConditionNotMet
  pure esv

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
