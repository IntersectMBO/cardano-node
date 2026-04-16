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
  , getEpochStateDetails

  , getMinDRepDeposit
  , getMinGovActionDeposit
  , getGovState
  , getCurrentEpochNo
  , getTreasuryValue

  , TestnetWaitPeriod (..)
  , waitForEpochs
  , waitUntilEpoch
  , waitForBlocks
  , retryUntilJustM
  , retryUntilM

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

import           Control.Applicative ((<|>))
import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import qualified Control.Concurrent.STM as STM
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT, runMaybeT)
import           Control.Monad.Trans.Resource
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import           Data.Type.Equality
import           Data.Word (Word64)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro (Lens', to, (^.))

import           Testnet.Process.RunIO (liftIOAnnotated)
import           Testnet.Property.Assert
import           Testnet.Runtime
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (MonadAssertion)
import qualified Hedgehog.Extras as H

import           UnliftIO.STM (atomically, readTVarIO, registerDelay)

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
  void . retryUntilRightM epochStateView (WaitForEpochs interval) . pure $ Left ()
  getCurrentEpochNo epochStateView

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
  H.note_ $ "Current block number: " <> show startingBlockNumber <> ". "
    <> "Waiting for " <> show numberOfBlocks <> " blocks"
  void . retryUntilRightM epochStateView (WaitForBlocks numberOfBlocks) . pure $ Left ()
  getBlockNumber epochStateView

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

-- | Core retry loop. Returns early on 'Right'; on 'Left', blocks via STM until
-- the 'EpochStateView' is updated (with a safety fallback timeout) and retries.
-- Gives up and returns the last 'Left' once the 'TestnetWaitPeriod' deadline is
-- exceeded.
retryUntilRightM
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> TestnetWaitPeriod
  -> m (Either e a)
  -> m (Either e a)
retryUntilRightM esv timeout act = withFrozenCallStack $ do
  startingValue <- getCurrentValue
  go $ startingValue + timeoutW64
  where
    go deadline = do
      -- Sample the version before running 'act' so that any update landing during 'act'
      -- makes 'awaitStateUpdateTimeout' return without blocking, rather than waiting for
      -- the next update and adding a block/epoch of latency.
      versionBeforeAct <- readTVarIO $ epochStateVersion esv
      act >>= \case
        r@(Right _) -> pure r
        l@(Left _) -> do
          cv <- getCurrentValue
          if cv > deadline
            then pure l
            else awaitStateUpdateTimeout esv 300 versionBeforeAct *> go deadline

    (getCurrentValue, timeoutW64) = case timeout of
      WaitForEpochs (EpochInterval n) -> (unEpochNo <$> getCurrentEpochNo esv, fromIntegral n)
      WaitForSlots n                  -> (unSlotNo <$> getSlotNumber esv, n)
      WaitForBlocks n                 -> (unBlockNo <$> getBlockNumber esv, n)

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
retryUntilJustM esv timeout act = withFrozenCallStack $
  retryUntilRightM esv timeout (maybe (Left ()) Right <$> act) >>= \case
    Right a -> pure a
    Left () -> do
      H.note_ $ "Action did not result in 'Just' - waited for: " <> show timeout
      H.failure

-- | Like 'retryUntilJustM' but takes a plain action and a predicate instead of
-- an action returning 'Maybe'. On timeout, annotates the last value that failed
-- the predicate. Intermediate attempts produce no annotations.
retryUntilM
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => Show a
  => EpochStateView
  -> TestnetWaitPeriod -- ^ timeout
  -> m a              -- ^ action to retry
  -> (a -> Bool)      -- ^ predicate that must hold
  -> m a
retryUntilM esv timeout act predicate = withFrozenCallStack $
  retryUntilRightM esv timeout ((\r -> if predicate r then Right r else Left r) <$> act) >>= \case
    Right a -> pure a
    Left r -> do
      H.noteShow_ r
      H.note_ $ "Predicate not satisfied after: " <> show timeout
      H.failure

-- | Status of the 'EpochStateView' background thread when epoch state is not yet available
data EpochStateStatus
  = EpochStateNotInitialised
  -- ^ The background thread has not yet received any epoch state from the node
  | EpochStateFoldError !FoldBlocksError
  -- ^ The background thread encountered an error while folding blocks

-- | A read-only mutable pointer to an epoch state, updated automatically
data EpochStateView = EpochStateView
  { epochStateView :: !(TVar (Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)))
  -- ^ Automatically updated current NewEpochState. 'Left' indicates the state is not yet available
  -- (either not initialised or an error occurred). 'Right' contains the latest epoch state.
  -- Use 'getEpochState', 'getBlockNumber', 'getSlotNumber' to access the values.
  , epochStateVersion :: !(TVar Word64)
  -- ^ Monotonically increasing counter, bumped on every state write.
  -- Used by 'awaitStateUpdateTimeout' to block until the next update.
  }

-- | Block until the epoch state version advances past the provided previously sampled
-- version, or until the fallback timeout expires. Returns immediately if the current
-- version already differs, so callers can sample before running an action and avoid
-- missing updates that land during the action. Returns 'Nothing' on timeout.
-- All threads blocked on the same 'EpochStateView' wake up on each update.
awaitStateUpdateTimeout
  :: MonadIO m
  => EpochStateView
  -> DTC.NominalDiffTime -- ^ Fallback timeout
  -> Word64 -- ^ Previously sampled version
  -> m (Maybe (Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)))
awaitStateUpdateTimeout EpochStateView{epochStateVersion, epochStateView} timeout sinceVersion = runMaybeT $ fastResult <|> awaitedResult
  where
    -- Fast path: if the version already differs, read state and version atomically and return
    -- without allocating a 'registerDelay' timer. This avoids accumulating timer-queue entries
    -- when callers sample a stale version and an update has already landed.
    fastResult = mapMaybeT atomically $ do
      v <- lift $ readTVar epochStateVersion
      guard $ v /= sinceVersion
      lift $ readTVar epochStateView

    awaitedResult = MaybeT $ do
      timedOutVar <- registerDelay . ceiling $ timeout * 1_000_000
      atomically $ do
        v <- readTVar epochStateVersion
        timedOut <- readTVar timedOutVar
        case (v /= sinceVersion, timedOut) of
          (True, _) -> Just <$> readTVar epochStateView
          (_, True) -> pure Nothing
          _ -> STM.retry

-- | Get epoch state from the view. If the state isn't available, retry waiting up to 25 seconds. Fails
-- immediately if the background thread encountered an error, or after 25 seconds if not yet initialised.
getEpochState
  :: HasCallStack
  => MonadTest m
  => MonadAssertion m
  => MonadIO m
  => EpochStateView
  -> m AnyNewEpochState
getEpochState epochStateView =
  withFrozenCallStack $ (\(nes, _, _) -> nes) <$> getEpochStateDetails epochStateView

getBlockNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m BlockNo -- ^ The number of last produced block
getBlockNumber epochStateView =
  withFrozenCallStack $ (\(_, _, blockNumber) -> blockNumber) <$> getEpochStateDetails epochStateView

getSlotNumber
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => MonadAssertion m
  => EpochStateView
  -> m SlotNo -- ^ The current slot number
getSlotNumber epochStateView =
  withFrozenCallStack $ (\(_, slotNumber, _) -> slotNumber) <$> getEpochStateDetails epochStateView

-- | Access the current epoch state. Returns immediately if state is already available.
-- Blocks up to 25 seconds waiting for initialisation if the background thread has not yet
-- received any epoch state. Fails immediately if the background thread encountered an error.
getEpochStateDetails
  :: HasCallStack
  => MonadAssertion m
  => MonadTest m
  => MonadIO m
  => EpochStateView
  -> m (AnyNewEpochState, SlotNo, BlockNo)
getEpochStateDetails EpochStateView{epochStateView} = withFrozenCallStack $
  -- Fast path: read the TVar outside STM block so we don't register a pointless
  -- 'initTimeoutSeconds' timer on every call. These getters run inside tight
  -- retry loops, and the unused timer-queue entries would otherwise accumulate.
  readTVarIO epochStateView
    >>= awaitForState
    >>= failEpochStateFoldError
  where
    initTimeoutSeconds :: Int
    initTimeoutSeconds = 25

    awaitForState
      :: MonadIO n
      => Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)
      -> n (Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo))
    awaitForState = \case
      Left EpochStateNotInitialised -> do
        -- register delay only when we're starting to retry
        timedOutVar <- registerDelay $ initTimeoutSeconds * 1_000_000
        atomically $ do
          state' <- readTVar epochStateView
          state' <$ case state' of
              -- retry until timeout
              Left EpochStateNotInitialised -> readTVar timedOutVar >>= guard
              _ -> pure ()
      state -> pure state

    failEpochStateFoldError
      :: (HasCallStack, MonadTest n)
      => Either EpochStateStatus (AnyNewEpochState, SlotNo, BlockNo)
      -> n (AnyNewEpochState, SlotNo, BlockNo)
    failEpochStateFoldError = \case
      Right details -> pure details
      Left (EpochStateFoldError err) -> do
        H.note_ $ "EpochStateView background thread failed: " <> docToString (prettyError err)
        H.failure
      Left EpochStateNotInitialised -> do
        H.note_ $ "EpochStateView has not been initialised within " <> show initTimeoutSeconds <> " seconds"
        H.failure


-- | Create a background thread listening for new epoch states. New epoch states are available to access
-- through 'EpochStateView', using query functions.
-- The background thread captures any 'FoldBlocksError' into the shared state, so that consumers
-- (e.g. 'getEpochStateDetails') can fail immediately with a meaningful error message instead of
-- waiting for the full timeout.
getEpochStateView
  :: HasCallStack
  => MonadResource m
  => MonadTest m
  => NodeConfigFile In -- ^ node Yaml configuration file path
  -> SocketPath -- ^ node socket path
  -> m EpochStateView
getEpochStateView nodeConfigFile socketPath = withFrozenCallStack $ do
  epochStateView <- H.evalIO $ newTVarIO $ Left EpochStateNotInitialised
  epochStateVersion <- H.evalIO $ newTVarIO 0
  void . asyncRegister_ $ do
    result <- runExceptT $ foldEpochState nodeConfigFile socketPath QuickValidation (EpochNo maxBound) ()
      $ \epochState slotNumber blockNumber -> do
          liftIOAnnotated . atomically $ do
            writeTVar epochStateView $ Right (epochState, slotNumber, blockNumber)
            modifyTVar' epochStateVersion (+ 1)
          pure ConditionNotMet
    case result of
      Left err -> atomically $ do
        writeTVar epochStateView $ Left $ EpochStateFoldError err
        modifyTVar' epochStateVersion (+ 1)
      Right _ -> pure ()
  pure $ EpochStateView epochStateView epochStateVersion

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
  AnyNewEpochState _ newEpochState _ <- getEpochState epochStateView
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
