{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Cli.Query
  ( hprop_cli_queries
  ) where

import           Cardano.Api
import qualified Cardano.Api.Genesis as Api
import           Cardano.Api.Ledger (Coin (Coin), EpochInterval (EpochInterval), StandardCrypto,
                   extractHash, unboundRational)
import           Cardano.Api.Shelley (StakeCredential (StakeCredentialByKey), StakePoolKey)

import           Cardano.CLI.Types.Key (VerificationKeyOrFile (VerificationKeyFilePath),
                   readVerificationKeyOrFile)
import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput)
import           Cardano.Crypto.Hash (hashToStringAsHex)
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Core (valueTxOutL)
import           Cardano.Ledger.Shelley.LedgerState (esLStateL, lsUTxOStateL, nesEpochStateL,
                   utxosUtxoL)
import qualified Cardano.Ledger.TxIn as L
import qualified Cardano.Ledger.UTxO as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad (forM_)
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (bimap)
import           Data.Data (type (:~:) (Refl))
import qualified Data.Map as Map
import           Data.String (IsString (fromString))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Lens.Micro ((^.))
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))

import           Testnet.Components.Query (checkDRepsNumber, getEpochStateView,
                   watchEpochStateUpdate, EpochStateView)
import qualified Testnet.Defaults as Defaults
import           Testnet.Process.Cli.Transaction (TxOutAddress (ReferenceScriptAddress),
                   retrieveTransactionId, signTx, simpleSpendOutputsOnlyTx, spendOutputsOnlyTx,
                   submitTx)
import           Testnet.Process.Run (execCli', execCliStdoutToJson, mkExecConfig)
import           Testnet.Property.Assert (assertErasEqual)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (eraToString)
import           Testnet.TestQueryCmds (TestQueryCmds (..), forallQueryCommands)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (readJsonFile, MonadAssertion)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H

-- | Test CLI queries
-- Execute me with:
-- @cabal test cardano-testnet-test --test-options '-p "/CliQueries/"'@
-- If you want to recreate golden files, run the comment with
-- RECREATE_GOLDEN_FILES=1 as its prefix
hprop_cli_queries :: Property
hprop_cli_queries = integrationWorkspace "cli-queries" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) }
    <- mkConf tempAbsBasePath'
  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      eraName = eraToString era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        , cardanoActiveSlotsCoeff = 0.5
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , configurationFile
    , wallets=wallet0:wallet1:_
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  let shelleyGeneisFile = work </> Defaults.defaultGenesisFilepath ShelleyEra

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  checkDRepsNumber epochStateView sbe 3

  -- If we don't wait, the leadershi-schedule test will say SPO has no stake
  _ <- waitForEpochs epochStateView (EpochInterval 1)

  forallQueryCommands $ \case

    TestQueryLeadershipScheduleCmd ->
      -- leadership-schedule
      do
        let spoKeys = Defaults.defaultSpoKeys 1
        spoVerificationKey :: VerificationKey StakePoolKey <- readVerificationKeyFromFile AsStakePoolKey work $ verificationKey $ poolNodeKeysCold spoKeys
        H.noteM_ $ execCli' execConfig [ eraName, "query", "leadership-schedule"
                                       , "--genesis", shelleyGeneisFile
                                       , "--stake-pool-verification-key", T.unpack $ serialiseToBech32 spoVerificationKey
                                       , "--vrf-signing-key-file", unFile $ signingKey $ poolNodeKeysVrf spoKeys
                                       , "--current"
                                       ]

    TestQueryProtocolParametersCmd ->
      -- protocol-parameters
      do
        -- to stdout
        protocolParametersOut <- execCli' execConfig [ eraName, "query", "protocol-parameters" ]
        H.diffVsGoldenFile
          protocolParametersOut
          "test/cardano-testnet-test/files/golden/queries/protocolParametersOut.txt"
        -- protocol-parameters to a file
        let protocolParametersOutFile = work </> "protocol-parameters-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "protocol-parameters"
                                       , "--out-file", protocolParametersOutFile ]
        H.diffFileVsGoldenFile
          protocolParametersOutFile
          "test/cardano-testnet-test/files/golden/queries/protocolParametersFileOut.json"

    TestQueryConstitutionHashCmd ->
      -- constitution-hash
      -- Currently disabled (not accessible from the command line)
      pure ()

    TestQueryTipCmd ->
      -- tip
      do
        -- to stdout
        _ :: QueryTipLocalStateOutput <- H.noteShowM $ execCliStdoutToJson execConfig [ eraName, "query", "tip" ]
        -- to a file
        let tipOutFile = work </> "tip-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "tip"
                                       , "--out-file", tipOutFile ]
        _ :: QueryTipLocalStateOutput <- H.readJsonFileOk tipOutFile
        pure ()

    TestQueryStakePoolsCmd ->
      -- stake-pools
      do
        -- to stdout
        stakePoolsOut <- execCli' execConfig [ eraName, "query", "stake-pools" ]
        H.assertWith stakePoolsOut $ \pools ->
          length (lines pools) == 3 -- Because, by default, 3 stake pools are created
        -- Light test of the query's answer, the ids should exist:
        forM_ (lines stakePoolsOut) $ \stakePoolId -> do
          execCli' execConfig [ eraName, "query", "pool-state"
                              , "--stake-pool-id", stakePoolId ]
        -- to a file
        let stakePoolsOutFile = work </> "stake-pools-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "stake-pools" , "--out-file", stakePoolsOutFile]

    TestQueryPoolStateCmd ->
      -- pool-state
      -- Already tested in TestQueryStakePoolsCmd and TestQueryStakeDistributionCmd
      pure ()

    TestQueryStakeDistributionCmd ->
      -- stake-distribution
      do
        -- to stdout
        stakeDistrOut <- execCli' execConfig [ eraName, "query", "stake-distribution" ]
        -- stake addresses with stake
        let stakeAddresses :: [(Text, Text)] =
              map
              ( bimap T.strip T.strip
                . T.breakOn " " -- separate address and stake
                . T.strip
                . fromString )
              . drop 2 -- drop header
              . lines
              $ stakeDistrOut
        H.assertWith stakeAddresses $ \sa ->
          -- Because, by default, 3 stake pools are created
          length sa == 3
        -- Light test of the query's answer, the ids should exist:
        forM_ stakeAddresses $ \(stakePoolId, _) -> do
          execCli' execConfig [ eraName, "query", "pool-state"
                              , "--stake-pool-id", T.unpack stakePoolId ]
        -- to a file
        let stakePoolsOutFile = work </> "stake-distribution-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "stake-distribution"
                                       , "--out-file", stakePoolsOutFile ]

    TestQueryStakeAddressInfoCmd ->
      -- stake-address-info
      do
        let delegatorKeys = Defaults.defaultDelegatorStakeKeyPair 1
        delegatorVKey :: VerificationKey StakeKey <- readVerificationKeyFromFile AsStakeKey work $ verificationKey delegatorKeys
        let stakeAddress :: StakeAddress = verificationStakeKeyToStakeAddress testnetMagic delegatorVKey
        H.noteM_ $ execCli' execConfig [ eraName, "query", "stake-address-info"
                                       , "--address", T.unpack $ serialiseAddress stakeAddress
                                       ]

    TestQueryUTxOCmd ->
      -- utxo
      H.noteM_ $ execCli' execConfig [ eraName, "query", "utxo", "--whole-utxo" ]

    TestQueryLedgerStateCmd ->
      -- ledger-state
      H.noteM_ $ execCli' execConfig [ eraName, "query", "ledger-state" ]

    TestQueryProtocolStateCmd ->
      -- protocol-state
      H.noteM_ $ execCli' execConfig [ eraName, "query", "protocol-state" ]

    TestQueryStakeSnapshotCmd ->
      -- stake-snapshot
      H.noteM_ $ execCli' execConfig [ eraName, "query", "stake-snapshot", "--all-stake-pools" ]

    TestQueryKesPeriodInfoCmd ->
      -- kes-period-info
      -- This is tested in hprop_kes_period_info in Cardano.Testnet.Test.Cli.KesPeriodInfo
      pure ()

    TestQueryTxMempoolCmd ->
      -- tx-mempool
      do
        H.noteM_ $ execCli' execConfig [ eraName, "query", "tx-mempool", "info" ]
        H.noteM_ $ execCli' execConfig [ eraName, "query", "tx-mempool", "next-tx" ]
        -- Now we create a transaction and check if it exists in the mempool
        mempoolWork <- H.createDirectoryIfMissing $ work </> "mempool-test"
        txBody <- simpleSpendOutputsOnlyTx execConfig epochStateView sbe mempoolWork "tx-body" wallet0 wallet1 10_000_000
        signedTx <- signTx execConfig cEra mempoolWork "signed-tx" txBody [SomeKeyPair $ paymentKeyInfoPair wallet0]
        submitTx execConfig cEra signedTx
        txId <- retrieveTransactionId execConfig signedTx
        -- And we check
        H.noteM_ $ execCli' execConfig [ eraName, "query", "tx-mempool", "tx-exists", txId ]

    TestQuerySlotNumberCmd ->
      -- slot-number
      -- This is tested in hprop_querySlotNumber in Cardano.Testnet.Test.Cli.QuerySlotNumber
      pure ()

    TestQueryRefScriptSizeCmd ->
      -- ref-script-size
      do
        -- Set up files and vars
        refScriptSizeWork <- H.createDirectoryIfMissing $ work </> "ref-script-size-test"
        plutusV3Script <- File <$> liftIO (makeAbsolute "test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus")
        let transferAmount = Coin 10_000_000
        -- Submit a transaction to publish the reference script
        txBody <- spendOutputsOnlyTx execConfig epochStateView sbe refScriptSizeWork "tx-body" wallet1
                    [(ReferenceScriptAddress plutusV3Script, transferAmount)]
        signedTx <- signTx execConfig cEra refScriptSizeWork "signed-tx" txBody [SomeKeyPair $ paymentKeyInfoPair wallet1]
        submitTx execConfig cEra signedTx
        -- Wait until transaction is on chain and obtain transaction identifier
        txId <- retrieveTransactionId execConfig signedTx
        txIx <- H.evalMaybeM $ watchEpochStateUpdate epochStateView (EpochInterval 2) (getTxIx sbe txId transferAmount)
        -- Query the reference script size
        let protocolParametersOutFile = refScriptSizeWork </> "ref-script-size-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "ref-script-size"
                                       , "--tx-in", txId ++ "#" ++ show (txIx :: Int)
                                       , "--out-file", protocolParametersOutFile
                                       ]
        H.diffFileVsGoldenFile
          protocolParametersOutFile
          "test/cardano-testnet-test/files/golden/queries/refScriptSizeOut.json"

    TestQueryConstitutionCmd ->
      -- constitution
      do
        output <- execCli' execConfig [ eraName, "query", "constitution" ]
        H.diffVsGoldenFile output "test/cardano-testnet-test/files/golden/queries/queryConstitutionOut.json"

    TestQueryGovStateCmd ->
      -- gov-state
      do
        -- wait for the proposal stage to end
        shelleyGenesisVal <- H.evalEitherM $ readJsonFile shelleyGeneisFile
        newSlot <- waitForFuturePParamsToStabilise epochStateView shelleyGenesisVal
        H.note_ $ "Current slot is: " ++ show newSlot
        -- to stdout
        output <- execCli' execConfig [ eraName, "query", "gov-state" ]
        H.diffVsGoldenFile output "test/cardano-testnet-test/files/golden/queries/govStateOut.json"
        -- to a file
        let govStateOutFile = work </> "gov-state-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "gov-state", "--out-file", govStateOutFile ]
        H.diffFileVsGoldenFile
          govStateOutFile
          "test/cardano-testnet-test/files/golden/queries/govStateOut.json"

    TestQueryDRepStateCmd ->
      -- drep-state
      do
        -- to stdout
        -- TODO: deserialize to a Haskell value when
        -- https://github.com/IntersectMBO/cardano-cli/issues/606 is tackled
        dreps :: Aeson.Value <- H.noteShowM $ execCliStdoutToJson execConfig [ eraName, "query", "drep-state", "--all-dreps" ]
        assertArrayOfSize dreps 3
        -- to a file
        let drepStateOutFile = work </> "drep-state-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "drep-state", "--all-dreps"
                                       , "--out-file", drepStateOutFile ]
        _ :: Aeson.Value <- H.readJsonFileOk drepStateOutFile
        pure ()

    TestQueryDRepStakeDistributionCmd ->
      -- drep-stake-distribution
      H.noteM_ $ execCli' execConfig [ eraName, "query", "drep-stake-distribution", "--all-dreps" ]

    TestQueryCommitteeMembersStateCmd ->
      -- committee-state
      H.noteM_ $ execCli' execConfig [ eraName, "query", "committee-state" ]

    TestQueryTreasuryValueCmd -> do
      -- treasury
      H.noteM_ $ execCli' execConfig [ eraName, "query", "treasury" ]

  where
  -- | Wait for the part of the epoch when futurePParams are known
  waitForFuturePParamsToStabilise
    :: HasCallStack
    => MonadIO m
    => MonadTest m
    => MonadAssertion m
    => MonadCatch m
    => EpochStateView
    -> ShelleyGenesis StandardCrypto
    -> m SlotNo -- ^ The block number reached
  waitForFuturePParamsToStabilise epochStateView shelleyGenesisConf = withFrozenCallStack $
    H.noteShowM . H.nothingFailM $
      watchEpochStateUpdate epochStateView (EpochInterval 2) $ \(_, slotNo, _) -> do
        pure $ if areFuturePParamsStable shelleyGenesisConf slotNo
               then Just slotNo
               else Nothing

  areFuturePParamsStable :: ShelleyGenesis StandardCrypto -> SlotNo -> Bool
  areFuturePParamsStable
    ShelleyGenesis{ Api.sgActiveSlotsCoeff = activeSlotsCoeff
                  , Api.sgEpochLength = L.EpochSize epochLength
                  , Api.sgSecurityParam = securityParam
                  }
    (SlotNo slotNo) =
    let firstSlotOfEpoch = slotNo `div` epochLength * epochLength
        slotsInEpochToWaitOut = ceiling (4 * fromIntegral securityParam / unboundRational activeSlotsCoeff) + 1
        minSlotInThisEpochToWaitTo = firstSlotOfEpoch + slotsInEpochToWaitOut + 1
    in slotNo >= minSlotInThisEpochToWaitTo

  readVerificationKeyFromFile :: (MonadIO m, MonadCatch m, MonadTest m,  HasTextEnvelope (VerificationKey keyrole), SerialiseAsBech32 (VerificationKey keyrole))
    => AsType keyrole
    -> FilePath
    -> File content direction
    -> m (VerificationKey keyrole)
  readVerificationKeyFromFile asKey work =
    H.evalEitherM . liftIO . runExceptT . readVerificationKeyOrFile asKey . VerificationKeyFilePath . File . (work </>) . unFile

  verificationStakeKeyToStakeAddress :: Int -> VerificationKey StakeKey -> StakeAddress
  verificationStakeKeyToStakeAddress testnetMagic delegatorVKey =
    makeStakeAddress (fromNetworkMagic $ NetworkMagic $ fromIntegral testnetMagic) (StakeCredentialByKey $ verificationKeyHash delegatorVKey)

  getTxIx :: forall m era. MonadTest m => ShelleyBasedEra era -> String -> Coin -> (AnyNewEpochState, SlotNo, BlockNo) -> m (Maybe Int)
  getTxIx sbe txId amount (AnyNewEpochState sbe' newEpochState, _, _) = do
    Refl <- H.leftFail $ assertErasEqual sbe sbe'
    shelleyBasedEraConstraints sbe' (do
      return $ Map.foldlWithKey (\acc (L.TxIn (L.TxId thisTxId) (L.TxIx thisTxIx)) txOut ->
        case acc of
          Nothing | hashToStringAsHex (extractHash thisTxId) == txId &&
                    valueToLovelace (fromLedgerValue sbe (txOut ^. valueTxOutL)) == Just amount -> Just $ fromIntegral thisTxIx
                  | otherwise -> Nothing
          x -> x) Nothing $ L.unUTxO $ newEpochState ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosUtxoL)

-- | @assertArrayOfSize v n@ checks that the value is a JSON array of size @n@,
-- otherwise it fails the test.
assertArrayOfSize :: ()
  => HasCallStack
  => MonadTest m
  => Aeson.Value
  -> Int
  -> m ()
assertArrayOfSize v n =
  case v of
    Aeson.Array a -> do
      let actualLength = Vector.length a
      if actualLength == n
      then H.success
      else do
        H.note_ $ "Expected an array of length " <> show n <> ", but got: " <> show actualLength
        H.failure
    Aeson.Object _ -> failWrongType "object"
    Aeson.Bool _   -> failWrongType "bool"
    Aeson.Number _ -> failWrongType "number"
    Aeson.Null     -> failWrongType "null"
    Aeson.String _ -> failWrongType "string"
   where
     failWrongType got = do
       H.note_ $ "Expected a JSON object, but received: " <> got
       H.failure

