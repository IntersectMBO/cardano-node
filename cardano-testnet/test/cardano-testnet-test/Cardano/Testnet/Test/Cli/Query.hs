{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Testnet.Test.Cli.Query
  ( hprop_cli_queries
  ) where

import           Cardano.Api
import qualified Cardano.Api.Genesis as Api
import           Cardano.Api.Ledger (Coin (Coin), EpochInterval (EpochInterval), StandardCrypto,
                   extractHash, unboundRational)
import qualified Cardano.Api.Ledger as L
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

import           Control.Lens ((^?))
import           Control.Monad (forM_)
import           Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Lens as Aeson
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LBS
import           Data.Data (type (:~:) (Refl))
import           Data.Default.Class
import qualified Data.Map as Map
import           Data.String (IsString (fromString))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as Vector
import           GHC.Exts (IsList (..))
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified GHC.Stack as GHC
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Testnet.Components.Configuration (eraToString)
import           Testnet.Components.Query (EpochStateView, checkDRepsNumber, getEpochStateView,
                   watchEpochStateUpdate)
import qualified Testnet.Defaults as Defaults
import           Testnet.Process.Cli.Transaction (
                   mkSimpleSpendOutputsOnlyTx, retrieveTransactionId, signTx,
                   submitTx)
import           Testnet.Process.Run (execCli', execCliStdoutToJson, mkExecConfig)
import           Testnet.Property.Assert (assertErasEqual)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types (ShelleyTestnetOptions(..))
import           Testnet.TestQueryCmds (TestQueryCmds (..), forallQueryCommands)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import           Hedgehog.Extras (MonadAssertion, readJsonFile)
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
      asbe = AnyShelleyBasedEra sbe
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      eraName = eraToString era
      fastTestnetOptions = def { cardanoNodeEra = asbe }
      shelleyOptions = def
        { shelleyEpochLength = 100
        -- We change slotCoeff because epochLength must be equal to:
        -- securityParam * 10 / slotCoeff
        , shelleyActiveSlotsCoeff = 0.5
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , configurationFile
    , wallets=wallet0:wallet1:_
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

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

  -- If we don't wait, the leadership-schedule test will say SPO has no stake
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

    TestQueryTipCmd ->
      -- tip
      do
        -- to stdout
        tips :: QueryTipLocalStateOutput <- H.noteShowM $ execCliStdoutToJson execConfig [ eraName, "query", "tip" ]
        let changes = fromList (map (, "<redacted>") ["hash", "block", "epoch", "slot", "slotInEpoch", "slotsToEpochEnd"])
            tipsRedacted = redactJsonFields changes (Aeson.toJSON tips)
        H.diffVsGoldenFile
          (TL.unpack . TL.decodeUtf8 $ Aeson.encodePretty tipsRedacted)
          "test/cardano-testnet-test/files/golden/queries/tipOut.json"
         -- to a file
        let tipOutFile = work </> "tip-out.json"
            tipOutRedactedFile = work </> "tip-out-redacted.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "tip"
                                       , "--out-file", tipOutFile]
        _ :: QueryTipLocalStateOutput <- H.readJsonFileOk tipOutFile
        redactJsonFieldsInFile changes tipOutFile tipOutRedactedFile
        H.diffFileVsGoldenFile
          tipOutRedactedFile
          "test/cardano-testnet-test/files/golden/queries/tipOut.json"
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

    TestQuerySPOStakeDistributionCmd ->
      -- spo-stake-distribution
      do
        -- Query all SPOs
        aesonSpoDist :: Aeson.Value <- execCliStdoutToJson execConfig [ eraName, "query", "spo-stake-distribution", "--all-spos" ]
        secondHash <- H.evalMaybe $ T.unpack <$> aesonSpoDist ^? Aeson.nth 1 . Aeson.nth 0 . Aeson._String
        secondAmount <- H.evalMaybe $ aesonSpoDist ^? Aeson.nth 1 . Aeson.nth 1 . Aeson._Number

        -- Query individual SPO using result and ensure result is the same
        secondSpoInfo :: Aeson.Value <- execCliStdoutToJson execConfig [ eraName, "query", "spo-stake-distribution", "--spo-key-hash", secondHash ]
        individualHash <- H.evalMaybe $ T.unpack <$> secondSpoInfo ^? Aeson.nth 0 . Aeson.nth 0 . Aeson._String
        individualAmount <- H.evalMaybe $ secondSpoInfo ^? Aeson.nth 0 . Aeson.nth 1 . Aeson._Number
        secondHash === individualHash
        secondAmount === individualAmount

        -- Query individual SPO using SPOs verification file
        let spoKey = verificationKey . poolNodeKeysCold $ Defaults.defaultSpoKeys 1
        fileQueryResult :: Aeson.Value <- execCliStdoutToJson execConfig [ eraName, "query", "spo-stake-distribution"
                                                                         , "--spo-verification-key-file", unFile spoKey
                                                                         ]
        fileQueryHash <- H.evalMaybe $ T.unpack <$> fileQueryResult ^? Aeson.nth 0 . Aeson.nth 0 . Aeson._String
        fileQueryAmount <- H.evalMaybe $ fileQueryResult ^? Aeson.nth 0 . Aeson.nth 1 . Aeson._Number

        -- Query individual SPO using SPOs bech32 of key and compare to previous result
        delegatorVKey :: VerificationKey StakePoolKey <- readVerificationKeyFromFile AsStakePoolKey work spoKey
        keyQueryResult :: Aeson.Value <- execCliStdoutToJson execConfig [ eraName, "query", "spo-stake-distribution"
                                                                        , "--spo-verification-key", T.unpack $ serialiseToBech32 delegatorVKey
                                                                        ]
        keyQueryHash <- H.evalMaybe $ T.unpack <$> keyQueryResult ^? Aeson.nth 0 . Aeson.nth 0 . Aeson._String
        keyQueryAmount <- H.evalMaybe $ keyQueryResult ^? Aeson.nth 0 . Aeson.nth 1 . Aeson._Number
        fileQueryHash === keyQueryHash
        fileQueryAmount === keyQueryAmount

    TestQueryStakeAddressInfoCmd ->
      -- stake-address-info
      do
        -- to stdout
        let delegatorKeys = Defaults.defaultDelegatorStakeKeyPair 1
        delegatorVKey :: VerificationKey StakeKey <- readVerificationKeyFromFile AsStakeKey work $ verificationKey delegatorKeys
        let stakeAddress :: StakeAddress = verificationStakeKeyToStakeAddress testnetMagic delegatorVKey
        H.noteM_ $ execCli' execConfig [ eraName, "query", "stake-address-info"
                                       , "--address", T.unpack $ serialiseAddress stakeAddress
                                       ]
        -- to a file
        let stakeAddressInfoOutFile = work </> "stake-address-info-out.json"
            redactedStakeAddressInfoOutFile = work </> "stake-address-info-out-redacted.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "stake-address-info"
                                       , "--address", T.unpack $ serialiseAddress stakeAddress
                                       , "--out-file", stakeAddressInfoOutFile
                                       ]

        redactJsonFieldsInFile
          (fromList (map (, "<redacted>") ["address", "stakeDelegation", "voteDelegation"]))
          stakeAddressInfoOutFile
          redactedStakeAddressInfoOutFile
        H.diffFileVsGoldenFile
          redactedStakeAddressInfoOutFile
          "test/cardano-testnet-test/files/golden/queries/stakeAddressInfoOut.json"

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
        txBody <- mkSimpleSpendOutputsOnlyTx execConfig epochStateView sbe mempoolWork "tx-body" wallet0 wallet1 10_000_000
        signedTx <- signTx execConfig cEra mempoolWork "signed-tx" txBody [SomeKeyPair $ paymentKeyInfoPair wallet0]
        submitTx execConfig cEra signedTx
        txId <- retrieveTransactionId execConfig signedTx
        -- And we check
        H.noteM_ $ execCli' execConfig [ eraName, "query", "tx-mempool", "tx-exists", txId ]

    TestQuerySlotNumberCmd ->
      -- slot-number
      -- This is tested in hprop_querySlotNumber in Cardano.Testnet.Test.Cli.QuerySlotNumber
      pure ()

    
    TestQueryRefScriptSizeCmd -> pure () -- TODO: Failing intermittently cardano-node-9.2
    --   -- ref-script-size
    --   do
    --     -- Set up files and vars
    --     refScriptSizeWork <- H.createDirectoryIfMissing $ work </> "ref-script-size-test"
    --     plutusV3Script <- File <$> liftIO (makeAbsolute "test/cardano-testnet-test/files/plutus/v3/always-succeeds.plutus")
    --     let transferAmount = Coin 10_000_000
    --     -- Submit a transaction to publish the reference script
    --     txBody <- mkSpendOutputsOnlyTx execConfig epochStateView sbe refScriptSizeWork "tx-body" wallet1
    --                 [(ReferenceScriptAddress plutusV3Script, transferAmount)]
    --     signedTx <- signTx execConfig cEra refScriptSizeWork "signed-tx" txBody [SomeKeyPair $ paymentKeyInfoPair wallet1]
    --     submitTx execConfig cEra signedTx
    --     -- Wait until transaction is on chain and obtain transaction identifier
    --     txId <- retrieveTransactionId execConfig signedTx
    --     txIx <- H.evalMaybeM $ watchEpochStateUpdate epochStateView (EpochInterval 2) (getTxIx sbe txId transferAmount)
    --     -- Query the reference script size
    --     let protocolParametersOutFile = refScriptSizeWork </> "ref-script-size-out.json"
    --     H.noteM_ $ execCli' execConfig [ eraName, "query", "ref-script-size"
    --                                    , "--tx-in", txId ++ "#" ++ show (txIx :: Int)
    --                                    , "--out-file", protocolParametersOutFile
    --                                    ]
    --     H.diffFileVsGoldenFile
    --       protocolParametersOutFile
    --       "test/cardano-testnet-test/files/golden/queries/refScriptSizeOut.json"

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
        dreps <- H.noteShowM $ execCliStdoutToJson execConfig [ eraName, "query", "drep-state", "--all-dreps", "--include-stake"]
        let drepsRedacted = redactJsonFields (fromList [("keyHash", "<redacted>")]) dreps
        H.diffVsGoldenFile
          (TL.unpack . TL.decodeUtf8 $ Aeson.encodePretty drepsRedacted)
          "test/cardano-testnet-test/files/golden/queries/drepStateOut.json"

        -- to a file
        let drepStateOutFile = work </> "drep-state-out.json"
            drepStateRedactedOutFile = work </> "drep-state-out-redacted.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "drep-state", "--all-dreps"
                                       , "--include-stake", "--out-file", drepStateOutFile]
        redactJsonFieldsInFile (fromList [("keyHash", "<redacted>")]) drepStateOutFile drepStateRedactedOutFile
        H.diffFileVsGoldenFile
          drepStateRedactedOutFile
          "test/cardano-testnet-test/files/golden/queries/drepStateOut.json"

    TestQueryDRepStakeDistributionCmd -> do
      -- drep-stake-distribution
      -- to stdout
      drepStakeDistribution :: [(L.DRep StandardCrypto, L.Coin)] <- H.noteShowM $ execCliStdoutToJson execConfig [ eraName, "query", "drep-stake-distribution", "--all-dreps" ]

      -- TODO: we could check that the Coin amount below is the one reported
      -- by query stake-address-info

      H.assertWith drepStakeDistribution $ \dreps ->
        length dreps == 3 -- Because, by default, 3 DReps are created

      forM_ drepStakeDistribution $ \(_drep, coin) -> Coin 15_000_003_000_000 H.=== coin

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

  -- We wait till a slot after: 4 * securityParam / slotCoeff
  -- If we query 'govState' before that we get 'PotentialPParamsUpdate'
  -- in 'futurePParams' field
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

  readVerificationKeyFromFile :: (HasCallStack, MonadIO m, MonadCatch m, MonadTest m,  HasTextEnvelope (VerificationKey keyrole), SerialiseAsBech32 (VerificationKey keyrole))
    => AsType keyrole
    -> FilePath
    -> File content direction
    -> m (VerificationKey keyrole)
  readVerificationKeyFromFile asKey work =
    H.evalEitherM . liftIO . runExceptT . readVerificationKeyOrFile asKey . VerificationKeyFilePath . File . (work </>) . unFile

  verificationStakeKeyToStakeAddress :: Int -> VerificationKey StakeKey -> StakeAddress
  verificationStakeKeyToStakeAddress testnetMagic delegatorVKey =
    makeStakeAddress (fromNetworkMagic $ NetworkMagic $ fromIntegral testnetMagic) (StakeCredentialByKey $ verificationKeyHash delegatorVKey)

  _getTxIx :: forall m era. HasCallStack => MonadTest m => ShelleyBasedEra era -> String -> Coin -> (AnyNewEpochState, SlotNo, BlockNo) -> m (Maybe Int)
  _getTxIx sbe txId amount (AnyNewEpochState sbe' newEpochState, _, _) = do
    Refl <- H.leftFail $ assertErasEqual sbe sbe'
    shelleyBasedEraConstraints sbe' (do
      return $ Map.foldlWithKey (\acc (L.TxIn (L.TxId thisTxId) (L.TxIx thisTxIx)) txOut ->
        case acc of
          Nothing | hashToStringAsHex (extractHash thisTxId) == txId &&
                    valueToLovelace (fromLedgerValue sbe (txOut ^. valueTxOutL)) == Just amount -> Just $ fromIntegral thisTxIx
                  | otherwise -> Nothing
          x -> x) Nothing $ L.unUTxO $ newEpochState ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosUtxoL)

-- | @redactJsonStringFieldInFile [(k0, v0), (k1, v1), ..] sourceFilePath targetFilePath@ reads the JSON at @sourceFilePath@, and then
-- replaces the value associated to @k0@ by @v0@, replaces the value associated to @k1@ by @v1@, etc.
-- Then the obtained JSON is written to @targetFilePath@. This replacement is done recursively
-- so @k0@, @k1@, etc. can appear at any depth within the JSON.
redactJsonFieldsInFile
  :: ()
  => MonadTest m
  => MonadIO m
  => HasCallStack
  => Map.Map Text Text -- ^ Map from key name, to the new (String) value to attach to this key
  -> FilePath
  -> FilePath
  -> m ()
redactJsonFieldsInFile changes sourceFilePath targetFilePath = GHC.withFrozenCallStack $ do
  contents <- H.evalIO $ LBS.readFile sourceFilePath
  case Aeson.eitherDecode contents :: Either String Aeson.Value of
    Left err -> do
      H.note_ $ "Failed to decode JSON: " <> err
      H.success
    Right json -> do
      let redactedJson = redactJsonFields changes json
      H.evalIO $ LBS.writeFile targetFilePath $ Aeson.encodePretty redactedJson

redactJsonFields :: () => Map.Map Text Text -> Aeson.Value -> Aeson.Value
redactJsonFields changes v =
  case v of
    Aeson.Object object ->
      let object' = Aeson.mapWithKey
                      (\k v' ->
                        case Map.lookup (Aeson.toText k) changes of
                          Just replacement -> Aeson.String replacement
                          Nothing -> recurse v')
                      object in
          Aeson.Object object'
    Aeson.Array vector ->
      Aeson.Array $ Vector.map recurse vector
    _ -> v
  where
    recurse = redactJsonFields changes
