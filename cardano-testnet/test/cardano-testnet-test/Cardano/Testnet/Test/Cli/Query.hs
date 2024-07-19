{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Testnet.Test.Cli.Query
  ( hprop_cli_queries
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (EpochInterval(EpochInterval))
import           Cardano.Api.Shelley (StakePoolKey)

import           Cardano.CLI.Types.Key (readVerificationKeyOrFile, VerificationKeyOrFile (VerificationKeyFilePath))
import           Cardano.CLI.Types.Output (QueryTipLocalStateOutput)
import           Cardano.Testnet

import           Prelude

import           Control.Monad (forM_)
import           Control.Monad.Catch (MonadCatch)
import           Data.Aeson (eitherDecodeStrictText)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.Bifunctor (bimap)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Testnet.Components.Configuration (eraToString)
import           Testnet.Components.Query
import qualified Testnet.Defaults as Defaults
import           Testnet.Process.Run (execCli', execCliStdoutToJson, mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.TestQueryCmds (TestQueryCmds (..), forallQueryCommands)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
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
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , configurationFile
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

  forallQueryCommands (\case

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

    TestQueryConstitutionHashCmd -> do
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

    TestQueryStakeAddressInfoCmd -> do
      -- stake-address-info
      pure ()

    TestQueryUTxOCmd -> do
      -- utxo
      pure ()

    TestQueryLedgerStateCmd -> do
      -- ledger-state
      pure ()

    TestQueryProtocolStateCmd -> do
      -- protocol-state
      pure ()

    TestQueryStakeSnapshotCmd -> do
      -- stake-snapshot
      pure ()

    TestQueryKesPeriodInfoCmd -> do
      -- kes-period-info
      pure ()

    TestQueryPoolStateCmd -> do
      -- pool-state
      pure ()

    TestQueryTxMempoolCmd -> do
      -- tx-mempool
      pure ()

    TestQuerySlotNumberCmd -> do
      -- slot-number
      pure ()

    TestQueryRefScriptSizeCmd -> do
      -- ref-script-size
      pure ()

    TestQueryConstitutionCmd -> do
      -- constitution
      pure ()

    TestQueryGovStateCmd ->
      -- gov-state
      do
        -- to stdout
        output <- execCli' execConfig [ eraName, "query", "gov-state" ]
        patchedOutput <- H.evalEither $ patchGovStateOutput output
        H.diffVsGoldenFile patchedOutput "test/cardano-testnet-test/files/golden/queries/govStateOut.json"
        -- to a file
        let govStateOutFile = work </> "gov-state-out.json"
        H.noteM_ $ execCli' execConfig [ eraName, "query", "gov-state", "--out-file", govStateOutFile ]
        patchGovStateOutputFile govStateOutFile
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

    TestQueryDRepStakeDistributionCmd -> do
      -- drep-stake-distribution
      pure ()

    TestQueryCommitteeMembersStateCmd -> do
      -- committee-state
      pure ()

    )
  where
  patchGovStateOutput :: String -> Either String String
  patchGovStateOutput output = do
    eOutput <- eitherDecodeStrictText (T.pack output)
    return $ T.unpack $ decodeUtf8 $ prettyPrintJSON $ patchGovStateJSON eOutput
    where
      patchGovStateJSON :: Aeson.Object -> Aeson.Object
      patchGovStateJSON o = Aeson.delete "futurePParams" o

  readVerificationKeyFromFile :: (MonadIO m, MonadCatch m, MonadTest m,  HasTextEnvelope (VerificationKey keyrole), SerialiseAsBech32 (VerificationKey keyrole))
    => AsType keyrole
    -> FilePath
    -> File content direction
    -> m (VerificationKey keyrole)
  readVerificationKeyFromFile asKey work =
    H.evalEitherM . liftIO . runExceptT . readVerificationKeyOrFile asKey . VerificationKeyFilePath . File . (work </>) . unFile

  patchGovStateOutputFile :: (MonadTest m, MonadIO m) => FilePath -> m ()
  patchGovStateOutputFile fp = do
    fileContents <- liftIO $ readFile fp
    patchedOutput <- H.evalEither $ patchGovStateOutput fileContents
    liftIO $ writeFile fp patchedOutput


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

