{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.Transaction.HashMismatch
  ( hprop_transaction_build_wrong_hash
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (Coin (unCoin))

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import qualified Data.Text as Text
import           GHC.IO.Exception (ExitCode (ExitFailure))
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Hash (serveFilesWhile, tamperBase16Hash)
import           Testnet.Components.Query
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Run (addEnvVarsToConfig, execCli', execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Transaction Build Wrong Hash/'@
hprop_transaction_build_wrong_hash :: Property
hprop_transaction_build_wrong_hash = integrationRetryWorkspace 2 "wrong-hash" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do

  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      asbe = AnyShelleyBasedEra sbe
      eraName = eraToString sbe
      fastTestnetOptions = def { cardanoNodeEra = asbe }
      shelleyOptions = def { genesisEpochLength = 200 }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions shelleyOptions conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  let proposalAnchorDataIpfsHash = "QmexFJuEn5RtnHEqpxDcqrazdHPzAwe7zs2RxHLfMH5gBz"
  proposalAnchorFile <- H.noteM $ liftIO $ makeAbsolute $ "test" </> "cardano-testnet-test" </> "files" </> "sample-proposal-anchor"

  infoActionFp <- H.note $ work </> gov </> "info.action"

  proposalAnchorDataHash <- execCli' execConfig
    [ "hash", "anchor-data", "--file-binary", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"
      stakeCertFp = gov </> "stake.regcert"
      stakeKeys =  KeyPair { verificationKey = File stakeVkeyFp
                           , signingKey = File stakeSKeyFp
                           }

  cliStakeAddressKeyGen stakeKeys

  -- Register stake address
  keyDeposit <- getKeyDeposit epochStateView ceo

  void $ execCli' execConfig
    [ eraName, "stake-address", "registration-certificate"
    , "--stake-verification-key-file", stakeVkeyFp
    , "--key-reg-deposit-amt", show $ unCoin keyDeposit
    , "--out-file", stakeCertFp
    ]

  stakeCertTxBodyFp <- H.note $ work </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ work </> "stake.registration.tx"

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertTxBodyFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", stakeCertTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--signing-key-file", stakeSKeyFp
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]

  let relativeUrl = ["ipfs", proposalAnchorDataIpfsHash]

  txbodyFp <- H.note $ work </> "tx.body"

  tamperedHash <- H.evalMaybe $ tamperBase16Hash proposalAnchorDataHash

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, proposalAnchorFile)]
    ( \port -> do
        let execConfig' = addEnvVarsToConfig execConfig [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]

        void $
          execCli'
            execConfig'
            [ eraName, "governance", "action", "create-info"
            , "--testnet"
            , "--governance-action-deposit", show @Int 1_000_000 -- TODO: Get this from the node
            , "--deposit-return-stake-verification-key-file", stakeVkeyFp
            , "--anchor-url", "ipfs://" ++ proposalAnchorDataIpfsHash
            , "--anchor-data-hash", tamperedHash
            , "--out-file", infoActionFp
            ]

        txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

        (exitCode, _, stderrOutput) <-
          execCliAny
            execConfig'
            [ eraName, "transaction", "build"
            , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
            , "--tx-in", Text.unpack $ renderTxIn txin2
            , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
            , "--proposal-file", infoActionFp
            , "--out-file", txbodyFp
            ]

        exitCode H.=== ExitFailure 1

        H.note_ stderrOutput

        H.assertWith (Text.pack stderrOutput) ("Hashes do not match!" `Text.isInfixOf`)
    )
