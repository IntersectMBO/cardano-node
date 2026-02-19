{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Transaction.BuildEstimate
  ( hprop_tx_build_estimate
  ) where

import           Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import qualified Data.Aeson as Aeson
import           Data.Default.Class
import qualified Data.Text as Text
import           GHC.Stack
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.SPO (
                   createStakeKeyRegistrationCertificate)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (aesonObjectLookUp,integrationWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Transaction Build Estimate/"'@
hprop_tx_build_estimate :: Property
hprop_tx_build_estimate = integrationWorkspace "transaction-build-estimate" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      fastTestnetOptions = def { cardanoNodeEra = AnyShelleyBasedEra sbe }
      shelleyOptions = def { genesisEpochLength = 200 }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- createAndRunTestnet fastTestnetOptions shelleyOptions conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  -- Register stake address
  let stakeCertFp = work </> "stake.regcert"
      stakeKeys =  KeyPair { verificationKey = File $ work </> "stake.vkey"
                           , signingKey = File $ work </> "stake.skey"
                           }

  cliStakeAddressKeyGen stakeKeys
  keyDeposit <- getKeyDeposit epochStateView ceo
  createStakeKeyRegistrationCertificate
    tempAbsPath (AnyShelleyBasedEra sbe) (verificationKey stakeKeys) keyDeposit stakeCertFp


  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ execCli' execConfig
    [ eraName, "query", "protocol-parameters"
    , "--cardano-mode"
    , "--out-file", work </> "pparams.json"
    ]
  let txBodyEstimateFile = work </> "tx-body-estimate.body"
  void $ execCli' execConfig 
    [ eraName, "transaction", "build-estimate"
    , "--shelley-key-witnesses", "2"
    , "--byron-key-witnesses", "0"
    , "--reference-script-size", "0"
    , "--total-utxo-value", "10800380"
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--certificate-file", stakeCertFp
    , "--certificate-file", stakeCertFp
    , "--certificate-file", stakeCertFp
    , "--certificate-file", stakeCertFp
    , "--certificate-file", stakeCertFp
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 10_000_000
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", txBodyEstimateFile
    ]
  let debugOutputFile = work </> "debug-output.txt" 
  void $ execCli' execConfig ["debug", "transaction", "view", "--tx-file", txBodyEstimateFile, "--out-file", debugOutputFile]

  generated :: Aeson.Value <- H.leftFailM . H.readJsonFile $ debugOutputFile
  mFee <- aesonObjectLookUp generated "fee"
  case mFee of 
    Just (Aeson.String feeText) ->  feeText === "380 Lovelace"
    Just v -> H.failMessage  callStack $ "Expected a String but got: " <> show v
    Nothing -> H.failMessage callStack "Expected a JSON object"
  
 



