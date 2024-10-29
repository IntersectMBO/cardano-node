{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Transaction.RegisterDeregisterStakeAddress
  ( hprop_tx_register_deregister_stake_address
  ) where

import           Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import qualified Data.Text as Text
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.SPO (createStakeKeyDeregistrationCertificate,
                   createStakeKeyRegistrationCertificate)
import           Testnet.Process.Run (execCli', execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/register deregister stake address in transaction build/"'@
hprop_tx_register_deregister_stake_address :: Property
hprop_tx_register_deregister_stake_address = integrationWorkspace "register-deregister-stake-address" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      eraName = eraToString sbe
      fastTestnetOptions = def { cardanoNodeEra = AnyShelleyBasedEra sbe }
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

  -- Register stake address
  let stakeCertFp = work </> "stake.regcert"
      stakeCertDeregFp = work </> "stake.deregcert"
      stakeKeys =  KeyPair { verificationKey = File $ work </> "stake.vkey"
                           , signingKey = File $ work </> "stake.skey"
                           }
  cliStakeAddressKeyGen stakeKeys
  keyDeposit <- getKeyDeposit epochStateView ceo
  createStakeKeyRegistrationCertificate
    tempAbsPath (AnyShelleyBasedEra sbe) (verificationKey stakeKeys) keyDeposit stakeCertFp

  stakeCertTxBodyFp <- H.note $ work </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ work </> "stake.registration.tx"

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  (_, stdout, stderr) <- execCliAny execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertTxBodyFp
    ]
  H.note_ stdout
  H.note_ stderr

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", stakeCertTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--signing-key-file", signingKeyFp stakeKeys
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]

  H.noteShowM_ $ waitForBlocks epochStateView 1

  -- deregister stake address
  createStakeKeyDeregistrationCertificate
    tempAbsPath sbe (verificationKey stakeKeys) keyDeposit stakeCertDeregFp

  stakeCertDeregTxBodyFp <- H.note $ work </> "stake.deregistration.txbody"
  stakeCertDeregTxSignedFp <- H.note $ work </> "stake.deregistration.tx"

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  (_, stdout', stderr') <- execCliAny execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertDeregFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertDeregTxBodyFp
    ]
  H.note_ stdout'
  H.note_ stderr'

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", stakeCertDeregTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--signing-key-file", signingKeyFp stakeKeys
    , "--out-file", stakeCertDeregTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertDeregTxSignedFp
    ]
