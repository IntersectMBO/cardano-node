{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Transaction.RegisterDeregisterStakeAddress
  ( hprop_tx_register_deregister_stake_address
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (Coin (..), EpochInterval (..))

import qualified Cardano.Crypto.Hash as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.State.Strict (StateT)
import           Data.Default.Class
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.String
import qualified Data.Text as Text
import           GHC.Exts (IsList (..))
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults
import           Testnet.EpochStateProcessing (waitForGovActionVotes)
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.Transaction
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
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
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

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"
      stakeCertFp = gov </> "stake.regcert"
      stakeCertDeregFp = gov </> "stake.deregcert"
      stakeKeys = KeyPair { verificationKey = File stakeVkeyFp
                          , signingKey = File stakeSKeyFp
                          }

  cliStakeAddressKeyGen stakeKeys

  keyDepositStr <- show . unCoin <$> getKeyDeposit epochStateView ceo
  -- Register stake address
  void $ execCli' execConfig
    [ eraName, "stake-address", "registration-certificate"
    , "--stake-verification-key-file", stakeVkeyFp
    , "--key-reg-deposit-amt", keyDepositStr
    , "--out-file", stakeCertFp
    ]

  stakeAddress <- H.noteM $ execCli' execConfig
    [ eraName, "stake-address", "build"
    , "--stake-verification-key-file", stakeVkeyFp
    , "--out-file", "/dev/stdout"
    ]

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
    , "--signing-key-file", stakeSKeyFp
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]

  H.noteShowM_ $ waitForBlocks epochStateView 1
  -- H.noteShowM_ $ waitForEpochs epochStateView (EpochInterval 2)

  do
    (_, stdout', stderr') <- execCliAny execConfig
      [ eraName, "query", "stake-address-info"
      , "--address", stakeAddress
      , "--out-file", "/dev/stdout"
      ]
    H.note_ stdout'
    H.note_ stderr'

  -- deregister stake address
  void $ execCli' execConfig
    [ eraName, "stake-address", "deregistration-certificate"
    , "--stake-verification-key-file", stakeVkeyFp
    , "--key-reg-deposit-amt", keyDepositStr
    , "--out-file", stakeCertDeregFp
    ]

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
    , "--signing-key-file", stakeSKeyFp
    , "--out-file", stakeCertDeregTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertDeregTxSignedFp
    ]
