{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Cli.Transaction
  ( hprop_transaction
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Type.Common
import           Cardano.Crypto.Hash.Class (hashToStringAsHex)
import qualified Cardano.Ledger.Core as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Text as Text
import           GHC.Exts (IsList (..))
import           Lens.Micro
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Configuration
import           Testnet.Components.Query (findLargestUtxoWithAddress, findUtxosWithAddress,
                   getEpochStateView, waitForBlocks)
import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H


-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/simple transaction build/"'@
hprop_transaction :: Property
hprop_transaction = integrationRetryWorkspace 2 "simple transaction build" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    sbe = ShelleyBasedEraConway
    era = toCardanoEra sbe
    cEra = AnyCardanoEra era
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    options = def { cardanoNodeEra = AnyShelleyBasedEra sbe }

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets=wallet0:_
    } <- cardanoTestnetDefault options def conf

  poolNode1 <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolNode1
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath poolNode1)


  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  -- This is a double check that we can still deserialize Plutus V2 protocol parameters
  void $ execCli' execConfig
    [ anyEraToString cEra, "query", "protocol-parameters"
    , "--cardano-mode"
    , "--out-file", work </> "pparams.json"
    ]

  (txin1, TxOut _addr outValue _datum _refScript) <- H.nothingFailM $ findLargestUtxoWithAddress epochStateView sbe (paymentKeyInfoAddr wallet0)
  let (L.Coin initialAmount) = txOutValueToLovelace outValue

  let transferAmount = 5_000_001
  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show transferAmount
    , "--out-file", txbodyFp
    ]
  cddlUnwitnessedTx <- H.readJsonFileOk txbodyFp
  apiTx <- H.evalEither $ deserialiseFromTextEnvelope cddlUnwitnessedTx
  let txFee = L.unCoin $ extractTxFee sbe apiTx

  -- This is the current calculated fee.
  -- It's a sanity check to see if anything has
  -- changed regarding fee calculation.
  -- 8.10 changed fee from 228 -> 330
  -- 9.2  changed fee from 330 -> 336
  336 === txFee

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]

  txSubmissionResult :: TxSubmissionResult <- execCliStdoutToJson execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]
  let TxId txHash = txhash txSubmissionResult

  H.noteShowM_ $ waitForBlocks epochStateView 1

  H.byDurationM 1 15 "Expected UTxO found" $ do
    utxo2 <- findUtxosWithAddress epochStateView sbe (paymentKeyInfoAddr wallet0)
    txouts2 <- H.noteShow $ L.unCoin . txOutValueToLovelace . txOutValue . snd <$> toList utxo2
    H.assertWith txouts2 $ \txouts2' ->
      [transferAmount, initialAmount - transferAmount - txFee] == txouts2'

    -- Check that the transaction output exists, when querying by id:
    void $ execCli' execConfig
      [ anyEraToString cEra, "query", "utxo"
      , "--tx-in", hashToStringAsHex txHash <> "#0"
      ]

txOutValue :: TxOut ctx era -> TxOutValue era
txOutValue (TxOut _ v _ _) = v

extractTxFee :: ShelleyBasedEra era -> Tx era -> L.Coin
extractTxFee _ (ShelleyTx sbe ledgerTx) =
  shelleyBasedEraConstraints sbe $ ledgerTx ^. (L.bodyTxL . L.feeTxBodyL)
