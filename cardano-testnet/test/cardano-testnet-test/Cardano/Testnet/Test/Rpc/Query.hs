{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.Query
  ( hprop_rpc_query
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.Network as Net.Tx
import           Cardano.Api.Shelley
import qualified Cardano.Api.Tx.UTxO as Utxo

import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Node.Rpc.Client as Rpc
import qualified Cardano.Node.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import qualified Data.ByteString.Short as SBS
import           Data.Default.Class
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Proxy
import           Data.Ratio
import           Data.Set (Set)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro
import           System.FilePath (takeDirectory, (</>))

import           Testnet.Components.Query
import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_rpc_query :: Property
hprop_rpc_query = integrationRetryWorkspace 2 "rpc-query" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  tr@TestnetRuntime
    { testnetMagic
    , configurationFile
    , testnetNodes = node0@TestnetNode{nodeSprocket} : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : wallet1 : _
    } <-
    cardanoTestnetDefault options def conf

  execConfig <- mkExecConfig tempAbsPath' nodeSprocket testnetMagic
  systemStart <- H.noteShowM $ getStartTime tempAbsPath' tr
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  connectionInfo <- nodeConnectionInfo tr 0
  pparams <- unLedgerProtocolParameters <$> getProtocolParams epochStateView ceo
  H.noteShowPretty_ pparams
  rpcSocket <- H.note $ takeDirectory (unFile $ nodeSocketPath node0) </> "rpc.sock"

  -- \* get tip
  QueryTipLocalStateOutput{localStateChainTip} <-
    H.noteShowM $ execCliStdoutToJson execConfig [eraName, "query", "tip"]
  (slot, blockHash) <- case localStateChainTip of
    ChainTipAtGenesis -> H.failure
    ChainTip (SlotNo slot) (HeaderHash hash) _ -> pure (slot, SBS.fromShort hash)

  -- \*** RPC query
  let rpcServer = Rpc.ServerUnix rpcSocket
  response <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    let req = Rpc.defMessage
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) req

  -- \* Test response
  response ^. #ledgerTip ^. #slot === slot
  response ^. #ledgerTip ^. #hash === blockHash

  let chainParams = response ^. #values ^. #cardano
  babbageEraOnwardsConstraints (convert ceo) $ do
    pparams ^. L.ppCoinsPerUTxOByteL ^. to L.unCoinPerByte ^. to L.unCoin
      === chainParams ^. #coinsPerUtxoByte ^. to fromIntegral
    pparams ^. L.ppMaxTxSizeL === chainParams ^. #maxTxSize ^. to fromIntegral
    pparams ^. L.ppMinFeeBL === chainParams ^. #minFeeCoefficient ^. to fromIntegral
    pparams ^. L.ppMinFeeAL === chainParams ^. #minFeeConstant ^. to fromIntegral
    pparams ^. L.ppMaxBBSizeL === chainParams ^. #maxBlockBodySize ^. to fromIntegral
    pparams ^. L.ppMaxBHSizeL === chainParams ^. #maxBlockHeaderSize ^. to fromIntegral
    pparams ^. L.ppKeyDepositL === chainParams ^. #stakeKeyDeposit ^. to fromIntegral
    pparams ^. L.ppPoolDepositL === chainParams ^. #poolDeposit ^. to fromIntegral
    pparams ^. L.ppEMaxL ^. to L.unEpochInterval === chainParams ^. #poolRetirementEpochBound ^. to fromIntegral
    pparams ^. L.ppNOptL === chainParams ^. #desiredNumberOfPools ^. to fromIntegral
    pparams ^. L.ppA0L ^. to L.unboundRational
      === chainParams ^. #poolInfluence ^. #numerator ^. to fromIntegral
        % chainParams ^. #poolInfluence ^. #denominator ^. to fromIntegral

  -- https://docs.cardano.org/about-cardano/explore-more/parameter-guide
  H.failure
