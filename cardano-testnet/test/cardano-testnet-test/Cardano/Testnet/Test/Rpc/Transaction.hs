{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.Transaction
  ( hprop_rpc_transaction
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Binary.Version as L
import qualified Cardano.Ledger.Conway.Core as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Plutus as L
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Query ()
import           Cardano.Testnet

import           Prelude

import qualified Data.ByteString.Short as SBS
import           Data.Default.Class
import qualified Data.Map.Strict as M
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_rpc_transaction :: Property
hprop_rpc_transaction = integrationRetryWorkspace 2 "rpc-tx" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe, cardanoEnableRpc = True}

  TestnetRuntime
    { testnetMagic
    , configurationFile
    , testnetNodes = node0@TestnetNode{nodeSprocket} : _
    } <-
    createAndRunTestnet options def conf

  execConfig <- mkExecConfig tempAbsPath' nodeSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  pparams <- unLedgerProtocolParameters <$> getProtocolParams epochStateView ceo
  -- H.noteShowPretty_ pparams
  utxos <- findAllUtxos epochStateView sbe
  H.noteShowPretty_ utxos
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  --------------
  -- RPC queries
  --------------
  let rpcServer = Rpc.ServerUnix rpcSocket
  (pparamsResponse, utxosResponse) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <- do
      let req = Rpc.defMessage
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) req

    utxos' <- do
      let req = Rpc.defMessage
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) req
    pure (pparams', utxos')

  H.failure
