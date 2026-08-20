{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.Genesis
  ( hprop_rpc_read_genesis
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp

import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.ByteString as BS
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Word (Word32)
import           Lens.Micro

import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Run with:
-- @TASTY_PATTERN='/RPC ReadGenesis/' cabal test cardano-testnet-test@
hprop_rpc_read_genesis :: H.Property
hprop_rpc_read_genesis = integrationRetryWorkspace 2 "rpc-read-genesis" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'

  let era = Exp.ConwayEra
      sbe = convert era
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
      runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

  TestnetRuntime
    { testnetMagic
    , testnetNodes = node0 :| _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket

  response <-
    H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.QueryService "readGenesis")) def

  H.note_ "genesis is the 32-byte Shelley genesis hash"
  H.assertWith (response ^. U5c.genesis) $ (== 32) . BS.length

  H.note_ "caip2 is derived from the testnet's own network magic"
  response ^. U5c.caip2 H.=== networkMagicToCaip2 (fromIntegral testnetMagic)

  H.note_ "The cardano config oneof is set"
  cardanoGenesis <- H.nothingFail (response ^. U5c.maybe'cardano)

  H.note_ "Shelley: epochLength, networkMagic, systemStart, protocolParams"
  H.assertWith (cardanoGenesis ^. U5c.epochLength) (> 0)
  cardanoGenesis ^. U5c.networkMagic H.=== fromIntegral testnetMagic
  H.assertWith (cardanoGenesis ^. U5c.systemStart) $ not . Text.null
  void $ H.nothingFail (cardanoGenesis ^. U5c.maybe'protocolParams)

  -- TODO: re-enable once cardano-rpc resolves initial funds from sgExtraConfig.
  -- cardano-cli create-testnet-data funds wallets via sgExtraConfig.secInitialFunds and leaves the legacy sgInitialFunds field empty, so the RPC response's initialFunds map is currently always empty for testnet genesis.
  -- Handler fix pending on cardano-api branch mgalazyn/fix/rpc-initial-funds-extraconfig.
  -- H.note_ "initialFunds is non-empty: only the uncompacted boot-time genesis carries it, and cardano-testnet funds its wallets there"
  -- H.assertWith (cardanoGenesis ^. U5c.initialFunds) $ not . Map.null

  H.note_ "Byron: protocolConsts, startTime, bootStakeholders"
  protocolConsts <- H.nothingFail (cardanoGenesis ^. U5c.maybe'protocolConsts)
  H.assertWith (protocolConsts ^. U5c.k) (> 0)
  H.assertWith (cardanoGenesis ^. U5c.startTime) (> 0)
  let bootStakeholderWeights = cardanoGenesis ^. U5c.bootStakeholders
  H.assertWith bootStakeholderWeights $ not . Map.null
  H.assertWith (Map.elems bootStakeholderWeights) $ all (== 1)

  H.note_ "Alonzo: executionPrices, maxTxExUnits, PlutusV1 cost model"
  void $ H.nothingFail (cardanoGenesis ^. U5c.maybe'executionPrices)
  maxTxExUnits <- H.nothingFail (cardanoGenesis ^. U5c.maybe'maxTxExUnits)
  H.assertWith (maxTxExUnits ^. U5c.steps, maxTxExUnits ^. U5c.memory) (/= (0, 0))
  plutusV1CostModel <- H.nothingFail (cardanoGenesis ^. U5c.costModels . U5c.maybe'plutusV1)
  H.assertWith (plutusV1CostModel ^. U5c.values) $ not . null

  H.note_ "Conway: PlutusV3 cost model, committee"
  plutusV3CostModel <- H.nothingFail (cardanoGenesis ^. U5c.costModels . U5c.maybe'plutusV3)
  H.assertWith (plutusV3CostModel ^. U5c.values) $ not . null
  void $ H.nothingFail (cardanoGenesis ^. U5c.maybe'committee)

-- | The CAIP-2 chain identifier for a Cardano network, keyed on the Shelley
-- network magic. Mirrors 'Cardano.Rpc.Server.Internal.UtxoRpc.Query.networkMagicToCaip2'
-- so the test derives its expectation independently of the handler's own logic.
networkMagicToCaip2 :: Word32 -> Text.Text
networkMagicToCaip2 = \case
  764824073 -> "cardano:mainnet"
  1 -> "cardano:preprod"
  2 -> "cardano:preview"
  magic -> "cardano:" <> Text.pack (show magic)
