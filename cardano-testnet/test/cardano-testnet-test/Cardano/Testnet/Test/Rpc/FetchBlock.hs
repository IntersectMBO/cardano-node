{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.FetchBlock (
    hprop_rpc_fetch_block,
)
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp

import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Sync as U5c
import           Cardano.Testnet

import           Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Default.Class
import           Lens.Micro

import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Run with:
-- @TASTY_PATTERN='/RPC FetchBlock/' cabal test cardano-testnet-test@
--
hprop_rpc_fetch_block :: H.Property
hprop_rpc_fetch_block = integrationRetryWorkspace 2 "rpc-fetch-block" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
    conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
    let tempAbsPath' = unTmpAbsPath tempAbsPath

    let era = Exp.ConwayEra
        sbe = convert era
        eraName = eraToString sbe
        creationOptions = def{creationEra = AnyShelleyBasedEra sbe}
        runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

    _tr@TestnetRuntime
        { testnetMagic
        , testnetNodes = node0@TestnetNode{nodeSprocket} : _
        } <-
        createAndRunTestnet creationOptions runtimeOptions conf

    execConfig <- mkExecConfig tempAbsPath' nodeSprocket testnetMagic
    rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

    -- Get chain tip via CLI
    QueryTipLocalStateOutput{localStateChainTip} <-
        H.noteShowM $ execCliStdoutToJson execConfig [eraName, "query", "tip"]
    (slot, blockHash, blockNo) <- case localStateChainTip of
        ChainTipAtGenesis -> H.failure
        ChainTip (SlotNo tipSlot) (HeaderHash hash) (BlockNo bn) -> pure (tipSlot, SBS.fromShort hash, bn)

    H.note_ $ "Tip slot: " <> show slot
    H.note_ $ "Tip block number: " <> show blockNo
    H.note_ $ "Tip hash: " <> show (BS.length blockHash) <> " bytes"

    -- Call FetchBlock via gRPC
    let rpcServer = Rpc.ServerUnix rpcSocket
        blockRef = def & U5c.slot .~ slot & U5c.hash .~ blockHash
        request = def & U5c.ref .~ blockRef

    response <- H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
        Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.SyncService "fetchBlock")) request

    let block = response ^. U5c.block

    -- Verify nativeBytes is non-empty
    let rawBytes = block ^. U5c.nativeBytes
    H.note_ $ "Block CBOR: " <> show (BS.length rawBytes) <> " bytes"
    H.assertWith rawBytes $ not . BS.null

    -- Verify cardano block header matches the requested tip
    block ^. U5c.cardano . U5c.header . U5c.slot H.=== slot
    block ^. U5c.cardano . U5c.header . U5c.hash H.=== blockHash

    -- height is the block number from ChainDB
    block ^. U5c.cardano . U5c.header . U5c.height H.=== blockNo

    -- TODO: verify timestamp once FetchBlock populates it from EraHistory (node kernel snapshot migration)
    -- connectionInfo <- nodeConnectionInfo tr 0
    -- (systemStart, eraHistory) <-
    --     (H.leftFail <=< H.leftFailM) . H.evalIO $
    --       executeLocalStateQueryExpr connectionInfo VolatileTip $ do
    --         ss <- querySystemStart
    --         eh <- queryEraHistory
    --         pure $ (,) <$> ss <*> eh
    -- expectedTimestampMs :: Word64 <- H.leftFail $ do
    --     utcTime <- slotToUTCTime systemStart eraHistory (SlotNo slot)
    --     pure . round $ utcTimeToPOSIXSeconds utcTime * 1000
    -- H.assertWithinTolerance (block ^. U5c.cardano . U5c.timestamp) expectedTimestampMs 1000
