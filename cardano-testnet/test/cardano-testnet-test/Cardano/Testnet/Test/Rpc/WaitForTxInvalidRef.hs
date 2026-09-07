{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.WaitForTxInvalidRef
  ( hprop_rpc_wait_for_tx_invalid_ref
  )
where

import           Cardano.Api (unFile)
import           Cardano.Testnet

import           Prelude

import           Control.Exception (try)
import           Control.Monad (void)
import qualified Data.ByteString as BS
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Proxy (Proxy (..))
import           Lens.Micro
import           Network.GRPC.Spec (GrpcError (..), GrpcException (..))

import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as U5c

import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | E2E test for the WaitForTx gRPC method's error path (SubmitService),
-- covering AC7 of cardano-rpc's mempool-methods.md: a malformed (wrong
-- length) transaction reference fails the stream with INVALID_ARGUMENT
-- before any message is delivered.
--
-- This bypasses 'Rpc.serverStreaming': its @recv@ maps both a clean end of
-- stream and an error-terminated stream to the same 'NextElem'/'NoNextElem'
-- signalling, swallowing the gRPC status (see
-- "Cardano.Testnet.Test.Rpc.FollowTip"'s @followTipExpectingError@, the
-- established precedent this mirrors). 'Rpc.recvOutput' checks the trailers
-- and raises 'GrpcException' on a non-OK terminal status instead.
--
-- Run with:
-- @TASTY_PATTERN='/RPC WaitForTx Invalid Ref/' cabal test cardano-testnet-test@
hprop_rpc_wait_for_tx_invalid_ref :: H.Property
hprop_rpc_wait_for_tx_invalid_ref = integrationRetryWorkspace 2 "rpc-wait-for-tx-invalid-ref" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsBasePath'
  let runtimeOptions = def{runtimeEnableRpc = RpcEnabled}

  TestnetRuntime{testnetNodes = node0 :| _} <-
    createAndRunTestnet def runtimeOptions conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket
      -- Not a well-formed (32-byte) transaction id.
      garbageRef = BS.replicate 10 0xab

  result <-
    H.evalIO . try . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.withRPC conn def (Proxy @(Rpc.Protobuf U5c.SubmitService "waitForTx")) $ \call -> do
        Rpc.sendFinalInput call (def & U5c.ref .~ [garbageRef])
        void $ Rpc.recvOutput call

  case result of
    Left GrpcException{grpcError}
      | grpcError == GrpcInvalidArgument -> H.success
      | otherwise -> do
          H.note_ $ "expected InvalidArgument, got: " <> show grpcError
          H.failure
    Right () -> do
      H.note_ "expected the call to fail with InvalidArgument, but it succeeded"
      H.failure
