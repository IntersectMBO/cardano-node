{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.Genesis
  ( hprop_rpc_read_genesis
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as U5c
import           Cardano.Testnet

import           Prelude

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Control.Monad.Catch (try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.ByteString as BS
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Word (Word32)
import           Lens.Micro
import           Network.GRPC.Spec (GrpcError (..), GrpcException (..))

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
    { shelleyGenesisFile
    , testnetMagic
    , testnetNodes = node0 :| _
    } <-
    createAndRunTestnet creationOptions runtimeOptions conf

  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0
  let rpcServer = Rpc.ServerUnix rpcSocket

  originalGenesisBytes <- H.evalIO $ BS.readFile shelleyGenesisFile

  H.note_ "The handler caches the parsed genesis only on a successful read (TimedCache.hs), so this hash-mismatch check must run before any successful ReadGenesis call: a successful call first would warm the cache and hide the corruption for up to five idle minutes"
  H.evalIO $ BS.writeFile shelleyGenesisFile (originalGenesisBytes <> " ")

  H.note_ "ReadGenesis fails with FAILED_PRECONDITION when the genesis file's bytes no longer match the hash the node computed at startup"
  readGenesisHashMismatchResult <-
    H.evalIO . try . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.QueryService "readGenesis")) def
  case readGenesisHashMismatchResult of
    Left GrpcException{grpcError}
      | grpcError == GrpcFailedPrecondition -> pure ()
      | otherwise -> do
          H.note_ $ "expected " <> show GrpcFailedPrecondition <> ", got: " <> show grpcError
          H.failure
    Right (_ :: Rpc.Proto U5c.ReadGenesisResponse) -> do
      H.note_ $ "expected " <> show GrpcFailedPrecondition <> ", but the call succeeded"
      H.failure

  H.evalIO $ BS.writeFile shelleyGenesisFile originalGenesisBytes

  response <-
    H.evalIO . Rpc.withConnection def rpcServer $ \conn ->
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf U5c.QueryService "readGenesis")) def

  H.note_ "genesis is the Blake2b-256 hash of the raw Shelley genesis file bytes, exactly as the node computed it at startup"
  response ^. U5c.genesis
    H.=== Crypto.hashToBytes (Crypto.hashWith id originalGenesisBytes :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString)

  H.note_ "caip2 is derived from the testnet's own network magic"
  response ^. U5c.caip2 H.=== networkMagicToCaip2 (fromIntegral testnetMagic)

  H.note_ "The cardano config oneof is set"
  cardanoGenesis <- H.nothingFail (response ^. U5c.maybe'cardano)

  H.note_ "Shelley: epochLength, networkMagic, systemStart, protocolParams"
  H.assertWith (cardanoGenesis ^. U5c.epochLength) (> 0)
  cardanoGenesis ^. U5c.networkMagic H.=== fromIntegral testnetMagic
  H.assertWith (cardanoGenesis ^. U5c.systemStart) $ not . Text.null
  void $ H.nothingFail (cardanoGenesis ^. U5c.maybe'protocolParams)

  H.note_ "initialFunds matches exactly the funds embedded in the genesis file: only the uncompacted boot-time genesis carries them, and cardano-testnet funds its wallets there. extraConfig.initialFunds.data wins when present; the legacy top-level initialFunds field is the fallback, so the test survives the pending cardano-cli revert (PR #1420) that moves the funds back to the top level"
  genesisJson <- H.leftFail (Aeson.eitherDecodeStrict' originalGenesisBytes :: Either String Aeson.Value)
  -- Mirrors the preference order of ledger's own 'resolveInjectionSource': extraConfig wins.
  initialFundsObject <-
    H.nothingFail $
      (genesisJson ^? Aeson.key "extraConfig" . Aeson.key "initialFunds" . Aeson.key "data" . Aeson._Object)
        <|> (genesisJson ^? Aeson.key "initialFunds" . Aeson._Object)

  expectedInitialFunds <-
    Map.fromList
      <$> H.nothingFail
        ( traverse
            (\(addressKey, amount) -> (,) (Aeson.toText addressKey) <$> amount ^? Aeson._Integer)
            (Aeson.toList initialFundsObject)
        )

  H.note_ "initialFunds is non-empty (regression guard for #6655: cardano-testnet must always fund its wallets)"
  H.assertWith expectedInitialFunds $ not . Map.null

  actualInitialFunds <-
    Map.fromList
      <$> traverse
        (\(addressHex, amount) -> (,) addressHex . toInteger <$> H.nothingFail (amount ^. U5c.maybe'int))
        (Map.toList (cardanoGenesis ^. U5c.initialFunds))

  actualInitialFunds H.=== expectedInitialFunds

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
