{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Golden.Manifest
  ( golden_Manifest
  ) where

import           Prelude

import           Data.Aeson (eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import           Data.IP (IP (IPv4), toIPv4)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Network.Socket (tupleToHostAddress)
import           System.FilePath ((</>))

import           Testnet.Manifest

import           Hedgehog
import           Hedgehog.Extras.Test.Base (propertyOnce)
import           Hedgehog.Extras.Test.Golden (diffVsGoldenFile)
import qualified Hedgehog.Extras.Test.Process as H

-- | A fixed sample manifest for golden testing.
-- All values are deterministic (fixed ports, PIDs, times) so the output
-- is stable across runs.  The three nodes vary the optional fields so
-- every ManifestGrpc constructor and the pid-null encoding are pinned.
sampleManifest :: Manifest
sampleManifest = Manifest
  { manifestSchemaVersion        = 1
  , manifestCreatedAt            = UTCTime (fromGregorian 2026 1 1) 0
    -- A fixed literal, not the live package version: the golden file must
    -- change only when the manifest shape changes, not on version bumps.
  , manifestCardanoTestnetVersion = "0.0.0.0"
  , manifestNetwork              = ManifestNetwork
      { mnMagic       = 42
      , mnEra         = "conway"
      , mnSystemStart = UTCTime (fromGregorian 2025 12 31) 43200
      }
  , manifestPaths                = ManifestPaths
      { mpNodeConfigFile = "configuration.yaml"
      , mpGenesisFiles   = ManifestGenesisFiles
          { mgfByron    = "byron-genesis.json"
          , mgfShelley  = "shelley-genesis.json"
          , mgfAlonzo   = "alonzo-genesis.json"
          , mgfConway   = "conway-genesis.json"
          , mgfDijkstra = "dijkstra-genesis.json"
          }
      }
  , manifestNodes =
      [ ManifestNode
          { mnodeName = "node1", mnodeRole = "spo", mnodeHost = tupleToHostAddress (127, 0, 0, 1)
          , mnodePort = 30001, mnodeSocketPath = "socket/node1/sock"
          , mnodeGrpc = Just (ManifestGrpcHttp (IPv4 (toIPv4 [127, 0, 0, 1])) 50051)
          , mnodePid = Just 12345
          , mnodePidFile = "logs/node1/node.pid"
          , mnodeTopologyFile = "node-data/node1/topology.json"
          , mnodeStdoutFile = "logs/node1/stdout.log"
          , mnodeStderrFile = "logs/node1/stderr.log"
          }
      , ManifestNode
          { mnodeName = "node2", mnodeRole = "relay", mnodeHost = tupleToHostAddress (127, 0, 0, 1)
          , mnodePort = 30002, mnodeSocketPath = "socket/node2/sock"
          , mnodeGrpc = Just (ManifestGrpcUnixSocket "socket/node2/rpc.sock")
          , mnodePid = Just 12346
          , mnodePidFile = "logs/node2/node.pid"
          , mnodeTopologyFile = "node-data/node2/topology.json"
          , mnodeStdoutFile = "logs/node2/stdout.log"
          , mnodeStderrFile = "logs/node2/stderr.log"
          }
      , ManifestNode
          { mnodeName = "node3", mnodeRole = "relay", mnodeHost = tupleToHostAddress (127, 0, 0, 1)
          , mnodePort = 30003, mnodeSocketPath = "socket/node3/sock"
          , mnodeGrpc = Nothing, mnodePid = Nothing
          , mnodePidFile = "logs/node3/node.pid"
          , mnodeTopologyFile = "node-data/node3/topology.json"
          , mnodeStdoutFile = "logs/node3/stdout.log"
          , mnodeStderrFile = "logs/node3/stderr.log"
          }
      ]
  , manifestWallets =
      [ ManifestWallet "utxo1" "addr_test1sample1"
          "utxo-keys/utxo1/utxo.skey" "utxo-keys/utxo1/utxo.vkey"
      , ManifestWallet "utxo2" "addr_test1sample2"
          "utxo-keys/utxo2/utxo.skey" "utxo-keys/utxo2/utxo.vkey"
      , ManifestWallet "utxo3" "addr_test1sample3"
          "utxo-keys/utxo3/utxo.skey" "utxo-keys/utxo3/utxo.vkey"
      ]
  }

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_Manifest/"'@
golden_Manifest :: Property
golden_Manifest = propertyOnce $ do
  base <- H.getProjectBase
  let encoded = encodePretty sampleManifest
      actualJson = Text.unpack $ Text.decodeUtf8 $ LBS.toStrict encoded
  diffVsGoldenFile actualJson $
    base </> "cardano-testnet/test/cardano-testnet-golden/files/golden/manifest.json"

  -- Round-trip check: decode the encoded JSON and re-encode; must match.
  decodedManifest <- evalEither (eitherDecode encoded :: Either String Manifest)
  let reEncoded = encodePretty decodedManifest
  encoded === reEncoded
