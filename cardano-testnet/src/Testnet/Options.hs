{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Options
  ( BabbageTestnetOptions(..)
  , defaultTestnetOptions
  , defaultYamlConfig
  ) where

import           Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMapAeson
import           Data.Proxy
import           Data.Scientific
import qualified Data.Vector as Vector

import           Cardano.Tracing.Config

import           Testnet.Util.Runtime (NodeLoggingFormat (..))

{- HLINT ignore "Redundant flip" -}

data BabbageTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes :: Int
  , babbageSlotDuration :: Int
  , babbageSecurityParam :: Int
  , babbageTotalBalance :: Int
  , babbageNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: BabbageTestnetOptions
defaultTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes = 3
  , babbageSlotDuration = 200
  , babbageSecurityParam = 10
  , babbageTotalBalance = 10020000000
  , babbageNodeLoggingFormat = NodeLoggingFormatAsJson
  }


defaultYamlConfig :: KeyMapAeson.KeyMap Aeson.Value
defaultYamlConfig =
  mconcat $ map (uncurry KeyMapAeson.singleton)
    [
    -- TODO: Remnants from the Byron era that we can actually eliminate
    -- and hardcode.
      ("ApplicationName", "cardano-sl")
    , ("ApplicationVersion", Aeson.Number 1)

    -- The consensus protocol to use
    , ("Protocol", "Cardano")

    -- Socket path of the node
    , ("SocketPath", "db/node.socket")
    , ("PBftSignatureThreshold", Aeson.Number (fromFloatDigits (0.6 :: Double)))

    -- Global logging severity filter. Messages must have at least this severity to pass.
    , ("minSeverity", "Debug")

    , ("EnableLogMetrics", Aeson.Bool False)
    , ("TurnOnLogMetrics", Aeson.Bool False)

    -- The maximum number of used peers during bulk sync.
    , ("MaxConcurrencyBulkSync", Aeson.Number 1)

    -- The maximum number of used peers when fetching newly forged blocks.
    , ("MaxConcurrencyDeadline", Aeson.Number 2)

    -- Turn logging on or off
    , ("EnableLogging", Aeson.Bool True)

    -- Genesis filepaths
    , ("ByronGenesisFile", "byron/genesis.json")
    , ("ShelleyGenesisFile", "shelley/genesis.json")
    , ("AlonzoGenesisFile", "shelley/genesis.alonzo.json")
    , ("ConwayGenesisFile", "shelley/genesis.conway.json")

    -- See: https://github.com/input-output-hk/cardano-ledger/blob/master/eras/byron/ledger/impl/doc/network-magic.md
    , ("RequiresNetworkMagic", "RequiresMagic")

    -- Thhe protocol version number gets used by block producing nodes as part
    -- of the system for agreeing on and synchronising protocol updates.
    , ("LastKnownBlockVersion-Major", Aeson.Number 6)
    , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
    , ("LastKnownBlockVersion-Alt", Aeson.Number 0)

    -- Allows a direct hardfork to an era of your choice via the configuration.
    -- This removes the usual requirement for submitting an update proposal,
    -- waiting for the protocol to change and then restarting the nodes.
    , ("ExperimentalHardForksEnabled", Aeson.Bool True)
    , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
    , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
    , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
    , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)

    -- Enable peer to peer discovery
    , ("EnableP2P", Aeson.Bool False)

    -- Logging related
    , ("setupScribes", setupScribes)
    , ("rotation", rotationObject)
    , ("defaultScribes", defaultScribes)
    , ("setupBackends", Aeson.Array $ Vector.fromList ["KatipBK"])
    , ("defaultBackends", Aeson.Array $ Vector.fromList ["KatipBK"])
    , ("options", Aeson.object mempty)
    ] ++ tracers
 where
  -- | Various tracers we can turn on or off
  tracers = map (\(k,v) -> KeyMapAeson.singleton (Key.fromText k) v)
    [ (proxyName (Proxy @TraceBlockFetchClient), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchDecisions), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchProtocolSerialised), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockchainTime), Aeson.Bool True)
    , (proxyName (Proxy @TraceChainDB), Aeson.Bool True)
    , (proxyName (Proxy @TraceChainSyncClient), Aeson.Bool False)
    , (proxyName (Proxy @TraceChainSyncBlockServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceChainSyncHeaderServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceChainSyncProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceDnsResolver), Aeson.Bool True)
    , (proxyName (Proxy @TraceDnsSubscription), Aeson.Bool True)
    , (proxyName (Proxy @TraceErrorPolicy), Aeson.Bool True)
    , (proxyName (Proxy @TraceLocalErrorPolicy), Aeson.Bool True)
    , (proxyName (Proxy @TraceForge), Aeson.Bool True)
    , (proxyName (Proxy @TraceHandshake), Aeson.Bool False)
    , (proxyName (Proxy @TraceIpSubscription), Aeson.Bool True)
    , (proxyName (Proxy @TraceLocalRootPeers), Aeson.Bool True)
    , (proxyName (Proxy @TracePublicRootPeers), Aeson.Bool True)
    , (proxyName (Proxy @TracePeerSelection), Aeson.Bool True)
    , (proxyName (Proxy @TracePeerSelection), Aeson.Bool False)
    , (proxyName (Proxy @TracePeerSelectionActions), Aeson.Bool True)
    , (proxyName (Proxy @TraceConnectionManager), Aeson.Bool True)
    , (proxyName (Proxy @TraceServer), Aeson.Bool True)
    , (proxyName (Proxy @TraceLocalConnectionManager), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalChainSyncProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalHandshake), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalTxSubmissionProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalTxSubmissionServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceMempool), Aeson.Bool True)
    , (proxyName (Proxy @TraceMux), Aeson.Bool False)
    , (proxyName (Proxy @TraceTxInbound), Aeson.Bool False)
    , (proxyName (Proxy @TraceTxOutbound), Aeson.Bool False)
    , (proxyName (Proxy @TraceTxSubmissionProtocol), Aeson.Bool False)
    ]

  defaultScribes :: Aeson.Value
  defaultScribes =
    Aeson.Array $ Vector.fromList
      [ Aeson.Array $ Vector.fromList ["FileSK","logs/mainnet.log"]
      , Aeson.Array $ Vector.fromList ["StdoutSK","stdout"]
      ]


  rotationObject :: Aeson.Value
  rotationObject =
    Aeson.Object $
      mconcat $ map (uncurry KeyMapAeson.singleton)
        [ ("rpLogLimitBytes", Aeson.Number 5000000)
        , ("rpKeepFilesNum", Aeson.Number 3)
        , ("rpMaxAgeHours", Aeson.Number 24)
        ]
  setupScribes :: Aeson.Value
  setupScribes =
    Aeson.Array $ Vector.fromList
      [ Aeson.Object $ mconcat $ map (uncurry KeyMapAeson.singleton)
          [ ("scKind", "FileSK")
          , ("scName", "logs/node.log")
          , ("scFormat", "ScJson")
          ]
      , Aeson.Object $ mconcat $ map (uncurry KeyMapAeson.singleton)
          [ ("scKind", "StdoutSK")
          , ("scName", "stdout")
          , ("scFormat", "ScJson")
          ]
      ]
