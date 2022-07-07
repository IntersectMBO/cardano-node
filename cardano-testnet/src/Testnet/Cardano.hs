{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Cardano
  ( ForkPoint(..)
  , TestnetOptions(..)
  , defaultTestnetOptions
  , TestnetNodeOptions(..)
  , defaultTestnetNodeOptions

  , Era(..)
  , TestnetRuntime (..)
  , bftSprockets
  , allNodes
  , TestnetNode (..)
  , Wallet(..)

  , testnet
  ) where

import           Control.Applicative (pure)
import           Control.Monad (Monad (..), forM_, void, forM, (=<<), when, fmap, return)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.=))
import           Data.Bool (Bool(..))
import           Data.ByteString.Lazy (ByteString)
import           Data.Eq (Eq)
import           Data.Function (($), (.), flip, id)
import           Data.Functor ((<$>), (<&>))
import           Data.Int (Int)
import           Data.List (length, replicate, unzip5, zip, zipWith6, (\\))
import           Data.Maybe (Maybe(Just), fromJust)
import           Data.Ord (Ord((<=)))
import           Data.Semigroup (Semigroup((<>)))
import           Data.String (IsString(fromString), String)
import           GHC.Enum (Bounded, Enum)
import           GHC.Float (Double)
import           GHC.Num (Num((-), (+)))
import           GHC.Real (fromIntegral, Integral(div))
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time (formatIso8601, showUTCTimeSeconds)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Read (Read)
import           Text.Show (Show(show))

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)

data Era = Byron | Shelley | Allegra | Mary | Alonzo deriving (Eq, Enum, Bounded, Read, Show)

data TestnetOptions = TestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    bftNodeOptions :: [TestnetNodeOptions]
  , numPoolNodes :: Int
  , era :: Era
  , epochLength :: Int
  , slotLength :: Double
  , activeSlotsCoeff :: Double
  , enableP2P :: Bool
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { bftNodeOptions = replicate 2 defaultTestnetNodeOptions
  , numPoolNodes = 1
  , era = Alonzo
  , epochLength = 1500
  , slotLength = 0.2
  , activeSlotsCoeff = 0.2
  , enableP2P = False
  }

newtype TestnetNodeOptions = TestnetNodeOptions
  { -- | These arguments will be appended to the default set of CLI options when
    -- starting the node.
    extraNodeCliArgs :: [String]
  } deriving (Eq, Show)

defaultTestnetNodeOptions :: TestnetNodeOptions
defaultTestnetNodeOptions = TestnetNodeOptions
  { extraNodeCliArgs = []
  }

data TestnetRuntime = TestnetRuntime
  { configurationFile :: FilePath
  , testnetMagic :: Int
  , bftNodes :: [TestnetNode]
  , poolNodes :: [TestnetNode]
  , wallets :: [Wallet]
  }

bftSprockets :: TestnetRuntime -> [Sprocket]
bftSprockets = fmap nodeSprocket . bftNodes

allNodes :: TestnetRuntime -> [TestnetNode]
allNodes tr = bftNodes tr <> poolNodes tr

data TestnetNode = TestnetNode
  { nodeName :: String
  , nodeSprocket :: Sprocket
  , nodeStdinHandle :: IO.Handle
  , nodeStdout :: FilePath
  , nodeStderr :: FilePath
  , nodeProcessHandle :: IO.ProcessHandle
  }

data Wallet = Wallet
  { paymentVKey :: FilePath
  , paymentSKey :: FilePath
  } deriving (Eq, Show)

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> ByteString
mkTopologyConfig numNodes allPorts port False = J.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numNodes - 1)
        | peerPort <- allPorts \\ [port]
        ]
mkTopologyConfig numNodes allPorts port True = J.encode topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        [ RelayAccessAddress (fromString ifaceAddress)
                             (fromIntegral peerPort)
        | peerPort <- allPorts \\ [port]
        ]
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (numNodes - 1)
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (P2P.UseLedger DontUseLedger)


testnet :: TestnetOptions -> H.Conf -> H.Integration TestnetRuntime
testnet testnetOptions H.Conf {..} = do
  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"
  let numBftNodes = length (bftNodeOptions testnetOptions)
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. numPoolNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames
      maxByronSupply = 10020000000
      fundsPerGenesisAddress = maxByronSupply `div` numBftNodes
      fundsPerByronAddress = fundsPerGenesisAddress - 100000000
      userPoolN = poolNodesN
      maxShelleySupply = 1000000000000

  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (L.length allNodeNames)
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodeNames allPorts))

  let securityParam = 10

  H.createDirectoryIfMissing logDir

  H.readFile configurationTemplate >>= H.writeFile configurationFile

  forkOptions <- pure $ id
    . HM.insert "EnableLogMetrics" (J.toJSON False)
    . HM.insert "EnableLogging" (J.toJSON True)
    . case era testnetOptions of
        Byron -> id
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 1)

        Shelley -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 2)

        Allegra -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAllegraHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 3)

        Mary -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAllegraHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestMaryHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 4)

        Alonzo -> id
          . HM.insert "TestShelleyHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAllegraHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestMaryHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "TestAlonzoHardForkAtEpoch" (J.toJSON @Int 0)
          . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 6)

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.insert "Protocol" (J.toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (J.toJSON @Double 0.6)
    . HM.insert "minSeverity" (J.toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (J.toJSON @String "byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (J.toJSON @String "shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (J.toJSON @String "shelley/genesis.alonzo.json")
    . HM.insert "RequiresNetworkMagic" (J.toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (J.toJSON @Int 0)
    . HM.insert "TraceBlockchainTime" (J.toJSON True)
    . HM.delete "GenesisFile"
    . HM.insert "TestEnableDevelopmentHardForkEras" (J.toJSON @Bool True)
    . HM.insert "TestEnableDevelopmentNetworkProtocols" (J.toJSON @Bool True)
    . HM.insert "EnableP2P" (J.toJSON @Bool (enableP2P testnetOptions))
    . forkOptions

  forM_ allNodeNames $ \node -> do
    H.createDirectoryIfMissing $ tempAbsPath </> node
    H.createDirectoryIfMissing $ tempAbsPath </> node </> "byron"
    H.createDirectoryIfMissing $ tempAbsPath </> node </> "shelley"

  -- Make topology files
  forM_ allNodeNames $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tempAbsPath </> node </> "topology.json") $
      mkTopologyConfig (numBftNodes + numPoolNodes testnetOptions) allPorts port (enableP2P testnetOptions)

    H.writeFile (tempAbsPath </> node </> "port") (show port)

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json") . J.encode $ J.object
    [ "heavyDelThd" .= J.toJSON @String "300000000000"
    , "maxBlockSize" .= J.toJSON @String "2000000"
    , "maxTxSize" .= J.toJSON @String "4096"
    , "maxHeaderSize" .= J.toJSON @String "2000000"
    , "maxProposalSize" .= J.toJSON @String "700"
    , "mpcThd" .= J.toJSON @String "20000000000000"
    , "scriptVersion" .= J.toJSON @Int 0
    , "slotDuration" .= J.toJSON @String "1000"
    , "softforkRule" .= J.object
      [ "initThd" .= J.toJSON @String "900000000000000"
      , "minThd" .= J.toJSON @String "600000000000000"
      , "thdDecrement" .= J.toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= J.object
      [ "multiplier" .= J.toJSON @String "43946000000"
      , "summand" .= J.toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= J.toJSON @String "18446744073709551615"
    , "updateImplicit" .= J.toJSON @String "10000"
    , "updateProposalThd" .= J.toJSON @String "100000000000000"
    , "updateVoteThd" .= J.toJSON @String "1000000000000"
    ]

  -- stuff
  void . H.execCli $
    [ "byron"
    , "genesis"
    , "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int securityParam
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int numBftNodes
    , "--total-balance", show @Int maxByronSupply
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath </> "byron"
    ]

  H.renameFile
    (tempAbsPath </> "byron.genesis.spec.json")
    (tempAbsPath </> "byron/genesis.spec.json")

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key") (tempAbsPath </> "node-bft" <> show @Int n </> "byron/delegate.key")
    H.createFileLink (tempAbsPath </> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json") (tempAbsPath </> "node-bft" <> show @Int n </> "byron/delegate.cert")

  -- Create keys and addresses to withdraw the initial UTxO into
  forM_ bftNodesN $ \n -> do
    void $ H.execCli
      [ "keygen"
      , "--secret", tempAbsPath </> "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
      ]

    H.execCli
      [ "signing-key-address"
      , "--testnet-magic", show @Int testnetMagic
      , "--secret", tempAbsPath </> "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
      ] >>= H.writeFile (tempAbsPath </> "byron/address-00" <> show @Int (n - 1))

    -- Write Genesis addresses to files
    H.execCli
      [ "signing-key-address"
      , "--testnet-magic", show @Int testnetMagic
      , "--secret", tempAbsPath </> "byron/genesis-keys.00" <> show @Int (n - 1) <> ".key"
      ] >>= H.writeFile (tempAbsPath </> "byron/genesis-address-00" <> show @Int (n - 1))

  do
    richAddrFrom <- S.firstLine <$> H.readFile (tempAbsPath </> "byron/genesis-address-000")
    txAddr <- S.firstLine <$> H.readFile (tempAbsPath </> "byron/address-000")

    -- Create Byron address that moves funds out of the genesis UTxO into a regular
    -- address.
    void $ H.execCli
      [ "issue-genesis-utxo-expenditure"
      , "--genesis-json", tempAbsPath </> "byron/genesis.json"
      , "--testnet-magic", show @Int testnetMagic
      , "--tx", tempAbsPath </> "tx0.tx"
      , "--wallet-key", tempAbsPath </> "byron/delegate-keys.000.key"
      , "--rich-addr-from", richAddrFrom
      , "--txout", show @(String, Int) (txAddr, fundsPerByronAddress)
      ]

  -- Update Proposal and votes
  void $ H.execCli
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", tempAbsPath </> "update-proposal"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--protocol-version-major", "1"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

  forM_ bftNodesN $ \n -> do
    void $ H.execCli
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", tempAbsPath </> "update-proposal"
      , "--testnet-magic", show @Int testnetMagic
      , "--signing-key", tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
      , "--vote-yes"
      , "--output-filepath", tempAbsPath </> "update-vote.00" <> show @Int (n - 1)
      ]

  void $ H.execCli
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", tempAbsPath </> "update-proposal-1"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--protocol-version-major", "2"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

  forM_ bftNodesN $ \n ->
    void $ H.execCli
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", tempAbsPath </> "update-proposal-1"
      , "--testnet-magic", show @Int testnetMagic
      , "--signing-key", tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
      , "--vote-yes"
      , "--output-filepath", tempAbsPath </> "update-vote-1.00" <> show @Int (n - 1)
      ]

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "byron"

  -- Set up our template
  H.createDirectoryIfMissing $ tempAbsPath </> "shelley"

  -- TODO: This is fragile, we should be passing in all necessary
  -- configuration files.
  let sourceAlonzoGenesisSpecFile = base </> "cardano-cli/test/data/golden/alonzo/genesis.alonzo.spec.json"
  alonzoSpecFile <- H.noteTempFile tempAbsPath "shelley/genesis.alonzo.spec.json"
  liftIO $ IO.copyFile sourceAlonzoGenesisSpecFile alonzoSpecFile

  void $ H.execCli
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tempAbsPath </> "shelley"
    , "--start-time", formatIso8601 startTime
    ]

  -- Then edit the genesis.spec.json ...

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJsonFile (tempAbsPath </> "shelley/genesis.spec.json") . J.rewriteObject
    $ HM.insert "activeSlotsCoeff" (J.toJSON @Double (activeSlotsCoeff testnetOptions))
    . HM.insert "securityParam" (J.toJSON @Int 10)
    . HM.insert "epochLength" (J.toJSON @Int (epochLength testnetOptions))
    . HM.insert "slotLength" (J.toJSON @Double (slotLength testnetOptions))
    . HM.insert "maxLovelaceSupply" (J.toJSON @Int maxShelleySupply)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject ( HM.insert "decentralisationParam" (J.toJSON @Double 0.7)
                        . HM.insert "rho" (J.toJSON @Double 0.1)
                        . HM.insert "tau" (J.toJSON @Double 0.1)
                        )
      )

  -- Now generate for real:
  void $ H.execCli
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tempAbsPath </> "shelley"
    , "--gen-genesis-keys", show @Int numBftNodes
    , "--start-time", formatIso8601 startTime
    , "--gen-utxo-keys", show @Int (numPoolNodes testnetOptions)
    ]

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "shelley"

  H.rewriteJsonFile (tempAbsPath </> "shelley/genesis.json") . J.rewriteObject
    $ flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject (HM.insert "major" (J.toJSON @Int 6))
          )
        )
      )
    . HM.insert "updateQuorum" (J.toJSON @Int 2)

  -- Generated shelley/genesis.json
  H.cat $ tempAbsPath </> "shelley/genesis.json"

  -- Generated alonzo/genesis.json
  --TODO: rationalise the naming convention on these genesis json files.
  H.cat $ tempAbsPath </> "shelley/genesis.alonzo.json"

  -- Make the pool operator cold keys
  -- This was done already for the BFT nodes as part of the genesis creation
  forM_ poolNodeNames $ \node -> do
    void $ H.execCli
      [ "node", "key-gen"
      , "--cold-verification-key-file", tempAbsPath </> node </> "shelley/operator.vkey"
      , "--cold-signing-key-file", tempAbsPath </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> node </> "shelley/operator.counter"
      ]

    void $ H.execCli
      [ "node", "key-gen-VRF"
      , "--verification-key-file", tempAbsPath </> node </> "shelley/vrf.vkey"
      , "--signing-key-file", tempAbsPath </> node </> "shelley/vrf.skey"
      ]

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".skey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.skey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vkey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.vkey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".counter") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.counter")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.vkey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/vrf.vkey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.skey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/vrf.skey")

  -- Make hot keys and for all nodes
  forM_ allNodeNames $ \node -> do
    void $ H.execCli
      [ "node", "key-gen-KES"
      , "--verification-key-file", tempAbsPath </> node </> "shelley/kes.vkey"
      , "--signing-key-file",      tempAbsPath </> node </> "shelley/kes.skey"
      ]

    void $ H.execCli
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", tempAbsPath </> node </> "shelley/kes.vkey"
      , "--cold-signing-key-file", tempAbsPath </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> node </> "shelley/operator.counter"
      , "--out-file", tempAbsPath </> node </> "shelley/node.cert"
      ]

  -- Generated node operator keys (cold, hot) and operational certs
  forM_ allNodeNames $ \node -> H.noteEachM_ . H.listDirectory $ tempAbsPath </> node </> "byron"

  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards
  let userAddrs = ("user" <>) . show @Int <$> userPoolN
      poolAddrs = ("pool-owner" <>) . show @Int <$> poolNodesN
      addrs = userAddrs <> poolAddrs


  H.createDirectoryIfMissing $ tempAbsPath </> "addresses"

  wallets <- forM addrs $ \addr -> do
    let paymentSKey = tempAbsPath </> "addresses/" <> addr <> ".skey"
    let paymentVKey = tempAbsPath </> "addresses/" <> addr <> ".vkey"

    -- Payment address keys
    void $ H.execCli
      [ "address", "key-gen"
      , "--verification-key-file", paymentVKey
      , "--signing-key-file", paymentSKey
      ]

    void $ H.execCli
      [ "address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "shelley/utxo-keys/utxo2.vkey"
      , "--signing-key-file", tempAbsPath </> "shelley/utxo-keys/utxo2.skey"
      ]

    void $ H.execCli
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--signing-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.skey"
      ]

    void $ H.execCli
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "shelley/utxo-keys/utxo-stake.vkey"
      , "--signing-key-file", tempAbsPath </> "shelley/utxo-keys/utxo-stake.skey"
      ]

    void $ H.execCli
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "shelley/utxo-keys/utxo2-stake.vkey"
      , "--signing-key-file", tempAbsPath </> "shelley/utxo-keys/utxo2-stake.skey"
      ]

    -- Payment addresses
    void $ H.execCli
      [ "address", "build"
      , "--payment-verification-key-file", tempAbsPath </> "addresses/" <> addr <> ".vkey"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> ".addr"
      ]

    -- Stake addresses
    void $ H.execCli
      [ "stake-address", "build"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> "-stake.addr"
      ]

    -- Stake addresses registration certs
    void $ H.execCli
      [ "stake-address", "registration-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> "-stake.reg.cert"
      ]

    pure $ Wallet
      { paymentSKey
      , paymentVKey
      }

  -- user N will delegate to pool N
  forM_ userPoolN $ \n -> do
    -- Stake address delegation certs
    void $ H.execCli
      [ "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/user" <> show @Int n <> "-stake.vkey"
      , "--cold-verification-key-file", tempAbsPath </> "node-pool" <> show @Int n </> "shelley/operator.vkey"
      , "--out-file", tempAbsPath </> "addresses/user" <> show @Int n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> show @Int n <> "-stake.vkey") (tempAbsPath </> "node-pool" <> show @Int n </> "owner.vkey")
    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> show @Int n <> "-stake.skey") (tempAbsPath </> "node-pool" <> show @Int n </> "owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address registration certs, and stake address delegation certs
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodeNames $ \node -> do
    H.execCli
      [ "stake-pool", "registration-certificate"
      , "--testnet-magic", show @Int testnetMagic
      , "--pool-pledge", "0", "--pool-cost", "0", "--pool-margin", "0"
      , "--cold-verification-key-file", tempAbsPath </> node </> "shelley/operator.vkey"
      , "--vrf-verification-key-file", tempAbsPath </> node </> "shelley/vrf.vkey"
      , "--reward-account-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--pool-owner-stake-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--out-file", tempAbsPath </> node </> "registration.cert"
      ]

  -- Generated stake pool registration certs
  forM_ poolNodeNames $ \node -> H.assertIO . IO.doesFileExist $ tempAbsPath </> node </> "registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  do
    -- We'll transfer all the funds to the user1, which delegates to pool1
    -- We'll register certs to:
    --  1. register the pool-owner1 stake address
    --  2. register the stake pool 1
    --  3. register the user1 stake address
    --  4. delegate from the user1 stake address to the stake pool
    txIn <- H.noteShow . S.strip =<< H.execCli
      [ "genesis", "initial-txin"
      , "--testnet-magic", show @Int testnetMagic
      , "--verification-key-file", tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
      ]

    H.note_ txIn

    user1Addr <- H.readFile $ tempAbsPath </> "addresses/user1.addr"

    void $ H.execCli
      [ "transaction", "build-raw"
      , "--invalid-hereafter", "1000"
      , "--fee", "0"
      , "--tx-in", txIn
      , "--tx-out",  user1Addr <> "+" <> show @Int maxShelleySupply
      , "--certificate-file", tempAbsPath </> "addresses/pool-owner1-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "node-pool1/registration.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user1-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user1-stake.deleg.cert"
      , "--out-file", tempAbsPath </> "tx1.txbody"
      ]
  -- TODO: this will become the transaction to register the pool, etc.
  -- We'll need to pick the tx-in from the actual UTxO since it contains the txid,
  -- we'll have to query this via cardano-cli query utxo.

  {-  cardano-cli transaction build-raw \
          --invalid-hereafter 1000000 --fee 0 \
          --tx-in 67209bfcdf78f8cd86f649da75053a80fb9bb3fad68465554f9301c31b496c65#0 \
          --tx-out $(cat example/addresses/user1.addr)+450000000 \
          --certificate-file example/addresses/pool-owner1-stake.reg.cert \
          --certificate-file example/node-pool1/registration.cert \
          --certificate-file example/addresses/user1-stake.reg.cert \
          --certificate-file example/addresses/user1-stake.deleg.cert \
          --out-file example/register-pool.txbody
  -}

  {-  cardano-cli address convert \
          --byron-key-file example/byron/payment-keys.000.key \
          --signing-key-file example/byron/payment-keys.000-converted.key
  -}

  {-  cardano-cli transaction sign \
          --tx-body-file example/register-pool.txbody \
          --testnet-magic 42 \
          --signing-key-file example/byron/payment-keys.000-converted.key \
          --signing-key-file example/shelley/utxo-keys/utxo1.skey \
          --signing-key-file example/addresses/user1-stake.skey \
          --signing-key-file example/node-pool1/owner.skey \
          --signing-key-file example/node-pool1/shelley/operator.skey \
          --out-file example/register-pool.tx
  -}

  {-  cardano-cli transaction submit \
          --tx-file example/register-pool.tx --testnet-magic 42
  -}

  -- So we'll need to sign this with a bunch of keys:
  -- 1. the initial utxo spending key, for the funds
  -- 2. the user1 stake address key, due to the delegation cert
  -- 3. the pool1 owner key, due to the pool registration cert
  -- 3. the pool1 operator key, due to the pool registration cert
  void $ H.execCli
    [ "transaction", "sign"
    , "--signing-key-file", tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
    , "--signing-key-file", tempAbsPath </> "addresses/user1-stake.skey"
    , "--signing-key-file", tempAbsPath </> "node-pool1/owner.skey"
    , "--signing-key-file", tempAbsPath </> "node-pool1/shelley/operator.skey"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", tempAbsPath </> "tx1.txbody"
    , "--out-file", tempAbsPath </> "tx1.tx"
    ]

  -- Generated a signed 'do it all' transaction:
  H.assertIO . IO.doesFileExist $ tempAbsPath </> "tx1.tx"

  --------------------------------
  -- Launch cluster of three nodes

  let bftNodeNameAndOpts = zip bftNodeNames (bftNodeOptions testnetOptions)
  (bftSprockets', bftStdins, bftStdouts, bftStderrs, bftProcessHandles) <- fmap unzip5 . forM bftNodeNameAndOpts $ \(node, nodeOpts) -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

    (Just stdIn, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        ([ "run"
        , "--config",  tempAbsPath </> "configuration.yaml"
        , "--topology",  tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "shelley/node.cert"
        , "--port",  portString
        , "--delegation-certificate",  tempAbsPath </> node </> "byron/delegate.cert"
        , "--signing-key", tempAbsPath </> node </> "byron/delegate.key"
        ] <> extraNodeCliArgs nodeOpts) <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    H.noteShowM_ $ H.getPid hProcess

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

    return (sprocket, stdIn, nodeStdoutFile, nodeStderrFile, hProcess)

  H.threadDelay 100000

  (poolSprockets, poolStdins, poolStdouts, poolStderrs, poolProcessHandles) <- fmap unzip5 . forM poolNodeNames $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

    (Just stdIn, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "shelley/node.cert"
        , "--host-addr", ifaceAddress
        , "--port",  portString
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

    return (sprocket, stdIn, nodeStdoutFile, nodeStderrFile, hProcess)

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ allNodeNames $ \node -> do
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.assertByDeadlineM deadline $ H.doesSprocketExist sprocket

  forM_ allNodeNames $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertByDeadlineIOCustom "stdout does not contain \"until genesis start time\"" deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile
    H.assertByDeadlineIOCustom "stdout does not contain \"Chain extended\"" deadline $ IO.fileContains "Chain extended, new tip" nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  return TestnetRuntime
    { configurationFile
    , testnetMagic
    , bftNodes = zipWith6 TestnetNode
        bftNodeNames
        bftSprockets'
        bftStdins
        bftStdouts
        bftStderrs
        bftProcessHandles
    , poolNodes = zipWith6 TestnetNode
        poolNodeNames
        poolSprockets
        poolStdins
        poolStdouts
        poolStderrs
        poolProcessHandles
    , wallets
    }
