{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Start.Cardano
  ( ForkPoint(..)
  , CardanoTestnetOptions(..)
  , cardanoPoolNodes
  , cardanoBftNodes
  , cardanoNumPoolNodes
  , extraBftNodeCliArgs
  , TestnetNodeOptions(..)
  , cardanoDefaultTestnetOptions
  , cardanoDefaultTestnetNodeOptions

  , TestnetRuntime (..)
  , PaymentKeyPair(..)

  , cardanoTestnet
  ) where

import           Prelude

import           Cardano.Api hiding (cardanoEra)

import           Control.Monad
import qualified Data.Aeson as J
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import           Data.List ((\\))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.String
import qualified Data.Time.Clock as DTC
import           Data.Word
import qualified System.Directory as IO
import           System.FilePath.Posix ((</>))
import qualified System.Info as OS

import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import           Hedgehog.Extras.Stock.Time (formatIso8601, showUTCTimeSeconds)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H

import qualified Testnet.Conf as H
import           Testnet.Defaults
import           Testnet.Filepath
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run (execCli_)
import qualified Testnet.Property.Assert as H
import           Testnet.Property.Utils
import           Testnet.Runtime as TR
import           Testnet.Start.Byron hiding (TestnetOptions (..))
import           Testnet.Start.Shelley

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoNodes :: [TestnetNodeOptions]
  , cardanoEra :: AnyCardanoEra
  , cardanoEpochLength :: Int
  , cardanoSlotLength :: Double
  , cardanoTestnetMagic :: Int
  , cardanoActiveSlotsCoeff :: Double
  , cardanoMaxSupply :: Word64 -- ^ The amount of ADA you are starting your testnet with
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

cardanoDefaultTestnetOptions :: CardanoTestnetOptions
cardanoDefaultTestnetOptions = CardanoTestnetOptions
  { cardanoNodes = cardanoDefaultTestnetNodeOptions
  , cardanoEra = AnyCardanoEra AlonzoEra
  , cardanoEpochLength = 1500
  , cardanoSlotLength = 0.2
  , cardanoTestnetMagic = 42
  , cardanoActiveSlotsCoeff = 0.2
  , cardanoMaxSupply = 10020000000
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
  }


data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)

-- | Specify a BFT node (Pre-Babbage era only) or an SPO (Shelley era onwards only)
data TestnetNodeOptions
  = BftTestnetNodeOptions [String]
    -- ^ These arguments will be appended to the default set of CLI options when
    -- starting the node.
  | SpoTestnetNodeOptions
  deriving (Eq, Show)

extraBftNodeCliArgs :: TestnetNodeOptions -> [String]
extraBftNodeCliArgs (BftTestnetNodeOptions args) = args
extraBftNodeCliArgs SpoTestnetNodeOptions = []

cardanoPoolNodes :: [TestnetNodeOptions] -> [TestnetNodeOptions]
cardanoPoolNodes = filter (== SpoTestnetNodeOptions)

cardanoBftNodes :: [TestnetNodeOptions] -> [TestnetNodeOptions]
cardanoBftNodes = filter (/= SpoTestnetNodeOptions)

cardanoNumPoolNodes :: [TestnetNodeOptions] -> Int
cardanoNumPoolNodes = length . cardanoPoolNodes

cardanoNumBftNodes :: [TestnetNodeOptions] -> Int
cardanoNumBftNodes = length . cardanoBftNodes

cardanoDefaultTestnetNodeOptions :: [TestnetNodeOptions]
cardanoDefaultTestnetNodeOptions =
  [ BftTestnetNodeOptions []
  , BftTestnetNodeOptions []
  , SpoTestnetNodeOptions
  ]

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> LBS.ByteString
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

cardanoTestnet :: CardanoTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
cardanoTestnet testnetOptions H.Conf {H.tempAbsPath} = do
  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  configurationFile <- H.noteShow $ tempAbsPath' </> "configuration.yaml"
  let testnetMagic = cardanoTestnetMagic testnetOptions
      numBftNodes = cardanoNumBftNodes $ cardanoNodes testnetOptions
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. cardanoNumPoolNodes $ cardanoNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames
      maxByronSupply = cardanoMaxSupply testnetOptions
      fundsPerGenesisAddress = maxByronSupply `div` fromIntegral numBftNodes
      fundsPerByronAddress = fundsPerGenesisAddress - 100000000
      userPoolN = poolNodesN
      maxShelleySupply = 1000000000000

  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (L.length allNodeNames)
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodeNames allPorts))

  let securityParam = 10
  let logDir = makeLogDir $ TmpAbsolutePath tempAbsPath'
  H.createDirectoryIfMissing_ logDir

  forM_ allNodeNames $ \node -> do
    H.createDirectoryIfMissing_ $ tempAbsPath' </> node
    H.createDirectoryIfMissing_ $ tempAbsPath' </> node </> "byron"
    H.createDirectoryIfMissing_ $ tempAbsPath' </> node </> "shelley"

  -- Make topology files
  forM_ allNodeNames $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tempAbsPath' </> node </> "topology.json") $
      mkTopologyConfig (numBftNodes + cardanoNumPoolNodes (cardanoNodes testnetOptions))
                       allPorts port (cardanoEnableP2P testnetOptions)

    H.writeFile (tempAbsPath' </> node </> "port") (show port)

  H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
    . J.encode $ defaultByronProtocolParamsJsonValue

  -- stuff
  execCli_
    [ "byron"
    , "genesis"
    , "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int securityParam
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int numBftNodes
    , "--total-balance", show @Word64 maxByronSupply
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath' </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath' </> "byron"
    ]

  H.renameFile
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "byron/genesis.spec.json")

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath' </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key") (tempAbsPath' </> "node-bft" <> show @Int n </> "byron/delegate.key")
    H.createFileLink (tempAbsPath' </> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json") (tempAbsPath' </> "node-bft" <> show @Int n </> "byron/delegate.cert")

  -- Create keys and addresses to withdraw the initial UTxO into
  forM_ bftNodesN $ \n -> do
    execCli_
      [ "keygen"
      , "--secret", tempAbsPath' </> "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
      ]

    H.execCli
      [ "signing-key-address"
      , "--testnet-magic", show @Int testnetMagic
      , "--secret", tempAbsPath' </> "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
      ] >>= H.writeFile (tempAbsPath' </> "byron/address-00" <> show @Int (n - 1))

    -- Write Genesis addresses to files
    H.execCli
      [ "signing-key-address"
      , "--testnet-magic", show @Int testnetMagic
      , "--secret", tempAbsPath' </> "byron/genesis-keys.00" <> show @Int (n - 1) <> ".key"
      ] >>= H.writeFile (tempAbsPath' </> "byron/genesis-address-00" <> show @Int (n - 1))

  do
    richAddrFrom <- S.firstLine <$> H.readFile (tempAbsPath' </> "byron/genesis-address-000")
    txAddr <- S.firstLine <$> H.readFile (tempAbsPath' </> "byron/address-000")

    -- Create Byron address that moves funds out of the genesis UTxO into a regular
    -- address.
    execCli_
      [ "issue-genesis-utxo-expenditure"
      , "--genesis-json", tempAbsPath' </> "byron/genesis.json"
      , "--testnet-magic", show @Int testnetMagic
      , "--tx", tempAbsPath' </> "tx0.tx"
      , "--wallet-key", tempAbsPath' </> "byron/delegate-keys.000.key"
      , "--rich-addr-from", richAddrFrom
      , "--txout", show @(String, Word64) (txAddr, fundsPerByronAddress)
      ]

  -- Update Proposal and votes
  createByronUpdateProposal
    testnetMagic
    (tempAbsPath' </> "byron/delegate-keys.000.key")
    (tempAbsPath' </> "update-proposal")
    1

  forM_ bftNodesN $ \n -> do
    createByronUpdateProposalVote
      testnetMagic
      (tempAbsPath' </> "update-proposal")
      (tempAbsPath' </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key")
      (tempAbsPath' </> "update-vote.00" <> show @Int (n - 1))

  createByronUpdateProposal
    testnetMagic
    (tempAbsPath' </> "byron/delegate-keys.000.key")
    (tempAbsPath' </> "update-proposal-1")
    2

  forM_ bftNodesN $ \n ->
    createByronUpdateProposalVote
      testnetMagic
      (tempAbsPath' </> "update-proposal-1")
      (tempAbsPath' </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key")
      (tempAbsPath' </> "update-vote-1.00" <> show @Int (n - 1))

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath' </> "byron"

  -- Set up our template
  shelleyDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "shelley"

  alonzoSpecFile <- H.noteTempFile tempAbsPath' "shelley/genesis.alonzo.spec.json"
  gen <- H.evalEither $ first displayError defaultAlonzoGenesis
  H.evalIO $ LBS.writeFile alonzoSpecFile $ J.encode gen


  conwaySpecFile <- H.noteTempFile tempAbsPath' "shelley/genesis.conway.spec.json"
  H.evalIO $ LBS.writeFile conwaySpecFile $ J.encode defaultConwayGenesis

  execCli_
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", shelleyDir
    , "--start-time", formatIso8601 startTime
    ]

  -- Then edit the genesis.spec.json ...

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJsonFile (tempAbsPath' </> "shelley/genesis.spec.json") . J.rewriteObject
    $ HM.insert "activeSlotsCoeff" (J.toJSON @Double (cardanoActiveSlotsCoeff testnetOptions))
    . HM.insert "securityParam" (J.toJSON @Int 10)
    . HM.insert "epochLength" (J.toJSON @Int (cardanoEpochLength testnetOptions))
    . HM.insert "slotLength" (J.toJSON @Double (cardanoSlotLength testnetOptions))
    . HM.insert "maxLovelaceSupply" (J.toJSON @Int maxShelleySupply)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject ( HM.insert "decentralisationParam" (J.toJSON @Double 0.7)
                        . HM.insert "rho" (J.toJSON @Double 0.1)
                        . HM.insert "tau" (J.toJSON @Double 0.1)
                        )
      )

  -- Now generate for real:
  execCli_
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", shelleyDir
    , "--gen-genesis-keys", show @Int numBftNodes
    , "--start-time", formatIso8601 startTime
    , "--gen-utxo-keys", show (cardanoNumBftNodes $ cardanoNodes testnetOptions)
    ]

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath' </> "shelley"

  H.rewriteJsonFile (tempAbsPath' </> "shelley/genesis.json") . J.rewriteObject
    $ flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject (HM.insert "major" (J.toJSON @Int 6))
          )
        )
      )
    . HM.insert "updateQuorum" (J.toJSON @Int 2)

  -- Generated shelley/genesis.json
  H.cat $ tempAbsPath' </> "shelley/genesis.json"

  -- Generated alonzo/genesis.json
  --TODO: rationalise the naming convention on these genesis json files.
  H.cat $ tempAbsPath' </> "shelley/genesis.alonzo.json"
  H.cat $ tempAbsPath' </> "shelley/genesis.conway.json"

  -- Make the pool operator cold keys
  -- This was done already for the BFT nodes as part of the genesis creation

  poolKeys <- forM poolNodesN $ \i -> do
    let node = "node-pool" <> show @Int i

    execCli_
      [ "node", "key-gen"
      , "--cold-verification-key-file", tempAbsPath' </> node </> "shelley/operator.vkey"
      , "--cold-signing-key-file", tempAbsPath' </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath' </> node </> "shelley/operator.counter"
      ]

    poolNodeKeysColdVkey <- H.note $ tempAbsPath' </> "node-pool" <> show i <> "/shelley/operator.vkey"
    poolNodeKeysColdSkey <- H.note $ tempAbsPath' </> "node-pool" <> show i <> "/shelley/operator.skey"
    poolNodeKeysVrfVkey <- H.note $ tempAbsPath' </> node </> "shelley/vrf.vkey"
    poolNodeKeysVrfSkey <- H.note $ tempAbsPath' </> node </> "shelley/vrf.skey"
    poolNodeKeysStakingVkey <- H.note $ tempAbsPath' </> node </> "shelley/staking.vkey"
    poolNodeKeysStakingSkey <- H.note $ tempAbsPath' </> node </> "shelley/staking.skey"

    execCli_
      [ "node", "key-gen-VRF"
      , "--verification-key-file", poolNodeKeysVrfVkey
      , "--signing-key-file", poolNodeKeysVrfSkey
      ]

    return PoolNodeKeys
      { poolNodeKeysColdVkey
      , poolNodeKeysColdSkey
      , poolNodeKeysVrfVkey
      , poolNodeKeysVrfSkey
      , poolNodeKeysStakingVkey
      , poolNodeKeysStakingSkey
      }

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath' </> "shelley/delegate-keys/delegate" <> show @Int n <> ".skey") (tempAbsPath' </> "node-bft" <> show @Int n </> "shelley/operator.skey")
    H.createFileLink (tempAbsPath' </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vkey") (tempAbsPath' </> "node-bft" <> show @Int n </> "shelley/operator.vkey")
    H.createFileLink (tempAbsPath' </> "shelley/delegate-keys/delegate" <> show @Int n <> ".counter") (tempAbsPath' </> "node-bft" <> show @Int n </> "shelley/operator.counter")
    H.createFileLink (tempAbsPath' </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.vkey") (tempAbsPath' </> "node-bft" <> show @Int n </> "shelley/vrf.vkey")
    H.createFileLink (tempAbsPath' </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.skey") (tempAbsPath' </> "node-bft" <> show @Int n </> "shelley/vrf.skey")

  -- Make hot keys and for all nodes
  forM_ allNodeNames $ \node -> do
    execCli_
      [ "node", "key-gen-KES"
      , "--verification-key-file", tempAbsPath' </> node </> "shelley/kes.vkey"
      , "--signing-key-file",      tempAbsPath' </> node </> "shelley/kes.skey"
      ]

    execCli_
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", tempAbsPath' </> node </> "shelley/kes.vkey"
      , "--cold-signing-key-file", tempAbsPath' </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath' </> node </> "shelley/operator.counter"
      , "--out-file", tempAbsPath' </> node </> "shelley/node.cert"
      ]

  -- Generated node operator keys (cold, hot) and operational certs
  forM_ allNodeNames $ \node -> H.noteEachM_ . H.listDirectory $ tempAbsPath' </> node </> "byron"

  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards
  let userAddrs = ("user" <>) . show @Int <$> userPoolN
      poolAddrs = ("pool-owner" <>) . show @Int <$> poolNodesN
      addrs = userAddrs <> poolAddrs


  H.createDirectoryIfMissing_ $ tempAbsPath' </> "addresses"

  wallets <- forM addrs $ \addr -> do
    let paymentSKey = tempAbsPath' </> "addresses/" <> addr <> ".skey"
    let paymentVKey = tempAbsPath' </> "addresses/" <> addr <> ".vkey"

    -- Payment address keys
    execCli_
      [ "address", "key-gen"
      , "--verification-key-file", paymentVKey
      , "--signing-key-file", paymentSKey
      ]

    execCli_
      [ "address", "key-gen"
      , "--verification-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo2.vkey"
      , "--signing-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo2.skey"
      ]

    execCli_
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.vkey"
      , "--signing-key-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.skey"
      ]

    execCli_
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo-stake.vkey"
      , "--signing-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo-stake.skey"
      ]

    execCli_
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo2-stake.vkey"
      , "--signing-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo2-stake.skey"
      ]

    -- Payment addresses
    execCli_
      [ "address", "build"
      , "--payment-verification-key-file", tempAbsPath' </> "addresses/" <> addr <> ".vkey"
      , "--stake-verification-key-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tempAbsPath' </> "addresses/" <> addr <> ".addr"
      ]

    -- Stake addresses
    execCli_
      [ "stake-address", "build"
      , "--stake-verification-key-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.addr"
      ]

    -- Stake addresses registration certs
    execCli_
      [ "stake-address", "registration-certificate"
      , "--stake-verification-key-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.vkey"
      , "--out-file", tempAbsPath' </> "addresses/" <> addr <> "-stake.reg.cert"
      ]

    pure $ PaymentKeyPair
      { paymentSKey
      , paymentVKey
      }

  -- user N will delegate to pool N
  forM_ userPoolN $ \n -> do
    -- Stake address delegation certs
    execCli_
      [ "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tempAbsPath' </> "addresses/user" <> show @Int n <> "-stake.vkey"
      , "--cold-verification-key-file", tempAbsPath' </> "node-pool" <> show @Int n </> "shelley/operator.vkey"
      , "--out-file", tempAbsPath' </> "addresses/user" <> show @Int n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tempAbsPath' </> "addresses/pool-owner" <> show @Int n <> "-stake.vkey") (tempAbsPath' </> "node-pool" <> show @Int n </> "owner.vkey")
    H.createFileLink (tempAbsPath' </> "addresses/pool-owner" <> show @Int n <> "-stake.skey") (tempAbsPath' </> "node-pool" <> show @Int n </> "owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address registration certs, and stake address delegation certs
  H.noteEachM_ . H.listDirectory $ tempAbsPath' </> "addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodeNames $ \node -> do
    H.execCli
      [ "stake-pool", "registration-certificate"
      , "--testnet-magic", show @Int testnetMagic
      , "--pool-pledge", "0", "--pool-cost", "0", "--pool-margin", "0"
      , "--cold-verification-key-file", tempAbsPath' </> node </> "shelley/operator.vkey"
      , "--vrf-verification-key-file", tempAbsPath' </> node </> "shelley/vrf.vkey"
      , "--reward-account-verification-key-file", tempAbsPath' </> node </> "owner.vkey"
      , "--pool-owner-stake-verification-key-file", tempAbsPath' </> node </> "owner.vkey"
      , "--out-file", tempAbsPath' </> node </> "registration.cert"
      ]

  -- Generated stake pool registration certs
  forM_ poolNodeNames $ \node -> H.assertIO . IO.doesFileExist $ tempAbsPath' </> node </> "registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  do
    -- We'll transfer all the funds to the user1, which delegates to pool1
    -- We'll register certs to:
    --  1. register the pool-owner1 stake address
    --  2. register the stake pool 1
    --  3. register the user1 stake address
    --  4. delegate from the user1 stake address to the stake pool
    txIn <- H.noteShow . S.strip =<< createShelleyGenesisInitialTxIn testnetMagic (tempAbsPath' </> "shelley/utxo-keys/utxo1.vkey")

    H.note_ txIn

    user1Addr <- H.readFile $ tempAbsPath' </> "addresses/user1.addr"

    execCli_
      [ "transaction", "build-raw"
      , "--invalid-hereafter", "1000"
      , "--fee", "0"
      , "--tx-in", txIn
      , "--tx-out",  user1Addr <> "+" <> show @Int maxShelleySupply
      , "--certificate-file", tempAbsPath' </> "addresses/pool-owner1-stake.reg.cert"
      , "--certificate-file", tempAbsPath' </> "node-pool1/registration.cert"
      , "--certificate-file", tempAbsPath' </> "addresses/user1-stake.reg.cert"
      , "--certificate-file", tempAbsPath' </> "addresses/user1-stake.deleg.cert"
      , "--out-file", tempAbsPath' </> "tx1.txbody"
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
  execCli_
    [ "transaction", "sign"
    , "--signing-key-file", tempAbsPath' </> "shelley/utxo-keys/utxo1.skey"
    , "--signing-key-file", tempAbsPath' </> "addresses/user1-stake.skey"
    , "--signing-key-file", tempAbsPath' </> "node-pool1/owner.skey"
    , "--signing-key-file", tempAbsPath' </> "node-pool1/shelley/operator.skey"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", tempAbsPath' </> "tx1.txbody"
    , "--out-file", tempAbsPath' </> "tx1.tx"
    ]

  -- Generated a signed 'do it all' transaction:
  H.assertIO . IO.doesFileExist $ tempAbsPath' </> "tx1.tx"

  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  -- TODO: These genesis filepaths should not be hardcoded. Using the cli as a library
  -- rather as an executable will allow us to get the genesis files paths in a more
  -- direct fashion.
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.json") "ShelleyGenesisHash"
  alonzoGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.alonzo.json") "AlonzoGenesisHash"
  conwayGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.conway.json") "ConwayGenesisHash"

  -- TODO: We default to defaultYamlHardforkViaConfig however to enable forking to the appropriate era, we need
  -- to be able to create the appropriate default config value, based on the era.
  -- i.e defaultConfigValue :: CardanoEra era -> Aeson.Value. Do this in a separate PR.

  let finalYamlConfig :: LBS.ByteString
      finalYamlConfig = J.encode . J.Object
                                 $ mconcat [ byronGenesisHash
                                           , shelleyGenesisHash
                                           , alonzoGenesisHash
                                           , conwayGenesisHash
                                           , defaultYamlHardforkViaConfig $ cardanoEra testnetOptions]

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "configuration.yaml") finalYamlConfig

  --------------------------------
  -- Launch cluster of three nodes

  let bftNodeNameAndOpts = L.zip bftNodeNames (cardanoBftNodes $ cardanoNodes testnetOptions)
  bftNodes <- forM bftNodeNameAndOpts $ \(node, nodeOpts) -> do
    startNode (TmpAbsolutePath tempAbsPath') node
      ([ "run"
        , "--config",  tempAbsPath' </> "configuration.yaml"
        , "--topology",  tempAbsPath' </> node </> "topology.json"
        , "--database-path", tempAbsPath' </> node </> "db"
        , "--shelley-kes-key", tempAbsPath' </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath' </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath' </> node </> "shelley/node.cert"
        , "--delegation-certificate",  tempAbsPath' </> node </> "byron/delegate.cert"
        , "--signing-key", tempAbsPath' </> node </> "byron/delegate.key"
        ] <> extraBftNodeCliArgs nodeOpts)

  H.threadDelay 100000

  poolNodes <- forM (L.zip poolNodeNames poolKeys) $ \(node, key) -> do
    runtime <- startNode (TmpAbsolutePath tempAbsPath') node
        [ "run"
        , "--config", tempAbsPath' </> "configuration.yaml"
        , "--topology", tempAbsPath' </> node </> "topology.json"
        , "--database-path", tempAbsPath' </> node </> "db"
        , "--shelley-kes-key", tempAbsPath' </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath' </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath' </> node </> "shelley/node.cert"
        , "--host-addr", ifaceAddress
        ]
    return $ PoolNode runtime key

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ allNodeNames $ \node -> do
    sprocket <- H.noteShow $ makeSprocket (TmpAbsolutePath tempAbsPath') node
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.byDeadlineM 10 deadline "Failed to connect to node socket" $ H.assertM $ H.doesSprocketExist sprocket

  forM_ allNodeNames $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertChainExtended deadline (cardanoNodeLoggingFormat testnetOptions) nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  return TestnetRuntime
    { configurationFile
    , shelleyGenesisFile = tempAbsPath' </> "shelley/genesis.json"
    , testnetMagic
    , bftNodes
    , poolNodes
    , wallets
    , delegators = [] -- TODO this should be populated
    }

