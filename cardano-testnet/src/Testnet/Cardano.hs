{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Testnet.Cardano
  ( ForkPoint(..)
  , CardanoTestnetOptions(..)
  , cardanoPoolNodes
  , cardanoBftNodes
  , cardanoNumPoolNodes
  , extraBftNodeCliArgs
  , defaultTestnetOptions
  , TestnetNodeOptions(..)
  , cardanoDefaultTestnetNodeOptions

  , Era(..)
  , TestnetRuntime (..)
  , PaymentKeyPair(..)

  , cardanoTestnet
  ) where

import           Prelude

import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import           Data.List ((\\))
import           Data.Maybe
import           Data.String
import           Data.Word
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.Time (formatIso8601, showUTCTimeSeconds)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           System.FilePath.Posix ((</>))

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified System.Directory as IO
import qualified System.Info as OS


import           Testnet.Commands.Genesis
import           Testnet.Commands.Governance
import qualified Testnet.Util.Assert as H
import           Testnet.Util.Cli
import qualified Testnet.Util.Process as H
import           Testnet.Util.Process (execCli_)
import           Testnet.Util.Runtime as TR (NodeLoggingFormat (..), PaymentKeyPair (..)
                   , PoolNode(..), PoolNodeKeys(..), NodeRuntime (..), TestnetRuntime (..), startNode, makeSprocket, TmpPath(..)
                   , getLogDir)

import qualified Testnet.Conf as H
import           Testnet.Conf hiding (testnetMagic)

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)

data Era = Byron | Shelley | Allegra | Mary | Alonzo deriving (Eq, Enum, Bounded, Read, Show)

data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoNodes :: [TestnetNodeOptions]
  , cardanoEra :: Era
  , cardanoEpochLength :: Int
  , cardanoSlotLength :: Double
  , cardanoActiveSlotsCoeff :: Double
  , cardanoMaxSupply :: Word64 -- ^ The amount of ADA you are starting your testnet with
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: CardanoTestnetOptions
defaultTestnetOptions = CardanoTestnetOptions
  { cardanoNodes = cardanoDefaultTestnetNodeOptions
  , cardanoEra = Alonzo
  , cardanoEpochLength = 1500
  , cardanoSlotLength = 0.2
  , cardanoActiveSlotsCoeff = 0.2
  , cardanoMaxSupply = 10020000000
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsText
  }

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

cardanoTestnet :: CardanoTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
cardanoTestnet testnetOptions configuration@H.Conf {tempAbsPath, testnetMagic} = do
  let tmpDir = tempAbsPath
  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  let
      numBftNodes = cardanoNumBftNodes $ cardanoNodes testnetOptions
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. cardanoNumPoolNodes $ cardanoNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames

  ( byronGenesisKeys
    , byronDelegationCerts
    , byronDelegationKeys) <- cardanoTestnetByronGenesis
       (TmpPath tmpDir)
       testnetMagic
       testnetOptions
       (configurationTemplate configuration)
       startTime
{-
  -- dead code: not checked in any test
  cardanoTestnetByronGenesisExpenditure testnetOptions configuration byronGenesisKeys
  cardanoTestnetByronGovernance
    (L.length $ cardanoBftNodeOptions testnetOptions)
    testnetMagic
    tmpDir
-}
  cardanoTestnetShelleyGenesis
    (base configuration)
    (TmpPath tmpDir)
    testnetMagic
    testnetOptions
    startTime

  operatorKeys <- cardanoTestnetOperatorKeys tmpDir poolNodeNames
  vrfKeys <- cardanoTestnetVrfKeys tmpDir poolNodeNames
  poolNodeKeys <- cardanoTestnetPoolKeys tmpDir poolNodesN operatorKeys vrfKeys
  cardanoTestnetDelegationKeyLinks tmpDir bftNodesN
  cardanoTestnetKesKeys tmpDir allNodeNames

  wallets <- cardanoTestnetPaymentKeys tmpDir testnetMagic poolNodesN
--  cardanoTestnetPoolCerts tmpDir testnetMagic poolNodesN      -- dead code
  bftNodes <- cardanoTestnetLaunchBftNodes (TmpPath tmpDir) testnetOptions
  H.threadDelay 100000

  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  byronGenesisHash <- getByronGenesisHash $ tmpDir </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash $ tmpDir </> "shelley/genesis.json"
  alonzoGenesisHash <- getShelleyGenesisHash $ tmpDir </> "shelley/genesis.alonzo.json"
  conwayGenesisHash <- getShelleyGenesisHash $ tmpDir </> "shelley/genesis.conway.json"
  H.rewriteYamlFile (tmpDir </> "configuration.yaml") . J.rewriteObject
    $ HM.insert "ByronGenesisHash" byronGenesisHash
    . HM.insert "ShelleyGenesisHash" shelleyGenesisHash
    . HM.insert "AlonzoGenesisHash" alonzoGenesisHash
    . HM.insert "ConwayGenesisHash" conwayGenesisHash

  poolRuntimes <- cardanoTestnetLaunchPoolNodes (TmpPath tmpDir) poolNodeNames

  cardanoTestnetWaitStartup configuration (cardanoNodeLoggingFormat testnetOptions) allNodeNames
  configurationFile <- H.noteShow $ tmpDir </> "configuration.yaml"

  let
  return TestnetRuntime
    { configurationFile
    , shelleyGenesisFile = tmpDir </> "shelley/genesis.json"
    , testnetMagic
    , bftNodes
    , poolNodes = zipWith PoolNode poolRuntimes poolNodeKeys
    , wallets
    , delegators = error "Testnet.Cardana delegators undefind"
    }

cardanoTestnetByronGenesis
  :: TmpPath
  -> TestnetMagic
  -> CardanoTestnetOptions
  -> FilePath
  -> DTC.UTCTime
  -> H.Integration
    ( [File ByronKey]
    , [File ByronDelegationCert]
    , [File ByronDelegationKey]
    )
cardanoTestnetByronGenesis tp testnetMagic testnetOptions configurationTemplate startTime = do
  let TmpPath tmpDir = tp
      numBftNodes = cardanoNumBftNodes $ cardanoNodes testnetOptions
      bftNodesN = [1 .. numBftNodes]
      poolNodesN = [1 .. cardanoNumPoolNodes $ cardanoNodes testnetOptions]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN
      poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      allNodeNames = bftNodeNames <> poolNodeNames
      maxByronSupply = cardanoMaxSupply testnetOptions

  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (L.length allNodeNames)
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodeNames allPorts))

  let securityParam = 10
  configurationFile <- H.noteShow $ tmpDir </> "configuration.yaml"
  H.readFile configurationTemplate >>= H.writeFile configurationFile

  forkOptions <- pure $ id
    . HM.insert "EnableLogMetrics" (J.toJSON False)
    . HM.insert "EnableLogging" (J.toJSON True)
    . case cardanoEra testnetOptions of
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
  H.rewriteYamlFile (tmpDir </> "configuration.yaml") . J.rewriteObject
    $ HM.insert "Protocol" (J.toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (J.toJSON @Double 0.6)
    . HM.insert "minSeverity" (J.toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (J.toJSON @String "byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (J.toJSON @String "shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (J.toJSON @String "shelley/genesis.alonzo.json")
    . HM.insert "ConwayGenesisFile" (J.toJSON @String "shelley/genesis.conway.json")
    . HM.insert "RequiresNetworkMagic" (J.toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (J.toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (J.toJSON @Int 0)
    . HM.insert "TraceBlockchainTime" (J.toJSON True)
    . HM.delete "GenesisFile"
    . HM.insert "TestEnableDevelopmentHardForkEras" (J.toJSON @Bool True)
    . HM.insert "EnableP2P" (J.toJSON @Bool (cardanoEnableP2P testnetOptions))
    . flip HM.alter "setupScribes"
        ( fmap
          . J.rewriteArrayElements
            . J.rewriteObject
              . HM.insert "scFormat"
                $ case cardanoNodeLoggingFormat testnetOptions of
                  NodeLoggingFormatAsJson -> "ScJson"
                  NodeLoggingFormatAsText -> "ScText")
    . forkOptions

  forM_ allNodeNames $ \node -> do
    H.createDirectoryIfMissing $ tmpDir </> node
    H.createDirectoryIfMissing $ tmpDir </> node </> "byron"
    H.createDirectoryIfMissing $ tmpDir </> node </> "shelley"

  -- Make topology files
  forM_ allNodeNames $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tmpDir </> node </> "topology.json") $
      mkTopologyConfig (numBftNodes + cardanoNumPoolNodes (cardanoNodes testnetOptions))
                       allPorts port (cardanoEnableP2P testnetOptions)


    H.writeFile (tmpDir </> node </> "port") (show port)

  H.lbsWriteFile (tmpDir </> "byron.genesis.spec.json")
    . J.encode $ defaultByronGenesisJsonValue

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
    , "--protocol-parameters-file", tmpDir </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tmpDir </> "byron"
    ]

  H.renameFile
    (tmpDir </> "byron.genesis.spec.json")
    (tmpDir </> "byron/genesis.spec.json")

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tmpDir </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key") (tmpDir </> "node-bft" <> show @Int n </> "byron/delegate.key")
    H.createFileLink (tmpDir </> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json") (tmpDir </> "node-bft" <> show @Int n </> "byron/delegate.cert")
  let
    forallBftNodesMkFile x = forM [1..numBftNodes] (fakeItH . x)

  genesisKeys <- forallBftNodesMkFile $ \n -> "byron/genesis-keys.00" <> show @Int (n - 1) <> ".json"
  delegationCerts <- forallBftNodesMkFile $ \n -> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
  delegationKeys <- forallBftNodesMkFile $ \n -> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json"

  return (genesisKeys, delegationCerts, delegationKeys)


{-
cardanoTestnetByronGenesisExpenditure creates an transaction `tx0.tx'
TODO: a test that transmits tx0.tx and checks that it gets accepted.
-}
cardanoTestnetByronGenesisExpenditure
  :: CardanoTestnetOptions
  -> H.Conf
  -> [File ByronKey]
  -> H.Integration ()
cardanoTestnetByronGenesisExpenditure testnetOptions H.Conf {..} genesisKeys = do
  let tmpDir = tempAbsPath
  let numBftNodes = cardanoNumBftNodes $ cardanoNodes testnetOptions

  -- Create keys and addresses to withdraw the initial UTxO into
  byronAddress0:_ <- forM [1 .. numBftNodes] $ \n -> do
    skey <- cliKeyGen tmpDir $ "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
    cliSigningKeyAddress tmpDir testnetMagic skey $ "byron/address-00" <> show @Int (n - 1)

  byronGensisAddress0:_ <- forM (zip [1 .. numBftNodes] genesisKeys) $ \(n,key) -> do
    cliSigningKeyAddress tmpDir testnetMagic
      key
      ("byron/genesis-address-00" <> show @Int (n - 1))

  richAddrFrom <- S.firstLine <$> H.readFile (tmpDir </> "byron/genesis-address-000")
  txAddr <- S.firstLine <$> H.readFile (unFile byronAddress0)
  let
      maxByronSupply = 10020000000
      fundsPerGenesisAddress = maxByronSupply `div` numBftNodes
      fundsPerByronAddress = fundsPerGenesisAddress - 100000000

  -- Create Byron address that moves funds out of the genesis UTxO into a regular
  -- address.
  execCli_
    [ "issue-genesis-utxo-expenditure"
    , "--genesis-json", tempAbsPath </> "byron/genesis.json"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx", tempAbsPath </> "tx0.tx"
    , "--wallet-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--rich-addr-from", richAddrFrom
    , "--txout", show @(String, Int) (txAddr, fundsPerByronAddress)
    ]

{-
cardanoTestnetByronGovernance creates two update-proposals and two update-votes.
TODO: There should be a test that transmits thoese proposals and checks
that they get activated.
-}
cardanoTestnetByronGovernance
  :: Int
  -> Int
  -> FilePath
  -> H.Integration ()
cardanoTestnetByronGovernance numBftNodes testnetMagic tmpDir = do
  let bftNodesN = [1 .. numBftNodes]
  createByronUpdateProposal
    testnetMagic
    (tmpDir </> "byron/delegate-keys.000.key")
    (tmpDir </> "update-proposal")
    1

  forM_ bftNodesN $ \n -> do
    createByronUpdateProposalVote
      testnetMagic
      (tmpDir </> "update-proposal")
      (tmpDir </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key")
      (tmpDir </> "update-vote.00" <> show @Int (n - 1))

  createByronUpdateProposal
    testnetMagic
    (tmpDir </> "byron/delegate-keys.000.key")
    (tmpDir </> "update-proposal-1")
    2

  forM_ bftNodesN $ \n ->
    createByronUpdateProposalVote
      testnetMagic
      (tmpDir </> "update-proposal-1")
      (tmpDir </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key")
      (tmpDir </> "update-vote-1.00" <> show @Int (n - 1))

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tmpDir </> "byron"

cardanoTestnetShelleyGenesis
  :: FilePath
  -> TmpPath
  -> TestnetMagic
  -> CardanoTestnetOptions
  -> DTC.UTCTime
  -> H.Integration ()
cardanoTestnetShelleyGenesis base (TmpPath tmpDir) testnetMagic testnetOptions startTime = do
  let numBftNodes = cardanoNumBftNodes $ cardanoNodes testnetOptions
      maxShelleySupply = 1000000000000

  -- Set up our template
  H.createDirectoryIfMissing $ tmpDir </> "shelley"

  -- TODO: This is fragile, we should be passing in all necessary
  -- configuration files.
  let sourceAlonzoGenesisSpecFile = base </> "cardano-cli/test/data/golden/alonzo/genesis.alonzo.spec.json" -- use of base in this module
  alonzoSpecFile <- H.noteTempFile tmpDir "shelley/genesis.alonzo.spec.json"
  liftIO $ IO.copyFile sourceAlonzoGenesisSpecFile alonzoSpecFile

  let sourceConwayGenesisSpecFile = base </> "cardano-cli/test/data/golden/conway/genesis.conway.spec.json" -- use of base in this module
  conwaySpecFile <- H.noteTempFile tmpDir "shelley/genesis.conway.spec.json"
  liftIO $ IO.copyFile sourceConwayGenesisSpecFile conwaySpecFile

  execCli_
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tmpDir </> "shelley"
    , "--start-time", formatIso8601 startTime
    ]

  -- Then edit the genesis.spec.json ...

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJsonFile (tmpDir </> "shelley/genesis.spec.json") . J.rewriteObject
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
    , "--genesis-dir", tmpDir </> "shelley"
    , "--gen-genesis-keys", show @Int numBftNodes
    , "--start-time", formatIso8601 startTime
    , "--gen-utxo-keys", show (cardanoNumBftNodes $ cardanoNodes testnetOptions)
    ]

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tmpDir </> "shelley"

  H.rewriteJsonFile (tmpDir </> "shelley/genesis.json") . J.rewriteObject
    $ flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject (HM.insert "major" (J.toJSON @Int 6))
          )
        )
      )
    . HM.insert "updateQuorum" (J.toJSON @Int 2)

  -- Generated shelley/genesis.json
  H.cat $ tmpDir </> "shelley/genesis.json"

  -- Generated alonzo/genesis.json
  --TODO: rationalise the naming convention on these genesis json files.
  H.cat $ tmpDir </> "shelley/genesis.alonzo.json"
  H.cat $ tmpDir </> "shelley/genesis.conway.json"

cardanoTestnetOperatorKeys
  :: TmpDir
  -> [String]
  -> H.Integration [(File (Operator VKey), File (Operator SKey), File OperatorCounter)]
cardanoTestnetOperatorKeys tmpDir poolNodeNames
  = forM poolNodeNames $ \node -> cliNodeKeyGen tmpDir
      (node </> "shelley/operator.vkey")
      (node </> "shelley/operator.skey")
      (node </> "shelley/operator.counter")

cardanoTestnetVrfKeys
  :: TmpDir
  -> [String]
  -> H.Integration [(File (Vrf VKey), File (Vrf SKey))]
cardanoTestnetVrfKeys tmpDir poolNodeNames
  = forM poolNodeNames $
      \node -> cliNodeKeyGenVrf tmpDir $ KeyNames (node </> "shelley/vrf.vkey") (node </> "shelley/vrf.skey")

cardanoTestnetPoolKeys
 :: TmpDir
 -> [Int]
 -> [(File (Operator VKey) , File (Operator SKey), File OperatorCounter)]
 -> [(File (Vrf VKey), File (Vrf SKey))]
 ->  H.Integration [PoolNodeKeys]
cardanoTestnetPoolKeys tmpDir poolNodesN operatorKeys vrfKeys = do
  forM (zip3 poolNodesN operatorKeys vrfKeys) $ \(i, opKey, vrfKey) -> do
    let node = "node-pool" <> show @Int i

    poolNodeKeysColdVkey <- H.note $ tmpDir </> node <> "/shelley/operator.vkey"
    poolNodeKeysColdSkey <- H.note $ tmpDir </> node <> "/shelley/operator.skey"
    poolNodeKeysVrfVkey <- H.note $ tmpDir </> node </> "shelley/vrf.vkey"
    poolNodeKeysVrfSkey <- H.note $ tmpDir </> node </> "shelley/vrf.skey"

    return PoolNodeKeys
      { TR.poolNodeKeysColdVkey
      , TR.poolNodeKeysColdSkey
      , poolNodeKeysOperator = opKey
      , TR.poolNodeKeysVrfVkey
      , TR.poolNodeKeysVrfSkey
      , TR.poolNodeKeysVrf = vrfKey
      }

cardanoTestnetDelegationKeyLinks
  :: TmpDir
  -> [Int]
  -> H.Integration ()
cardanoTestnetDelegationKeyLinks tmpDir bftNodesN = do
  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tmpDir </> "shelley/delegate-keys/delegate" <> show @Int n <> ".skey") (tmpDir </> "node-bft" <> show @Int n </> "shelley/operator.skey")
    H.createFileLink (tmpDir </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vkey") (tmpDir </> "node-bft" <> show @Int n </> "shelley/operator.vkey")
    H.createFileLink (tmpDir </> "shelley/delegate-keys/delegate" <> show @Int n <> ".counter") (tmpDir </> "node-bft" <> show @Int n </> "shelley/operator.counter")
    H.createFileLink (tmpDir </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.vkey") (tmpDir </> "node-bft" <> show @Int n </> "shelley/vrf.vkey")
    H.createFileLink (tmpDir </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.skey") (tmpDir </> "node-bft" <> show @Int n </> "shelley/vrf.skey")

cardanoTestnetKesKeys
  :: TmpDir
  -> [String]
  -> H.Integration ()
cardanoTestnetKesKeys tmpDir allNodeNames = do
  -- Make hot keys and for all nodes
  forM_ allNodeNames $ \node -> do
    cliNodeKeyGenKes tmpDir $ KeyNames (node </> "shelley/kes.vkey") (node </> "shelley/kes.skey")

    execCli_
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file",  tmpDir </> node </> "shelley/kes.vkey"
      , "--cold-signing-key-file",  tmpDir </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tmpDir </> node </> "shelley/operator.counter"
      , "--out-file", tmpDir </> node </> "shelley/node.cert"
      ]

cardanoTestnetPaymentKeys
  :: TmpDir
  -> Int
  -> [Int]
  -> H.Integration [PaymentKeyPair]
cardanoTestnetPaymentKeys tmpDir testnetMagic poolNodesN = do
  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards
  let userAddrs = ("user" <>) . show @Int <$> poolNodesN
      poolAddrs = ("pool-owner" <>) . show @Int <$> poolNodesN
      addrs = userAddrs <> poolAddrs

  H.createDirectoryIfMissing $ tmpDir </> "addresses"

  forM addrs $ \addr -> do
    let paymentSKey = tmpDir </> "addresses/" <> addr <> ".skey"
    let paymentVKey = tmpDir </> "addresses/" <> addr <> ".vkey"

    -- Payment address keys
    -- TODO !

    cliAddressKeyGen tmpDir $ KeyNames ("addresses" </> addr <> ".vkey") ("addresses" </> addr <> ".skey")
    cliAddressKeyGen tmpDir $ KeyNames "shelley/utxo-keys/utxo2.vkey" "shelley/utxo-keys/utxo2.skey"
    cliStakeAddressKeyGen tmpDir $ KeyNames ("addresses" </> addr <> "-stake.vkey") ("addresses" </> addr <> "-stake.skey")
    cliStakeAddressKeyGen tmpDir $ KeyNames "shelley/utxo-keys/utxo-stake.vkey" "shelley/utxo-keys/utxo-stake.skey"
    cliStakeAddressKeyGen tmpDir $ KeyNames "shelley/utxo-keys/utxo2-stake.vkey" "shelley/utxo-keys/utxo2-stake.skey"

    -- Payment addresses
    execCli_
      [ "address", "build"
      , "--payment-verification-key-file", tmpDir </> "addresses/" <> addr <> ".vkey"
      , "--stake-verification-key-file", tmpDir </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tmpDir </> "addresses/" <> addr <> ".addr"
      ]

    -- Stake addresses
    execCli_
      [ "stake-address", "build"
      , "--stake-verification-key-file", tmpDir </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", tmpDir </> "addresses/" <> addr <> "-stake.addr"
      ]

    -- Stake addresses registration certs
    execCli_
      [ "stake-address", "registration-certificate"
      , "--stake-verification-key-file", tmpDir </> "addresses/" <> addr <> "-stake.vkey"
      , "--out-file", tmpDir </> "addresses/" <> addr <> "-stake.reg.cert"
      ]

    pure $ PaymentKeyPair
      { paymentSKey
      , paymentVKey
      }

cardanoTestnetPoolCerts
  :: TmpDir
  -> TestnetMagic
  -> [Int]
  -> H.Integration ()
cardanoTestnetPoolCerts tmpDir testnetMagic poolNodesN = do
  let poolNodeNames = ("node-pool" <>) . show @Int <$> poolNodesN
      maxShelleySupply = 1000000000000

  -- user N will delegate to pool N
  forM_ poolNodesN $ \n -> do
    -- Stake address delegation certs
    execCli_
      [ "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tmpDir </> "addresses/user" <> show @Int n <> "-stake.vkey"
      , "--cold-verification-key-file", tmpDir </> "node-pool" <> show @Int n </> "shelley/operator.vkey"
      , "--out-file", tmpDir </> "addresses/user" <> show @Int n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tmpDir </> "addresses/pool-owner" <> show @Int n <> "-stake.vkey") (tmpDir </> "node-pool" <> show @Int n </> "owner.vkey")
    H.createFileLink (tmpDir </> "addresses/pool-owner" <> show @Int n <> "-stake.skey") (tmpDir </> "node-pool" <> show @Int n </> "owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address registration certs, and stake address delegation certs
  H.noteEachM_ . H.listDirectory $ tmpDir </> "addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodeNames $ \node -> do
    H.execCli
      [ "stake-pool", "registration-certificate"
      , "--testnet-magic", show @Int testnetMagic
      , "--pool-pledge", "0", "--pool-cost", "0", "--pool-margin", "0"
      , "--cold-verification-key-file", tmpDir </> node </> "shelley/operator.vkey"
      , "--vrf-verification-key-file", tmpDir </> node </> "shelley/vrf.vkey"
      , "--reward-account-verification-key-file", tmpDir </> node </> "owner.vkey"
      , "--pool-owner-stake-verification-key-file", tmpDir </> node </> "owner.vkey"
      , "--out-file", tmpDir </> node </> "registration.cert"
      ]

  -- Generated stake pool registration certs
  forM_ poolNodeNames $ \node -> H.assertIO . IO.doesFileExist $ tmpDir </> node </> "registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  do
    -- We'll transfer all the funds to the user1, which delegates to pool1
    -- We'll register certs to:
    --  1. register the pool-owner1 stake address
    --  2. register the stake pool 1
    --  3. register the user1 stake address
    --  4. delegate from the user1 stake address to the stake pool
    txIn <- H.noteShow . S.strip =<< createShelleyGenesisInitialTxIn testnetMagic (tmpDir </> "shelley/utxo-keys/utxo1.vkey")

    H.note_ txIn

    user1Addr <- H.readFile $ tmpDir </> "addresses/user1.addr"

    execCli_
      [ "transaction", "build-raw"
      , "--invalid-hereafter", "1000"
      , "--fee", "0"
      , "--tx-in", txIn
      , "--tx-out",  user1Addr <> "+" <> show @Int maxShelleySupply
      , "--certificate-file", tmpDir </> "addresses/pool-owner1-stake.reg.cert"
      , "--certificate-file", tmpDir </> "node-pool1/registration.cert"
      , "--certificate-file", tmpDir </> "addresses/user1-stake.reg.cert"
      , "--certificate-file", tmpDir </> "addresses/user1-stake.deleg.cert"
      , "--out-file", tmpDir </> "tx1.txbody"
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
    , "--signing-key-file", tmpDir </> "shelley/utxo-keys/utxo1.skey"
    , "--signing-key-file", tmpDir </> "addresses/user1-stake.skey"
    , "--signing-key-file", tmpDir </> "node-pool1/owner.skey"
    , "--signing-key-file", tmpDir </> "node-pool1/shelley/operator.skey"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", tmpDir </> "tx1.txbody"
    , "--out-file", tmpDir </> "tx1.tx"             -------tx1.tx never used !
    ]

  -- Generated a signed 'do it all' transaction:
  H.assertIO . IO.doesFileExist $ tmpDir </> "tx1.tx"

cardanoTestnetLaunchBftNodes
  :: TmpPath
  -> CardanoTestnetOptions
  -> H.Integration [ NodeRuntime ]
cardanoTestnetLaunchBftNodes tp@(TmpPath tmpDir) testnetOptions = do
  let numBftNodes = cardanoNumBftNodes $ cardanoNodes testnetOptions
      bftNodesN = [1 .. numBftNodes]
      bftNodeNames = ("node-bft" <>) . show @Int <$> bftNodesN

  let bftNodeNameAndOpts = L.zip bftNodeNames (cardanoBftNodes $ cardanoNodes testnetOptions)
  forM bftNodeNameAndOpts $ \(node, nodeOpts) -> do
    startNode tp node
      ([ "run"
        , "--config",  tmpDir </> "configuration.yaml"
        , "--topology",  tmpDir </> node </> "topology.json"
        , "--database-path", tmpDir </> node </> "db"
        , "--shelley-kes-key", tmpDir </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tmpDir </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tmpDir </> node </> "shelley/node.cert"
        , "--delegation-certificate",  tmpDir </> node </> "byron/delegate.cert"
        , "--signing-key", tmpDir </> node </> "byron/delegate.key"
        ] <> extraBftNodeCliArgs nodeOpts)

cardanoTestnetLaunchPoolNodes
  :: TmpPath
  -> [String]
  -> H.Integration [ NodeRuntime ]
cardanoTestnetLaunchPoolNodes tp@(TmpPath tmpDir) poolNodeNames = do
  forM poolNodeNames $ \node -> do
    startNode tp node
      [ "run"
      , "--config", tmpDir </> "configuration.yaml"
      , "--topology", tmpDir </> node </> "topology.json"
      , "--database-path", tmpDir </> node </> "db"
      , "--shelley-kes-key", tmpDir </> node </> "shelley/kes.skey"
      , "--shelley-vrf-key", tmpDir </> node </> "shelley/vrf.skey"
      , "--shelley-operational-certificate", tmpDir </> node </> "shelley/node.cert"
      , "--host-addr", ifaceAddress
      ]

cardanoTestnetWaitStartup
  :: H.Conf
  -> NodeLoggingFormat
  -> [String]
  -> H.Integration ()
cardanoTestnetWaitStartup H.Conf {..} loggingFormat allNodeNames = do
  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ allNodeNames $ \node -> do
    sprocket <- H.noteShow $ makeSprocket (TmpPath tempAbsPath) node
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.byDeadlineM 10 deadline "Failed to connect to node socket" $ H.assertM $ H.doesSprocketExist sprocket

  forM_ allNodeNames $ \node -> do
    nodeStdoutFile <- H.noteTempFile (getLogDir $ TmpPath tempAbsPath) $ node <> ".stdout.log"
    H.assertChainExtended deadline loggingFormat nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

getByronGenesisHash :: (H.MonadTest m, MonadIO m) => FilePath -> m J.Value
getByronGenesisHash path = do
  e <- runExceptT $ readGenesisData path
  (_, genesisHash) <- H.leftFail e
  let genesisHash' = J.toJSON $ unGenesisHash genesisHash
  pure genesisHash'

getShelleyGenesisHash :: (H.MonadTest m, MonadIO m) => FilePath -> m J.Value
getShelleyGenesisHash path = do
  content <- liftIO $ BS.readFile path
  let genesisHash = Cardano.Crypto.Hash.Class.hashWith id content :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 BS.ByteString
  pure $ J.toJSON genesisHash
