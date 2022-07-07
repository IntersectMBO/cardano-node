{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Shelley
  ( TestnetOptions(..)
  , defaultTestnetOptions

  , testnet
  , hprop_testnet
  , hprop_testnet_pause
  ) where

import           Control.Monad (Monad(..), forever, forM_, void, (=<<), when)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Resource (MonadResource(liftResourceT), resourceForkIO)
import           Data.Aeson (Value, ToJSON(toJSON))
import           Data.ByteString.Lazy (ByteString)
import           Data.Eq (Eq)
import           Data.Function (($), (.), flip)
import           Data.Functor (Functor(fmap), (<$>), (<&>))
import           Data.Int (Int)
import           Data.List ((\\))
import           Data.Maybe (Maybe(Nothing, Just), fromJust)
import           Data.Ord (Ord((<=)))
import           Data.Semigroup (Semigroup((<>)))
import           Data.String (String, fromString)
import           Data.Time.Clock (UTCTime)
import           GHC.Float (Double)
import           GHC.Real (fromIntegral)
import           Hedgehog.Extras.Stock.Aeson (rewriteObject)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket(..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter(..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint(..))
import           Prelude (Bool(..), Integer, (-))
import           System.FilePath.Posix ((</>))
import           Text.Show (Show(show))

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import qualified Control.Concurrent as IO
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Stock.Time as DTC
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

data TestnetOptions = TestnetOptions
  { numPraosNodes :: Int
  , numPoolNodes :: Int
  , activeSlotsCoeff :: Double
  , securityParam :: Int
  , epochLength :: Int
  , slotLength :: Double
  , maxLovelaceSupply :: Integer
  , enableP2P :: Bool
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { numPraosNodes = 2
  , numPoolNodes = 1
  , activeSlotsCoeff = 0.1
  , securityParam = 10
  , epochLength = 1000
  , slotLength = 0.2
  , maxLovelaceSupply = 1000000000
  , enableP2P = False
  }

-- TODO: We need to refactor this to directly check the parsed configuration
-- and fail with a suitable error message.
-- | Rewrite a line in the configuration file
rewriteConfiguration :: Bool -> String -> String
rewriteConfiguration True "EnableP2P: False" = "EnableP2P: True"
rewriteConfiguration False "EnableP2P: True" = "EnableP2P: False"
rewriteConfiguration _ s                     = s

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

rewriteGenesisSpec :: TestnetOptions -> UTCTime -> Value -> Value
rewriteGenesisSpec testnetOptions startTime =
  rewriteObject
    $ HM.insert "activeSlotsCoeff" (J.toJSON @Double (activeSlotsCoeff testnetOptions))
    . HM.insert "securityParam" (J.toJSON @Int (securityParam testnetOptions))
    . HM.insert "epochLength" (J.toJSON @Int (epochLength testnetOptions))
    . HM.insert "slotLength" (J.toJSON @Double (slotLength testnetOptions))
    . HM.insert "maxLovelaceSupply" (J.toJSON @Integer (maxLovelaceSupply testnetOptions))
    . HM.insert "systemStart" (J.toJSON @String (DTC.formatIso8601 startTime))
    . flip HM.adjust "protocolParams"
      ( rewriteObject (HM.insert "decentralisationParam" (toJSON @Double 0.7))
      )

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


mkTopologyConfig :: Int -> [Int] -> Int
                 -> Bool -- ^ if true use p2p topology configuration
                 -> ByteString
mkTopologyConfig numPraosNodes allPorts port False = J.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numPraosNodes - 1)
        | peerPort <- allPorts \\ [port]
        ]
mkTopologyConfig numPraosNodes allPorts port True = J.encode topologyP2P
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
                                  (numPraosNodes - 1)
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (P2P.UseLedger DontUseLedger)

testnet :: TestnetOptions -> H.Conf -> H.Integration [String]
testnet testnetOptions H.Conf {..} = do
  void $ H.note OS.os

  let praosNodesN = show @Int <$> [1 .. numPraosNodes testnetOptions]
  let praosNodes = ("node-praos" <>) <$> praosNodesN
  let poolNodesN = show @Int <$> [1 .. numPoolNodes testnetOptions]
  let poolNodes = ("node-pool" <>) <$> poolNodesN
  let allNodes = praosNodes <> poolNodes :: [String]
  let numPraosNodes = L.length allNodes :: Int
  let userPoolN = poolNodesN -- User N will delegate to pool N

  allPorts <- H.noteShowIO $ IO.allocateRandomPorts numPraosNodes
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodes allPorts))
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  let userAddrs = ("user" <>) <$> userPoolN
  let poolAddrs = ("pool-owner" <>) <$> poolNodesN
  let addrs = userAddrs <> poolAddrs

  -- TODO: This is fragile, we should be passing in all necessary
  -- configuration files.
  let sourceAlonzoGenesisSpecFile = base </> "cardano-cli/test/data/golden/alonzo/genesis.alonzo.spec.json"
  alonzoSpecFile <- H.noteTempFile tempAbsPath "genesis.alonzo.spec.json"
  liftIO $ IO.copyFile sourceAlonzoGenesisSpecFile alonzoSpecFile

  -- Set up our template
  void $ H.execCli
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tempAbsPath
    , "--start-time", DTC.formatIso8601 startTime
    ]

  -- Then edit the genesis.spec.json ...

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJsonFile (tempAbsPath </> "genesis.spec.json") (rewriteGenesisSpec testnetOptions startTime)
  H.rewriteJsonFile (tempAbsPath </> "genesis.json"     ) (rewriteGenesisSpec testnetOptions startTime)

  H.assertIsJsonFile $ tempAbsPath </> "genesis.spec.json"

  -- Now generate for real
  void $ H.execCli
    [ "genesis", "create"
    , "--testnet-magic", show @Int testnetMagic
    , "--genesis-dir", tempAbsPath
    , "--gen-genesis-keys", show numPraosNodes
    , "--gen-utxo-keys", show @Int (numPoolNodes testnetOptions)
    , "--start-time", DTC.formatIso8601 startTime
    ]

  forM_ allNodes $ \p -> H.createDirectoryIfMissing $ tempAbsPath </> p

  -- Make the pool operator cold keys
  -- This was done already for the BFT nodes as part of the genesis creation
  forM_ poolNodes $ \n -> do
    void $ H.execCli
      [ "node", "key-gen"
      , "--cold-verification-key-file", tempAbsPath </> n </> "operator.vkey"
      , "--cold-signing-key-file", tempAbsPath </> n </> "operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> n </> "operator.counter"
      ]

    void $ H.execCli
      [ "node", "key-gen-VRF"
      , "--verification-key-file", tempAbsPath </> n </> "vrf.vkey"
      , "--signing-key-file", tempAbsPath </> n </> "vrf.skey"
      ]
  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ praosNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "delegate-keys/delegate" <> n <> ".skey") (tempAbsPath </> "node-praos" <> n </> "operator.skey")
    H.createFileLink (tempAbsPath </> "delegate-keys/delegate" <> n <> ".vkey") (tempAbsPath </> "node-praos" <> n </> "operator.vkey")
    H.createFileLink (tempAbsPath </> "delegate-keys/delegate" <> n <> ".counter") (tempAbsPath </> "node-praos" <> n </> "operator.counter")
    H.createFileLink (tempAbsPath </> "delegate-keys/delegate" <> n <> ".vrf.vkey") (tempAbsPath </> "node-praos" <> n </> "vrf.vkey")
    H.createFileLink (tempAbsPath </> "delegate-keys/delegate" <> n <> ".vrf.skey") (tempAbsPath </> "node-praos" <> n </> "vrf.skey")

  --  Make hot keys and for all nodes
  forM_ allNodes $ \node -> do
    void $ H.execCli
      [ "node", "key-gen-KES"
      , "--verification-key-file", tempAbsPath </> node </> "kes.vkey"
      , "--signing-key-file", tempAbsPath </> node </> "kes.skey"
      ]

    void $ H.execCli
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", tempAbsPath </> node </> "kes.vkey"
      , "--cold-signing-key-file", tempAbsPath </> node </> "operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> node </> "operator.counter"
      , "--out-file", tempAbsPath </> node </> "node.cert"
      ]

  -- Make topology files
  forM_ allNodes $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tempAbsPath </> node </> "topology.json") $
      mkTopologyConfig numPraosNodes allPorts port (enableP2P testnetOptions)

    H.writeFile (tempAbsPath </> node </> "port") (show port)

  -- Generated node operator keys (cold, hot) and operational certs
  forM_ allNodes $ \n -> H.noteShowM_ . H.listDirectory $ tempAbsPath </> n

  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards
  H.createDirectoryIfMissing $ tempAbsPath </> "addresses"

  forM_ addrs $ \addr -> do
    -- Payment address keys
    void $ H.execCli
      [ "address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "addresses/" <> addr <> ".vkey"
      , "--signing-key-file", tempAbsPath </> "addresses/" <> addr <> ".skey"
      ]

    -- Stake address keys
    void $ H.execCli
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--signing-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.skey"
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

  forM_ userPoolN $ \n -> do
    -- Stake address delegation certs
    void $ H.execCli
      [ "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/user" <> n <> "-stake.vkey"
      , "--cold-verification-key-file", tempAbsPath </> "node-pool" <> n </> "operator.vkey"
      , "--out-file", tempAbsPath </> "addresses/user" <> n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> n <> "-stake.vkey") (tempAbsPath </> "node-pool" <> n </> "owner.vkey")
    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> n <> "-stake.skey") (tempAbsPath </> "node-pool" <> n </> "owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address registration certs, and stake address delegation certs
  H.noteShowM_ . H.listDirectory $ tempAbsPath </> "addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodes $ \node -> do
    void $ H.execCli
      [ "stake-pool", "registration-certificate"
      , "--testnet-magic", show @Int testnetMagic
      , "--pool-pledge", "0"
      , "--pool-cost", "0"
      , "--pool-margin", "0"
      , "--cold-verification-key-file", tempAbsPath </> node </> "operator.vkey"
      , "--vrf-verification-key-file", tempAbsPath </> node </> "vrf.vkey"
      , "--reward-account-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--pool-owner-stake-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--out-file", tempAbsPath </> node </> "registration.cert"
      ]

  -- Generated stake pool registration certs:
  forM_ poolNodes $ \node -> H.assertIO . IO.doesFileExist $ tempAbsPath </> node </> "registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  forM_ userPoolN $ \n -> do
    -- We'll transfer all the funds to the user n, which delegates to pool n
    -- We'll register certs to:
    --  1. register the pool-owner n stake address
    --  2. register the stake pool n
    --  3. register the usern stake address
    --  4. delegate from the usern stake address to the stake pool
    genesisTxinResult <- H.noteShowM $ S.strip <$> H.execCli
      [ "genesis", "initial-txin"
      , "--testnet-magic", show @Int testnetMagic
      , "--verification-key-file", tempAbsPath </> "utxo-keys/utxo" <> n <> ".vkey"
      ]

    userNAddr <- H.readFile $ tempAbsPath </> "addresses/user" <> n <> ".addr"

    void $ H.execCli
      [ "transaction", "build-raw"
      , "--invalid-hereafter", "1000"
      , "--fee", "0"
      , "--tx-in", genesisTxinResult
      , "--tx-out", userNAddr <> "+" <> show @Integer (maxLovelaceSupply testnetOptions)
      , "--certificate-file", tempAbsPath </> "addresses/pool-owner" <> n <> "-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "node-pool" <> n <> "/registration.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user" <> n <> "-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user" <> n <> "-stake.deleg.cert"
      , "--out-file", tempAbsPath </> "tx" <> n <> ".txbody"
      ]

    -- So we'll need to sign this with a bunch of keys:
    -- 1. the initial utxo spending key, for the funds
    -- 2. the user n stake address key, due to the delegation cert
    -- 3. the pool n owner key, due to the pool registration cert
    -- 3. the pool n operator key, due to the pool registration cert

    void $ H.execCli
      [ "transaction", "sign"
      , "--signing-key-file", tempAbsPath </> "utxo-keys/utxo" <> n <> ".skey"
      , "--signing-key-file", tempAbsPath </> "addresses/user" <> n <> "-stake.skey"
      , "--signing-key-file", tempAbsPath </> "node-pool" <> n <> "/owner.skey"
      , "--signing-key-file", tempAbsPath </> "node-pool" <> n <> "/operator.skey"
      , "--testnet-magic", show @Int testnetMagic
      , "--tx-body-file", tempAbsPath </> "tx" <> n <> ".txbody"
      , "--out-file", tempAbsPath </> "tx" <> n <> ".tx"
      ]

    -- Generated a signed 'do it all' transaction:
    H.assertIO . IO.doesFileExist $ tempAbsPath </> "tx" <> n <> ".tx"

  --------------------------------
  -- Launch cluster of three nodes

  H.createDirectoryIfMissing logDir

  H.readFile (base </> "configuration/chairman/shelley-only/configuration.yaml")
    <&> L.unlines . fmap (rewriteConfiguration (enableP2P testnetOptions)) . L.lines
    >>= H.writeFile (tempAbsPath </> "configuration.yaml")

  forM_ allNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- H.readFile $ tempAbsPath </> node </> "port"

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--shelley-kes-key", tempAbsPath </> node </> "kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "vrf.skey"
        , "--shelley-operational-certificate" , tempAbsPath </> node </> "node.cert"
        , "--host-addr", ifaceAddress
        , "--port", portString
        , "--socket-path", IO.sprocketArgumentName sprocket
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

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ allNodes $ \node -> do
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.assertByDeadlineM deadline $ H.doesSprocketExist sprocket

  forM_ allNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertByDeadlineIOCustom "stdout does not contain \"until genesis start time\"" deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile
    H.assertByDeadlineIOCustom "stdout does not contain \"Chain extended\"" deadline $ IO.fileContains "Chain extended, new tip" nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  return allNodes

hprop_testnet :: H.Property
hprop_testnet = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsPath' -> do
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsPath' Nothing

  void . liftResourceT . resourceForkIO . forever . liftIO $ IO.threadDelay 10000000

  void $ testnet defaultTestnetOptions conf

  H.failure -- Intentional failure to force failure report

hprop_testnet_pause :: H.Property
hprop_testnet_pause = H.integration $ do
  void . forever . liftIO $ IO.threadDelay 10000000
