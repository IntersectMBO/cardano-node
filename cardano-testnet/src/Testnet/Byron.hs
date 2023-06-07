{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Byron
  ( createByronGenesis
  , defaultByronProtocolParamsJsonValue
  , defaultYamlHardforkViaConfig
  , defaultYamlConfig
  , testnet
  , TestnetOptions(..)
  , byronDefaultTestnetOptions
  ) where

import           Control.Monad (forM_, void, when)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (ToJSON (..), Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMapAeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor ((<&>))
import           Data.Proxy
import           Data.Scientific
import           Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vector
import           GHC.Stack
import           Hedgehog.Extras.Stock.Aeson (rewriteObject)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           System.FilePath.Posix ((</>))

import           Cardano.Api hiding (Value)

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Tracing.Config
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import           Hedgehog.Internal.Property (MonadTest)
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Testnet.Conf as H
import qualified Testnet.Util.Process as H
import           Testnet.Util.Process
import qualified Testnet.Util.Runtime as TR
import           Testnet.Utils


{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use head" -}

data TestnetOptions = TestnetOptions
  { numBftNodes :: Int
  , slotDuration :: Int
  , securityParam :: Int
  , nPoorAddresses :: Int
  , testnetMagic :: Int
  , totalBalance :: Int
  , enableP2P :: Bool
  } deriving (Eq, Show)

byronDefaultTestnetOptions :: TestnetOptions
byronDefaultTestnetOptions = TestnetOptions
  { numBftNodes = 3
  , slotDuration = 2000
  , securityParam = 10
  , testnetMagic = 42
  , nPoorAddresses = 128
  , totalBalance = 8000000000000000
  , enableP2P = False
  }

-- TODO: We should not abuse the byron testnet options for genesis creation.
-- This will lead to confusion. We should create a separate type for genesis creation.
-- | Creates a default Byron genesis. This is required for any testnet, predominantly because
-- we inject our ADA supply into our testnet via the Byron genesis.
createByronGenesis
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int
  -> UTCTime
  -> TestnetOptions
  -> String
  -> String
  -> m ()
createByronGenesis testnetMagic' startTime testnetOptions pParamFp genOutputDir =
  withFrozenCallStack $ execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show testnetMagic'
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show (securityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", pParamFp
    , "--genesis-output-dir", genOutputDir
    ]

rewriteParams :: TestnetOptions -> Value -> Value
rewriteParams testnetOptions = rewriteObject
  $ HM.insert "slotDuration" (J.toJSON @String (show @Int (slotDuration testnetOptions)))

mkTopologyConfig :: Int -> Int -> [Int]
                 -> Bool -- ^ if true use p2p topology configuration
                 -> ByteString
mkTopologyConfig i numBftNodes' allPorts False = J.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        $ flip fmap ([0 .. numBftNodes' - 1] L.\\ [i])
        $ \j -> NonP2P.RemoteAddress "127.0.0.1"
                                    (fromIntegral $ allPorts L.!! j)
                                    1
mkTopologyConfig i numBftNodes' allPorts True = J.encode topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        (flip fmap ([0 .. numBftNodes' - 1] L.\\ [i])
        $ \j -> RelayAccessAddress "127.0.0.1"
                            (fromIntegral $ allPorts L.!! j)
        )
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (numBftNodes' - 1)
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (P2P.UseLedger DontUseLedger)


testnet :: TestnetOptions -> H.Conf -> H.Integration [String]
testnet testnetOptions conf = do
  void $ H.note OS.os
  let tNetMagic = testnetMagic testnetOptions
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 15 currentTime -- 15 seconds into the future
  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (numBftNodes testnetOptions)
  let tempAbsPath' = TR.unTmpAbsPath $ H.tempAbsPath conf
      sockDir = TR.makeSocketDir $ H.tempAbsPath conf
      tempBaseAbsPath' = TR.makeTmpBaseAbsPath $ H.tempAbsPath conf
      logDir = TR.makeLogDir $ H.tempAbsPath conf


  H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
    . J.encode $ defaultByronProtocolParamsJsonValue

  H.copyRewriteJsonFile
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "protocol-params.json")
    (rewriteParams testnetOptions)

  -- Generate keys
  void $ H.execCli
    [ "byron"
    , "genesis"
    , "genesis"
    , "--genesis-output-dir", tempAbsPath' </> "genesis"
    , "--start-time", showUTCTimeSeconds startTime
    , "--protocol-parameters-file", tempAbsPath' </> "protocol-params.json"
    , "--k", show @Int (securityParam testnetOptions)
    , "--protocol-magic", show @Int tNetMagic
    , "--n-poor-addresses", show @Int (nPoorAddresses testnetOptions)
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--avvm-entry-count", "128"
    , "--avvm-entry-balance", "10000000000000"
    , "--delegate-share", "0.9"
    , "--secret-seed", "2718281828"
    ]

  H.writeFile (tempAbsPath' </> "genesis/GENHASH") . S.lastLine =<< H.execCli
    [ "print-genesis-hash"
    , "--genesis-json"
    , tempAbsPath' </> "genesis/genesis.json"
    ]

  let nodeIndexes = [0..numBftNodes testnetOptions - 1]
  let allNodes = fmap (\i -> "node-" <> show @Int i) nodeIndexes

  H.createDirectoryIfMissing_ logDir


  -- Launch cluster of three nodes in P2P Mode
  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    let dbDir = TR.makeDbDir i (TR.TmpAbsolutePath tempAbsPath')
    H.createDirectoryIfMissing_ dbDir
    nodeStdoutFile <- H.noteTempFile tempAbsPath' $ "cardano-node-" <> si <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile tempAbsPath' $ "cardano-node-" <> si <> ".stderr.log"
    sprocket <- H.noteShow $ TR.makeSprocket (TR.TmpAbsolutePath tempAbsPath') $ "node-" <> si
    portString <- H.note $ show @Int (allPorts L.!! i)
    topologyFile <- H.noteShow $ tempAbsPath' </> "topology-node-" <> si <> ".json"
    configFile <- H.noteShow $ tempAbsPath' </> "config-" <> si <> ".yaml"
    signingKeyFile <- H.noteShow $ tempAbsPath' </> "genesis/delegate-keys.00" <> si <> ".key"
    delegationCertificateFile <- H.noteShow $ tempAbsPath' </> "genesis/delegation-cert.00" <> si <> ".json"


    H.lbsWriteFile (tempAbsPath' </> "topology-node-" <> si <> ".json") $
      mkTopologyConfig i (numBftNodes testnetOptions) allPorts (enableP2P testnetOptions)

    byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "genesis/genesis.json"

    let finalYamlConfig :: LBS.ByteString
        finalYamlConfig = J.encode . J.Object
                            $ mconcat [ byronGenesisHash
                                      , defaultYamlHardforkViaConfig $ AnyCardanoEra ByronEra
                                      ]

    H.evalIO $ LBS.writeFile (tempAbsPath' </> "config-" <> si <> ".yaml")  finalYamlConfig

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--database-path", dbDir
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--port", portString
        , "--topology", topologyFile
        , "--config", configFile
        , "--signing-key", signingKeyFile
        , "--delegation-certificate", delegationCertificateFile
        , "--shutdown-ipc", "0"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath'
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath' (sockDir </> "node-" <> si)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    -- TODO: Better error message need to indicate a sprocket was not created
    H.byDeadlineM 10 deadline "Failed to connect to node socket" $ H.assertM $ H.doesSprocketExist sprocket

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    nodeStdoutFile <- H.noteTempFile tempAbsPath' $ "cardano-node-" <> si <> ".stdout.log"
    H.assertByDeadlineIOCustom "stdout does not contain \"until genesis start time\"" deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile

  H.copyFile (tempAbsPath' </> "config-1.yaml") (tempAbsPath' </> "configuration.yaml")

  return allNodes



-- | Configuration value that allows you to hardfork to any Cardano era
-- at epoch 0.
defaultYamlHardforkViaConfig :: AnyCardanoEra -> KeyMapAeson.KeyMap Aeson.Value
defaultYamlHardforkViaConfig era =
  mconcat $ concat
    [ defaultYamlConfig
    , tracers
    , protocolVersions era
    , hardforkViaConfig era
    ]

 where
  -- The protocol version number gets used by block producing nodes as part
  -- of the system for agreeing on and synchronising protocol updates.
  -- NB: We follow the mainnet protocol versions and assume the latest
  -- protocol version for a given era that has had an intraera hardfork.
  protocolVersions :: AnyCardanoEra -> [KeyMapAeson.KeyMap Aeson.Value]
  protocolVersions (AnyCardanoEra era') =
    case era' of
      ByronEra ->
        map (uncurry KeyMapAeson.singleton)
          -- We assume Byron with Ouroboros permissive BFT
          [ ("LastKnownBlockVersion-Major", Aeson.Number 1)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 2)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      AllegraEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 3)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      MaryEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 4)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      AlonzoEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 5)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      BabbageEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 8)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ConwayEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 9)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]

  -- Allows a direct hardfork to an era of your choice via the configuration.
  -- This removes the usual requirement for submitting an update proposal,
  -- waiting for the protocol to change and then restarting the nodes.
  hardforkViaConfig :: AnyCardanoEra -> [KeyMapAeson.KeyMap Aeson.Value]
  hardforkViaConfig (AnyCardanoEra era') =
    case era' of
      ByronEra -> []
      ShelleyEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          ]
      AllegraEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          ]
      MaryEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          ]
      AlonzoEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          ]
      BabbageEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
          ]
      ConwayEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
          , ("TestConwayHardForkAtEpoch", Aeson.Number 0)
          ]


  -- | Various tracers we can turn on or off
  tracers :: [KeyMapAeson.KeyMap Aeson.Value]
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

defaultYamlConfig :: [KeyMapAeson.KeyMap Aeson.Value]
defaultYamlConfig =
  map (uncurry KeyMapAeson.singleton)
    [
    -- The consensus protocol to use
      ("Protocol", "Cardano")

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

    -- Enable peer to peer discovery
    , ("EnableP2P", Aeson.Bool False)

    -- Logging related
    , ("setupScribes", setupScribes)
    , ("rotation", rotationObject)
    , ("defaultScribes", defaultScribes)
    , ("setupBackends", Aeson.Array $ Vector.fromList ["KatipBK"])
    , ("defaultBackends", Aeson.Array $ Vector.fromList ["KatipBK"])
    , ("options", Aeson.object mempty)
    ]

-- | We need a Byron genesis in order to be able to hardfork to the later Shelley based eras.
-- The values here don't matter as the testnet conditions are ultimately determined
-- by the Shelley genesis.
defaultByronProtocolParamsJsonValue :: Value
defaultByronProtocolParamsJsonValue =
  Aeson.object
    [ "heavyDelThd" .= toJSON @String "300000000000"
    , "maxBlockSize" .= toJSON @String "2000000"
    , "maxTxSize" .= toJSON @String "4096"
    , "maxHeaderSize" .= toJSON @String "2000000"
    , "maxProposalSize" .= toJSON @String "700"
    , "mpcThd" .= toJSON @String "20000000000000"
    , "scriptVersion" .= toJSON @Int 0
    , "slotDuration" .= toJSON @String "1000"
    , "softforkRule" .= Aeson.object
      [ "initThd" .= toJSON @String "900000000000000"
      , "minThd" .= toJSON @String "600000000000000"
      , "thdDecrement" .= toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= Aeson.object
      [ "multiplier" .= toJSON @String "43946000000"
      , "summand" .= toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= toJSON @String "18446744073709551615"
    , "updateImplicit" .= toJSON @String "10000"
    , "updateProposalThd" .= toJSON @String "100000000000000"
    , "updateVoteThd" .= toJSON @String "1000000000000"
    ]
