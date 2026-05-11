{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Testnet.Start.Cardano
  ( CardanoTestnetCliOptions(..)
  , NoUserProvidedEnvOptions(..)
  , StartFromEnvOptions(..)
  , TestnetCreationOptions(..)
  , TestnetRuntimeOptions(..)
  , TestnetEnvOptions(..)
  , TestnetNodesWithOptions(..)
  , NodeWithOptions(..)
  , cardanoDefaultTestnetNodesWithOptions

  , TestnetRuntime (..)

  , cardanoTestnet
  , createAndRunTestnet
  , createTestnetEnv
  , getDefaultAlonzoGenesis
  , getDefaultShelleyGenesis
  , readNodesWithOptionsFromEnv
  , retryOnAddressInUseError

  , liftToIntegration
  ) where


import           Cardano.Api
import           Cardano.Api.Byron (GenesisData (..))
import qualified Cardano.Api.Byron as Byron

import           Cardano.Network.Diffusion.Topology (CardanoNetworkTopology)
import           Cardano.Node.Configuration.NodeAddress (PortNumber)
import           Cardano.Prelude (NonEmpty ((:|)), canonicalEncodePretty, readMaybe)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))

import           Prelude hiding (lines)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM, forM_, guard, unless, when)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Exception (IOException)
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (MonadResource, getInternalState)
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as LBS
import           Data.Default.Class ()
import           Data.Either
import           Data.Maybe (mapMaybe)
import           Data.Functor
import           Data.List (sort, stripPrefix, uncons)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.MonoTraversable (Element, MonoFunctor, omap)
import qualified Data.Text as Text
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as DTC
import           GHC.Stack
import qualified System.Directory as IO
import qualified System.Process as Process
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import qualified Testnet.Defaults as Defaults
import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultNodeEnvFile,
                   defaultPortFile, defaultUtxoAddrPath)
import           Testnet.Filepath
import           Testnet.Handlers (interruptNodesOnSigINT)
import           Testnet.Orphans ()
import           Testnet.Process.RunIO (execCli', execCli_, liftIOAnnotated, mkExecConfig)
import           Testnet.Property.Assert (assertExpectedSposInLedgerState)
import           Testnet.Runtime as TR
import           Testnet.Start.Types
import           Testnet.Types as TR hiding (shelleyGenesis)

import qualified Hedgehog.Extras as H
import           Hedgehog.Extras.Stock (sprocketSystemName)
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H
import           Hedgehog.Internal.Property (failException)

import           RIO (MonadUnliftIO, RIO (..), runRIO, throwString, timeout)
import           RIO.Orphans (ResourceMap)
import           RIO.State (put)
import           UnliftIO.Async
import           UnliftIO.Exception (stringException)


liftToIntegration :: HasCallStack => RIO ResourceMap a -> H.Integration a
liftToIntegration r =  do
   rMap <- lift $ lift getInternalState
   catch @_ @SomeException (runRIO rMap r) (withFrozenCallStack $ failException . toException . stringException . displayException)

createTestnetEnv :: ()
  => HasCallStack
  => MonadIO m
  => MonadThrow m
  => MonadFail m
  => TestnetCreationOptions
  -> Conf
  -> m ()
createTestnetEnv
  creationOptions@TestnetCreationOptions
    { creationEra=asbe
    , creationNodes=TestnetNodesWithOptions{optSpoNodes, optRelayNodes}
    }
  Conf
    { genesisHashesPolicy
    , tempAbsPath=TmpAbsolutePath tmpAbsPath
    } = do

  AnyShelleyBasedEra sbe <- pure asbe

  _ <- createSPOGenesisAndFiles
    creationOptions
    (TmpAbsolutePath tmpAbsPath)

  let configurationFile = tmpAbsPath </> defaultConfigFile
  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  config <- case genesisHashesPolicy of
    WithHashes -> createConfigJson (TmpAbsolutePath tmpAbsPath) sbe
    WithoutHashes -> pure $ createConfigJsonNoHash sbe

  liftIOAnnotated . LBS.writeFile configurationFile $ A.encodePretty $ Object config

  let allNodes = NEL.toList optSpoNodes ++ optRelayNodes
      numberedNodes = zip [1..] allNodes
      nodeIds = map fst numberedNodes

  portNumbers <- forM numberedNodes
    (\(i, _nodeOption) -> (i,) <$> H.randomPort testnetDefaultIpv4Address)

  let portNumbersMap = Map.fromList portNumbers

  -- Create network topology, write port files, and write env files for custom binaries
  forM_ numberedNodes $ \(i, nodeOption) -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    liftIOAnnotated $ IO.createDirectoryIfMissing True nodeDataDir

    -- Write port file
    case Map.lookup i portNumbersMap of
      Just port -> liftIOAnnotated $ writeFile (tmpAbsPath </> defaultPortFile i) (show port)
      Nothing -> throwString $ "Port not found for node " <> show i

    producers <- mapM (idToRemoteAddressP2P portNumbersMap) $ NodeId <$> filter (/= i) nodeIds
    let topology = Defaults.defaultP2PTopology producers
    liftIOAnnotated . LBS.writeFile (nodeDataDir </> "topology.json") $ A.encodePretty topology

    -- Write env file for nodes with custom binaries
    forM_ (nodeBin nodeOption) $ \bin -> do
        absBin <- liftIOAnnotated $ IO.makeAbsolute bin
        version <- getNodeVersion absBin
        let envFile = tmpAbsPath </> defaultNodeEnvFile i
            nodeEnv = NodeEnv { nodeBinary = absBin, nodeVersion = version }
        liftIOAnnotated $ Yaml.encodeFile envFile nodeEnv

-- | Starts a number of nodes, as given by the first argument. You can either:
--
-- 1. Pass a value 'UserProvidedNodeOptions filepath' to specify your own node configuration file.
--    In this case, only 1 node will be started (TODO: allow an arbitrary number of nodes to be started)
-- 2. Pass value 'NoUserProvidedData' to leave this function to generate the node configuration file.
--    In this, one SPO node will be started, as well as two relay nodes.
--
-- No matter the scenario above, this function setups a number of credentials and nodes (SPOs and relays), like this:
--
-- > ├── byron-gen-command
-- > │   └── genesis-keys.00{0,1,2}.key
-- > ├── delegate-keys
-- > │   ├── delegate{1,2,3}
-- > │   │   ├── kes.{skey,vkey}
-- > │   │   ├── key.{skey,vkey}
-- > │   │   ├── opcert.{cert,counter}
-- > │   │   └── vrf.{skey,vkey}
-- > │   └── README.md
-- > ├── drep-keys
-- > │   ├── drep{1,2,3}
-- > │   │   └── drep.{skey,vkey}
-- > │   └── README.md
-- > ├── genesis-keys
-- > │   ├── genesis{1,2,3}
-- > │   │   └── key.{skey,vkey}
-- > │   └── README.md
-- > ├── logs
-- > │   ├── node{1,2,3}
-- > │   │   ├── node.pid
-- > |   |   └── {stderr,stdout}.log
-- > │   ├── ledger-epoch-state-diffs.log
-- > │   ├── ledger-epoch-state.log
-- > │   ├── node-20241010121635.log
-- > │   └── node.log -> node-20241010121635.log
-- > ├── node-data
-- > │   ├── node{1,2,3}
-- > │   │   ├── db
-- > │   │   │   └── <node database files>
-- > │   │   ├── port
-- > │   │   └── topology.json
-- > ├── pools-keys
-- > │   ├── pool1
-- > │   │   ├── byron-delegate.key
-- > │   │   ├── byron-delegation.cert
-- > │   │   ├── cold.{skey,vkey}
-- > │   │   ├── kes.{skey,vkey}
-- > │   │   ├── opcert.{cert,counter}
-- > │   │   ├── staking-reward.{skey,vkey}
-- > │   │   └── vrf.{skey,vkey}
-- > │   └── README.md
-- > ├── socket
-- > │   ├── node{1,2,3}
-- > │   │   └── sock
-- > ├── stake-delegators
-- > │   ├── delegator{1,2,3}
-- > │   │   ├── payment.{skey,vkey}
-- > │   │   └── staking.{skey,vkey}
-- > ├── utxo-keys
-- > │   ├── utxo{1,2,3}
-- > │   │   └── utxo.{addr,skey,vkey}
-- > │   └── README.md
-- > ├── {alonzo,byron,conway,shelley}-genesis.json
-- > ├── configuration.json
-- > ├── current-stake-pools.json
-- > └── module
cardanoTestnet
  :: (HasCallStack)
  => MonadUnliftIO m
  => MonadResource m
  => MonadCatch m
  => MonadFail m
  => TestnetNodesWithOptions -- ^ The nodes to start
  -> TestnetRuntimeOptions -- ^ Runtime options
  -> Conf -- ^ Path to the test sandbox
  -> m TestnetRuntime
cardanoTestnet
  TestnetNodesWithOptions{optSpoNodes=cardanoSpoNodes, optRelayNodes=cardanoRelayNodes}
  TestnetRuntimeOptions
    { runtimeEnableNewEpochStateLogging=enableNewEpochStateLogging
    , runtimeEnableRpc=cardanoEnableRpc
    , runtimeKESSource=cardanoKESSource
    }
  Conf
    { tempAbsPath=TmpAbsolutePath tmpAbsPath
    , updateTimestamps
    } = do
  let nPools = NumPools $ NEL.length cardanoSpoNodes
      allNodes = map (True,) (NEL.toList cardanoSpoNodes) ++ map (False,) cardanoRelayNodes
      nodeConfigFile = tmpAbsPath </> defaultConfigFile
      byronGenesisFile = tmpAbsPath </> "byron-genesis.json"
      shelleyGenesisFile = tmpAbsPath </> "shelley-genesis.json"

  sBytes <- liftIOAnnotated (LBS.readFile shelleyGenesisFile)
  shelleyGenesis@ShelleyGenesis{sgNetworkMagic}
    <- case eitherDecode sBytes of
          Right sg -> return sg
          Left err -> throwString $ "Could not decode shelley genesis file: " <> shelleyGenesisFile <> " Error: " <> err
  let testnetMagic :: Int = fromIntegral sgNetworkMagic

  wallets <- forM [1..3] $ \idx -> do
    let utxoKeys@KeyPair{verificationKey} = makePathsAbsolute $ Defaults.defaultUtxoKeys idx
    let paymentAddrFile = tmpAbsPath </> defaultUtxoAddrPath idx

    execCli_
      [ "latest", "address", "build"
      , "--payment-verification-key-file", unFile verificationKey
      , "--testnet-magic", show testnetMagic
      , "--out-file", paymentAddrFile
      ]

    paymentAddr <- liftIOAnnotated $ readFile paymentAddrFile

    pure $ PaymentKeyInfo
      { paymentKeyInfoPair = utxoKeys
      , paymentKeyInfoAddr = Text.pack paymentAddr
      }

  -- Read port numbers from disk (written by createTestnetEnv)
  portNumbers <- forM (zip [1..] allNodes) $ \(i, _) -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
        portPath = tmpAbsPath </> defaultPortFile i
    portStr <- liftIOAnnotated $ readFile portPath
    let port = read portStr :: PortNumber
    let topologyPath = nodeDataDir </> "topology.json"
    tBytes <- liftIOAnnotated $ LBS.readFile topologyPath
    case eitherDecode tBytes of
      Right (abstractTopology :: CardanoNetworkTopology) -> do
        liftIOAnnotated $ LBS.writeFile topologyPath $ encode abstractTopology
      Left e -> do
        -- There can be multiple reasons for why both decodings have failed.
        -- Here we assume, very optimistically, that the user has already
        -- instantiated it with a concrete topology file.
        liftIOAnnotated . putStrLn $ "Could not decode topology file: " <> topologyPath <> ". This may be okay. Reason for decoding failure is:\n" ++ e
    pure (i, port)

  -- If necessary, update the time stamps in Byron and Shelley Genesis files.
  -- This is a QoL feature so that users who edit their configuration files don't
  -- have to manually set up the start times themselves.
  when (updateTimestamps == UpdateTimestamps) $ do
    currentTime <- liftIOAnnotated DTC.getCurrentTime
    let startTime = DTC.addUTCTime (fromIntegral startTimeOffsetSeconds) currentTime

    -- Update start time in Byron genesis file
    eByron <- runExceptT $ Byron.readGenesisData byronGenesisFile
    (byronGenesis', _byronHash) <-
      case eByron of
        Right bg -> return bg
        Left err -> throwString $ "Could not read byron genesis data from file: " <> byronGenesisFile <> " Error: " <> show err
    let byronGenesis = byronGenesis'{gdStartTime = startTime}
    liftIOAnnotated . LBS.writeFile  byronGenesisFile $ canonicalEncodePretty byronGenesis

    -- Update start time in Shelley genesis file (which has been read already)
    let shelleyGenesis' = shelleyGenesis{sgSystemStart = startTime}
    liftIOAnnotated . LBS.writeFile shelleyGenesisFile $ A.encodePretty shelleyGenesis'

  let portNumbersMap = Map.fromList portNumbers

  eTestnetNodes <- forConcurrently (zip [1..] allNodes) $ \(i, (isSpo, nodeWithOptions)) -> do
    port <- case Map.lookup i portNumbersMap of
      Just p -> pure p
      Nothing -> throwString $ "Port not found for node " <> show i
    let nodeName = Defaults.defaultNodeName i
        nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
        nodePoolKeysDir = tmpAbsPath </> Defaults.defaultSpoKeysDir i
    (mKeys, spoNodeCliArgs) <- if not isSpo then pure (Nothing, []) else do
      -- depending on testnet configuration, either start a 'kes-agent' or use a key from disk
      kesSourceCliArg <-
        case cardanoKESSource of
          UseKesKeyFile -> pure ["--shelley-kes-key", nodePoolKeysDir </> "kes.skey"]
          UseKesSocket -> do
            -- wait startTimeOffsetSeconds so that the startTime from shelly-genesis.json is not in the future,
            -- as otherwise we will trigger an underflow in kes-agent with a negative time difference.
            liftIOAnnotated $ threadDelay (startTimeOffsetSeconds * 1_000_000)
            kesAgent <- runExceptT $
              initAndStartKesAgent (TmpAbsolutePath tmpAbsPath) nodeName
                TestnetKesAgentArgs{ tkaaShelleyGenesisFile = shelleyGenesisFile
                                   , tkaaColdVKeyFile = nodePoolKeysDir </> "cold.vkey"
                                   , tkaaColdSKeyFile = nodePoolKeysDir </> "cold.skey"
                                   , tkaaKesVKeyFile = nodePoolKeysDir </> "kes.vkey"
                                   , tkaaOpcertCounterFile = nodePoolKeysDir </> "opcert.counter"
                                   , tkaaOpcertFile = nodePoolKeysDir </> "opcert.cert"
                                   }
            case kesAgent of
              Left e -> do
                -- TODO: fail if could not start KES agent
                liftIOAnnotated . putStrLn $ "Could not start KES agent: " <> show e
                pure ["--shelley-kes-key", nodePoolKeysDir </> "kes.skey"]
              Right (TestnetKesAgent{kesAgentServiceSprocket}) ->
                pure ["--shelley-kes-agent-socket", sprocketSystemName kesAgentServiceSprocket]
      let shelleyCliArgs = [ "--shelley-vrf-key", unFile $ signingKey poolNodeKeysVrf
                           , "--shelley-operational-certificate", nodePoolKeysDir </> "opcert.cert"
                           ]
          byronCliArgs = [ "--byron-delegation-certificate", nodePoolKeysDir </> "byron-delegation.cert"
                         , "--byron-signing-key", nodePoolKeysDir </> "byron-delegate.key"
                         ]
          keys@SpoNodeKeys{poolNodeKeysVrf} = mkTestnetNodeKeyPaths i
      pure (Just keys, kesSourceCliArg <> shelleyCliArgs <> byronCliArgs)

    eRuntime <- runExceptT . retryOnAddressInUseError $
      startNode (TmpAbsolutePath tmpAbsPath) nodeName testnetDefaultIpv4Address port testnetMagic (nodeBin nodeWithOptions) $
        [ "run"
        , "--config", nodeConfigFile
        , "--topology", nodeDataDir </> "topology.json"
        , "--database-path", nodeDataDir </> "db"
        ]
        <> spoNodeCliArgs
        <> nodeExtraCliArgs nodeWithOptions
        <> ["--grpc-enable" | RpcEnabled <- [cardanoEnableRpc]]
    pure $ eRuntime <&> \rt -> rt{poolKeys=mKeys}

  let (failedNodes, testnetNodes') = partitionEithers eTestnetNodes
  unless (null failedNodes) $ do
    throwString $ "Some nodes failed to start:\n" ++ show (vsep $ prettyError <$> failedNodes)

  -- Interrupt cardano nodes when the main process is interrupted
  liftIOAnnotated $ interruptNodesOnSigINT testnetNodes'

  -- Make sure that all nodes are healthy by waiting for a chain extension
  mapConcurrently_ (waitForBlockThrow 45 (File nodeConfigFile)) testnetNodes'

  let runtime = TestnetRuntime
        { configurationFile = File nodeConfigFile
        , shelleyGenesisFile = tmpAbsPath </> Defaults.defaultGenesisFilepath ShelleyEra
        , testnetMagic
        , testnetNodes=testnetNodes'
        , wallets
        , delegators = []
        }

  let tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tmpAbsPath

  node1sprocket <- case uncons $ testnetSprockets runtime of
        Just (sprocket, _) -> pure sprocket
        Nothing            -> throwString "No testnet sprocket available"
  execConfig <- mkExecConfig tempBaseAbsPath node1sprocket testnetMagic

  forM_ wallets $ \wallet -> do

    execCli' execConfig
      [ "latest", "query", "utxo"
      , "--address", Text.unpack $ paymentKeyInfoAddr wallet
      , "--cardano-mode"
      ]

  let stakePoolsFp = tmpAbsPath </> "current-stake-pools.json"

  assertExpectedSposInLedgerState stakePoolsFp nPools execConfig

  when enableNewEpochStateLogging $
    TR.startLedgerNewEpochStateLogging runtime tempBaseAbsPath

  pure runtime
  where
    -- TODO: This should come from the configuration!
    makePathsAbsolute :: (Element a ~ FilePath, MonoFunctor a) => a -> a
    makePathsAbsolute = omap (tmpAbsPath </>)
    mkTestnetNodeKeyPaths :: Int -> SpoNodeKeys
    mkTestnetNodeKeyPaths n = makePathsAbsolute $ Defaults.defaultSpoKeys n

    -- wait for new blocks or throw an exception if there are none in the timeout period
    waitForBlockThrow :: MonadUnliftIO m
                      => MonadCatch m
                      => Int -- ^ timeout in seconds
                      -> NodeConfigFile 'In
                      -> TestnetNode
                      -> m ()
    waitForBlockThrow timeoutSeconds nodeConfigFile node@TestnetNode{nodeName} = do
      result <- timeout (timeoutSeconds * 1_000_000) $
        runExceptT . foldEpochState
          nodeConfigFile
          (nodeSocketPath node)
          QuickValidation
          (EpochNo maxBound)
          minBound
          $ \_ slotNo blockNo -> do
            put slotNo
            pure $ if blockNo >= 1
               then ConditionMet -- we got one block
               else ConditionNotMet

      case result of
        Just (Right (ConditionMet, _)) -> pure ()
        Just (Right (ConditionNotMet, slotNo)) ->
          throwString $ nodeName <> " was unable to produce any blocks. Reached slot " <> show slotNo
        Just (Left err) ->
          throwString $ "foldBlocks on " <> nodeName <> " encountered an error while waiting for new blocks: " <> show (prettyError err)
        _ ->
          throwString $ nodeName <> " was unable to produce any blocks for " <> show timeoutSeconds <> "s"


idToRemoteAddressP2P :: ()
  => MonadIO m
  => HasCallStack
  => Map.Map Int PortNumber -> NodeId -> m RelayAccessPoint
idToRemoteAddressP2P portNumbersMap (NodeId i) = case Map.lookup i portNumbersMap of
  Just port -> pure $ RelayAccessAddress
      (showIpv4Address testnetDefaultIpv4Address)
      port
  Nothing -> do
    throwString $ "Found node id that was unaccounted for: " ++ show i

-- | A convenience wrapper around `createTestnetEnv` and `cardanoTestnet`
createAndRunTestnet :: ()
  => HasCallStack
  => TestnetCreationOptions
  -> TestnetRuntimeOptions
  -> Conf -- ^ Path to the test sandbox
  -> H.Integration TestnetRuntime
createAndRunTestnet creationOptions runtimeOptions conf = do
  liftToIntegration $ do
     createTestnetEnv creationOptions conf
     cardanoTestnet (creationNodes creationOptions) runtimeOptions conf

-- | Retry an action when `NodeAddressAlreadyInUseError` gets thrown from an action
retryOnAddressInUseError
  :: forall m a. HasCallStack
  => MonadIO m
  => ExceptT NodeStartFailure m a -- ^ action being retried
  -> ExceptT NodeStartFailure m a
retryOnAddressInUseError act = withFrozenCallStack $ go maximumTimeout retryTimeout
  where
    go :: HasCallStack => NominalDiffTime -> NominalDiffTime -> ExceptT NodeStartFailure m a
    go timeout' interval
      | timeout' <= 0 = withFrozenCallStack $ do
        act
      | otherwise = withFrozenCallStack $ do
        !time <- liftIOAnnotated DTC.getCurrentTime
        catchError act $ \case
          NodeAddressAlreadyInUseError _ -> do
            liftIOAnnotated $ threadDelay (round $ interval * 1_000_000)
            !time' <- liftIOAnnotated DTC.getCurrentTime
            let elapsedTime = time' `diffUTCTime` time
                newTimeout = timeout' - elapsedTime
            go newTimeout interval
          e -> throwError e

    -- Retry timeout in seconds. This should be > 2 * net.inet.tcp.msl on darwin,
    -- net.inet.tcp.msl in RFC 793 determines TIME_WAIT socket timeout.
    -- Usually it's 30 or 60 seconds. We take two times that plus some extra time.
    maximumTimeout = 150
    -- Wait for that many seconds before retrying.
    retryTimeout = 5

-- | Read node options from an existing testnet environment directory.
-- Scans @node-data/@ for node directories numbered @node1, node2, ...@
-- and checks @pools-keys/@ to classify each as SPO or relay.
-- Validates that nodes are consecutively numbered starting from 1,
-- and that all SPO nodes come before relay nodes.
readNodesWithOptionsFromEnv :: HasCallStack => MonadIO m => FilePath -> m TestnetNodesWithOptions
readNodesWithOptionsFromEnv envDir = do
  entries <- liftIO $ IO.listDirectory (envDir </> "node-data")
  let nodeNums = sort $ mapMaybe parseNodeNum entries
  when (null nodeNums) $
    throwString "No node directories found in environment"
  when (nodeNums /= [1 .. length nodeNums]) $
    throwString $ "Node directories are not consecutively numbered from 1: " <> show nodeNums
  isSpoFlags <- forM nodeNums $ \i ->
    liftIO $ IO.doesDirectoryExist (envDir </> Defaults.defaultSpoKeysDir i)
  let (spoFlags, relayFlags) = span id isSpoFlags
  unless (all not relayFlags) $
    throwString "SPO nodes must come before relay nodes in the environment"
  when (null spoFlags) $
    throwString "No SPO node directories found in environment"
  let nSpos = length spoFlags
  spoOpts <- mapM readNodeOpt [1 .. nSpos]
  relayOpts <- mapM readNodeOpt [nSpos + 1 .. length nodeNums]
  case spoOpts of
    (s:ss) -> pure $ TestnetNodesWithOptions { optSpoNodes = s :| ss, optRelayNodes = relayOpts }
    [] -> throwString "No SPO node directories found in environment"
  where
    parseNodeNum s = do
      rest <- stripPrefix "node" s
      readMaybe rest :: Maybe Int
    readNodeOpt i = do
      bin <- readNodeBinFromEnvFile (envDir </> defaultNodeEnvFile i)
      pure $ NodeWithOptions bin []

-- | Environment file contents for a node, serialized as YAML.
-- Written during testnet creation and read back when starting from an existing environment.
data NodeEnv = NodeEnv
  { nodeBinary :: FilePath -- ^ Absolute path to the @cardano-node@ binary
  , nodeVersion :: String -- ^ Version string (e.g. @"10.4.1"@), extracted from @cardano-node --version@ output
  } deriving (Eq, Show)

instance FromJSON NodeEnv where
  parseJSON = withObject "NodeEnv" $ \o ->
    NodeEnv <$> o .: "node_binary"
            <*> o .: "node_version"

instance ToJSON NodeEnv where
  toJSON NodeEnv{nodeBinary, nodeVersion} =
    object [ "node_binary" .= nodeBinary
           , "node_version" .= nodeVersion
           ]

readNodeBinFromEnvFile :: (HasCallStack, MonadIO m) => FilePath -> m (Maybe FilePath)
readNodeBinFromEnvFile envFile = runMaybeT $ do
  guard =<< liftIOAnnotated (IO.doesFileExist envFile)
  NodeEnv{nodeBinary} <- either failParse pure =<< liftIOAnnotated (Yaml.decodeFileEither envFile)
  pure nodeBinary
  where
    failParse err = throwString $ "Failed to parse node env file " <> envFile <> ": " <> show err

getNodeVersion :: HasCallStack => MonadIO m => FilePath -> m String
getNodeVersion bin = liftIOAnnotated $ do
  output <- Process.readProcess bin ["--version"] ""
    `catch` \(e :: IOException) ->
      throwString $ "Failed to run " <> bin <> " --version: " <> displayException e
  case words output of
    ("cardano-node":version:_) -> pure version
    _ -> throwString $ "Unexpected output from " <> bin <> " --version (expected 'cardano-node <version> ...'): " <> output
