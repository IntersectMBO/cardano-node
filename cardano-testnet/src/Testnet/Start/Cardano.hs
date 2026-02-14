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
  , CardanoTestnetOptions(..)
  , NodeOption(..)
  , cardanoDefaultTestnetNodeOptions

  , TestnetRuntime (..)

  , cardanoTestnet
  , createAndRunTestnet
  , createTestnetEnv
  , getDefaultAlonzoGenesis
  , getDefaultShelleyGenesis
  , retryOnAddressInUseError

  , liftToIntegration
  ) where


import           Cardano.Api
import           Cardano.Api.Byron (GenesisData (..))
import qualified Cardano.Api.Byron as Byron
import           Cardano.CLI.Type.Common (SigningKeyFile)

import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Prelude (canonicalEncodePretty, NonEmpty ((:|)))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))

import           Prelude hiding (lines)

import           Control.Concurrent (threadDelay)
import           Control.Monad ( when, forM_, forM, unless )
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (MonadResource, getInternalState)
import           Cardano.Node.Configuration.NodeAddress (NodeHostIPv4Address(..), NodeAddress'(..))
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..), NodeDescription(..))
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import           Data.Default.Class (def)
import           Data.Either
import           Data.Functor
import           Data.List (uncons)
import qualified Data.List.NonEmpty as NEL
import           Data.IP (fromHostAddress)
import qualified Data.Map as Map
import           Data.MonoTraversable (Element, MonoFunctor, omap)
import qualified Data.Text as Text
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as DTC
import           GHC.Stack
import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Handlers (interruptNodesOnSigINT)
import           Testnet.Orphans ()
import           Testnet.Process.RunIO (execCli', execCli_, liftIOAnnotated, mkExecConfig)
import           Testnet.Property.Assert (assertChainExtended, assertExpectedSposInLedgerState)
import           Testnet.Runtime as TR
import           Testnet.Start.Types
import           Testnet.Types as TR hiding (shelleyGenesis)

import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H
import           Hedgehog.Internal.Property (failException)

import           RIO (MonadUnliftIO, RIO (..), runRIO, throwString)
import           RIO.Orphans (ResourceMap)
import           UnliftIO.Async
import           UnliftIO.Exception (stringException)

-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testMinimumConfigurationRequirements :: ()
  => HasCallStack
  => MonadIO m
  => CardanoTestnetOptions -> m ()
testMinimumConfigurationRequirements options = withFrozenCallStack $ do
  when (cardanoNumPools options < 1) $ do
    throwString "Need at least one SPO node to produce blocks, but got none."

liftToIntegration :: HasCallStack => RIO ResourceMap a -> H.Integration a
liftToIntegration r =  do
   rMap <- lift $ lift getInternalState
   catch @_ @SomeException (runRIO rMap r) (withFrozenCallStack $ failException . toException . stringException . displayException)

createTestnetEnv :: ()
  => HasCallStack
  => MonadIO m
  => MonadThrow m
  => CardanoTestnetOptions
  -> GenesisOptions
  -> CreateEnvOptions
  -> Conf
  -> m ()
createTestnetEnv
  testnetOptions@CardanoTestnetOptions
    { cardanoNodeEra=asbe
    , cardanoNodes
    }
  genesisOptions
  CreateEnvOptions
    { ceoOnChainParams=onChainParams
    }
  Conf
    { genesisHashesPolicy
    , tempAbsPath=TmpAbsolutePath tmpAbsPath
    } = do

  testMinimumConfigurationRequirements testnetOptions

  AnyShelleyBasedEra sbe <- pure asbe

  _ <- createSPOGenesisAndFiles
    testnetOptions genesisOptions onChainParams
    (TmpAbsolutePath tmpAbsPath)

  let configurationFile = tmpAbsPath </> "configuration.yaml"
  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  config <- case genesisHashesPolicy of
    WithHashes -> createConfigJson (TmpAbsolutePath tmpAbsPath) sbe
    WithoutHashes -> pure $ createConfigJsonNoHash sbe

  liftIOAnnotated . LBS.writeFile configurationFile $ A.encodePretty $ Object config

  -- Create network topology, with abstract IDs in lieu of addresses
  let nodeIds = fst <$> NEL.zip (1 :| [2..]) cardanoNodes
  forM_ nodeIds $ \i -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    liftIOAnnotated $ IO.createDirectoryIfMissing True nodeDataDir

    let producers = NodeId <$> NEL.filter (/= i) nodeIds
        topology = Defaults.defaultP2PTopology producers
    liftIOAnnotated . LBS.writeFile (nodeDataDir </> "topology.json") $ A.encodePretty topology

-- | Starts a number of nodes, as configured by the value of the 'cardanoNodes'
-- field in the 'CardanoTestnetOptions' argument. Regarding this field, you can either:
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
  => CardanoTestnetOptions -- ^ The options to use
  -> Conf -- ^ Path to the test sandbox
  -> m TestnetRuntime
cardanoTestnet
  testnetOptions
  Conf
    { tempAbsPath=TmpAbsolutePath tmpAbsPath
    , updateTimestamps
    } = do
  let CardanoTestnetOptions
        { cardanoNodeLoggingFormat=nodeLoggingFormat
        , cardanoEnableNewEpochStateLogging=enableNewEpochStateLogging
        , cardanoNodes
        } = testnetOptions
      nPools = cardanoNumPools testnetOptions
      nodeConfigFile = tmpAbsPath </> "configuration.yaml"
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
    let paymentAddrFile = tmpAbsPath </> "utxo-keys" </> "utxo" <> show idx </> "utxo.addr"

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

  portNumbersWithNodeOptions <- forM cardanoNodes
    (\nodeOption -> (nodeOption,) <$> H.randomPort testnetDefaultIpv4Address)

  let portNumbers = NEL.zip (1 :| [2..]) $ snd <$> portNumbersWithNodeOptions
      portNumbersMap = Map.fromList (NEL.toList portNumbers)

      idToRemoteAddressP2P :: ()
        => MonadIO m
        => HasCallStack
        => NodeId -> m RelayAccessPoint
      idToRemoteAddressP2P (NodeId i) = case Map.lookup i portNumbersMap of
        Just port -> pure $ RelayAccessAddress
            (showIpv4Address testnetDefaultIpv4Address)
            port
        Nothing -> do
          throwString $ "Found node id that was unaccounted for: " ++ show i

  forM_ portNumbers $ \(i, portNumber) -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    liftIOAnnotated $ IO.createDirectoryIfMissing True nodeDataDir
    liftIOAnnotated $ writeFile (nodeDataDir </> "port") (show portNumber)
    let topologyPath = tmpAbsPath </> Defaults.defaultNodeDataDir i </> "topology.json"
    tBytes <- liftIOAnnotated $ LBS.readFile topologyPath
    case eitherDecode tBytes of 
      Right (abstractTopology :: P2P.NetworkTopology NodeId) -> do
        topology <- mapM idToRemoteAddressP2P abstractTopology
        liftIOAnnotated $ LBS.writeFile topologyPath $ encode topology
      Left e -> do
        -- There can be multiple reasons for why both decodings have failed.
        -- Here we assume, very optimistically, that the user has already
        -- instantiated it with a concrete topology file.
        liftIOAnnotated . putStrLn $ "Could not decode topology file: " <> topologyPath <> ". This may be okay. Reason for decoding failure is:\n" ++ e

  -- If necessary, update the time stamps in Byron and Shelley Genesis files.
  -- This is a QoL feature so that users who edit their configuration files don't
  -- have to manually set up the start times themselves.
  when (updateTimestamps == UpdateTimestamps) $ do
    currentTime <- liftIOAnnotated DTC.getCurrentTime
    let startTime = DTC.addUTCTime startTimeOffsetSeconds currentTime

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

  eTestnetNodes <- forConcurrently (NEL.zip (1 :| [2..]) portNumbersWithNodeOptions) $ \(i, (nodeOptions, port)) -> do
    let nodeName = Defaults.defaultNodeName i
        nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
        nodePoolKeysDir = tmpAbsPath </> Defaults.defaultSpoKeysDir i
    let (mKeys, spoNodeCliArgs) =
          case nodeOptions of
            RelayNodeOptions{} -> (Nothing, [])
            SpoNodeOptions{} -> (Just keys, shelleyCliArgs <> byronCliArgs)
          where
            shelleyCliArgs = [ "--shelley-kes-key", nodePoolKeysDir </> "kes.skey"
                             , "--shelley-vrf-key", unFile $ signingKey poolNodeKeysVrf
                             , "--shelley-operational-certificate", nodePoolKeysDir </> "opcert.cert"
                             ]
            byronCliArgs = [ "--byron-delegation-certificate", nodePoolKeysDir </> "byron-delegation.cert"
                           , "--byron-signing-key", nodePoolKeysDir </> "byron-delegate.key"
                           ]
            keys@SpoNodeKeys{poolNodeKeysVrf} = mkTestnetNodeKeyPaths i

    eRuntime <- runExceptT . retryOnAddressInUseError $
      startNode (TmpAbsolutePath tmpAbsPath) nodeName testnetDefaultIpv4Address port testnetMagic $
        [ "run"
        , "--config", nodeConfigFile
        , "--topology", nodeDataDir </> "topology.json"
        , "--database-path", nodeDataDir </> "db"
        ]
        <> spoNodeCliArgs
        <> extraCliArgs nodeOptions

    pure $ eRuntime <&> \rt -> rt{poolKeys=mKeys}

  let (failedNodes, testnetNodes') = partitionEithers (NEL.toList eTestnetNodes)
  unless (null failedNodes) $ do
    throwString $ "Some nodes failed to start:\n" ++ show (vsep $ prettyError <$> failedNodes)

  -- Interrupt cardano nodes when the main process is interrupted
  liftIOAnnotated $ interruptNodesOnSigINT testnetNodes'

  -- FIXME: use foldEpochState waiting for chain extensions
  now <- liftIOAnnotated DTC.getCurrentTime
  let deadline = DTC.addUTCTime 45 now
  forM_ testnetNodes' $ \nodeStdoutFile -> do
    assertChainExtended deadline nodeLoggingFormat nodeStdoutFile

  let node1SocketPath = tmpAbsPath </> "socket" </> "node1" </> "sock"
      utxoSigningKeyFile = File $ (tmpAbsPath </>) $ unFile $ signingKey $ Defaults.defaultUtxoKeys 1
      nodeDescriptions = NEL.map (\(i, port) -> NodeDescription (NodeAddress (NodeHostIPv4Address (fromHostAddress testnetDefaultIpv4Address)) port) (Defaults.defaultNodeName i) ) portNumbers

  generateTxGenConfig tmpAbsPath nodeConfigFile utxoSigningKeyFile node1SocketPath nodeDescriptions

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
    extraCliArgs = \case
      SpoNodeOptions args -> args
      RelayNodeOptions args -> args
    -- TODO: This should come from the configuration!
    makePathsAbsolute :: (Element a ~ FilePath, MonoFunctor a) => a -> a
    makePathsAbsolute = omap (tmpAbsPath </>)
    mkTestnetNodeKeyPaths :: Int -> SpoNodeKeys
    mkTestnetNodeKeyPaths n = makePathsAbsolute $ Defaults.defaultSpoKeys n

    generateTxGenConfig :: MonadUnliftIO m => FilePath -> FilePath -> SigningKeyFile 'In -> String -> NonEmpty NodeDescription -> m ()
    generateTxGenConfig basePath nodeConfigFilePath utxoSigningKeyFile localNodeSocketPath nodeTopology = do
      let nixServiceOptions = NixServiceOptions {
            _nix_debugMode        = False
          , _nix_tx_count         = 100
          , _nix_tps              = 10
          , _nix_inputs_per_tx    = 2
          , _nix_outputs_per_tx   = 2
          , _nix_tx_fee           = 212_345
          , _nix_min_utxo_value   = 1_000_000
          , _nix_add_tx_size      = 39
          , _nix_init_cooldown    = 50
          , _nix_era              = AnyCardanoEra ConwayEra
          , _nix_plutus           = Nothing
          , _nix_keepalive        = Just 30
          , _nix_nodeConfigFile       = Just nodeConfigFilePath
          , _nix_cardanoTracerSocket  = Nothing
          , _nix_sigKey               = utxoSigningKeyFile
          , _nix_localNodeSocketPath  = localNodeSocketPath
          , _nix_targetNodes          = nodeTopology
          }
      liftIOAnnotated . LBS.writeFile (basePath </> "tx-generator-config.json") $ A.encodePretty nixServiceOptions
      pure ()

-- | A convenience wrapper around `createTestnetEnv` and `cardanoTestnet`
createAndRunTestnet :: ()
  => HasCallStack
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> Conf -- ^ Path to the test sandbox
  -> H.Integration TestnetRuntime
createAndRunTestnet testnetOptions genesisOptions conf = do
  liftToIntegration $ do
     createTestnetEnv
       testnetOptions genesisOptions def
       conf
     cardanoTestnet testnetOptions conf

-- | Retry an action when `NodeAddressAlreadyInUseError` gets thrown from an action
retryOnAddressInUseError
  :: forall m a. HasCallStack
  => MonadIO m
  => ExceptT NodeStartFailure m a -- ^ action being retried
  -> ExceptT NodeStartFailure m a
retryOnAddressInUseError act = withFrozenCallStack $ go maximumTimeout retryTimeout
  where
    go :: HasCallStack => NominalDiffTime -> NominalDiffTime -> ExceptT NodeStartFailure m a
    go timeout interval
      | timeout <= 0 = withFrozenCallStack $ do
        act
      | otherwise = withFrozenCallStack $ do
        !time <- liftIOAnnotated DTC.getCurrentTime
        catchError act $ \case
          NodeAddressAlreadyInUseError _ -> do
            liftIOAnnotated $ threadDelay (round $ interval * 1_000_000)
            !time' <- liftIOAnnotated DTC.getCurrentTime
            let elapsedTime = time' `diffUTCTime` time
                newTimeout = timeout - elapsedTime
            go newTimeout interval
          e -> throwError e

    -- Retry timeout in seconds. This should be > 2 * net.inet.tcp.msl on darwin,
    -- net.inet.tcp.msl in RFC 793 determines TIME_WAIT socket timeout.
    -- Usually it's 30 or 60 seconds. We take two times that plus some extra time.
    maximumTimeout = 150
    -- Wait for that many seconds before retrying.
    retryTimeout = 5
