{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  ) where


import           Cardano.Api

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)
import           Cardano.Node.Configuration.Topology

import           Prelude hiding (lines)

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Either
import           Data.Functor
import           Data.MonoTraversable (Element, MonoFunctor, omap)
import qualified Data.Text as Text
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as DTC
import           GHC.Stack
import qualified System.Directory as IO
import           System.FilePath ((</>))
import qualified System.Info as OS

import           Testnet.Components.Configuration
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Handlers (interruptNodesOnSigINT)
import           Testnet.Process.Run (execCli', execCli_, mkExecConfig)
import           Testnet.Property.Assert (assertChainExtended, assertExpectedSposInLedgerState)
import           Testnet.Runtime as TR
import           Testnet.Start.Types
import           Testnet.Types as TR hiding (shelleyGenesis)

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H

-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testMinimumConfigurationRequirements :: ()
  => HasCallStack
  => MonadTest m
  => CardanoTestnetOptions -> m ()
testMinimumConfigurationRequirements options = withFrozenCallStack $ do
  when (cardanoNumPools options < 1) $ do
    H.note_ "Need at least one SPO node to produce blocks, but got none."
    H.failure

createTestnetEnv :: ()
  => HasCallStack
  => CardanoTestnetOptions
  -> GenesisOptions
  -> UserProvidedData ShelleyGenesis
  -> UserProvidedData AlonzoGenesis
  -> UserProvidedData ConwayGenesis
  -> Conf
  -> H.Integration ()
createTestnetEnv
  testnetOptions@CardanoTestnetOptions
    { cardanoNodeEra=asbe
    , cardanoNodes
    }
  genesisOptions
  mShelley mAlonzo mConway
  Conf
    { genesisHashesPolicy
    , tempAbsPath=TmpAbsolutePath tmpAbsPath
    } = do

  testMinimumConfigurationRequirements testnetOptions

  AnyShelleyBasedEra sbe <- pure asbe
  _ <- createSPOGenesisAndFiles
    testnetOptions genesisOptions
    mShelley mAlonzo mConway
    (TmpAbsolutePath tmpAbsPath)

  configurationFile <- H.noteShow $ tmpAbsPath </> "configuration.yaml"
  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  config <- case genesisHashesPolicy of
    WithHashes -> createConfigJson (TmpAbsolutePath tmpAbsPath) sbe
    WithoutHashes -> pure $ createConfigJsonNoHash sbe
  H.evalIO $ LBS.writeFile configurationFile config

  -- Create network topology, with abstract IDs in lieu of addresses
  let nodeIds = fst <$> zip [1..] cardanoNodes
  forM_ nodeIds $ \i -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    H.evalIO $ IO.createDirectoryIfMissing True nodeDataDir

    let producers = NodeId <$> filter (/= i) nodeIds
    H.lbsWriteFile (tmpAbsPath </> Defaults.defaultNodeDataDir i </> "topology.json") . encode $
      RealNodeTopology producers

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
cardanoTestnet :: ()
  => HasCallStack
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> Conf -- ^ Path to the test sandbox
  -> H.Integration TestnetRuntime
cardanoTestnet
  testnetOptions
  GenesisOptions{genesisTestnetMagic=testnetMagic}
  Conf{tempAbsPath=TmpAbsolutePath tmpAbsPath} = do
  let CardanoTestnetOptions
        { cardanoNodeLoggingFormat=nodeLoggingFormat
        , cardanoEnableNewEpochStateLogging=enableNewEpochStateLogging
        , cardanoNodes
        } = testnetOptions
      nPools = cardanoNumPools testnetOptions
      nodeConfigFile = tmpAbsPath </> "configuration.yaml"

  H.note_ OS.os

  wallets <- forM [1..3] $ \idx -> do
    let utxoKeys@KeyPair{verificationKey} = makePathsAbsolute $ Defaults.defaultUtxoKeys idx
    let paymentAddrFile = tmpAbsPath </> "utxo-keys" </> "utxo" <> show idx </> "utxo.addr"

    execCli_
      [ "latest", "address", "build"
      , "--payment-verification-key-file", unFile verificationKey
      , "--testnet-magic", show testnetMagic
      , "--out-file", paymentAddrFile
      ]

    paymentAddr <- H.readFile paymentAddrFile

    pure $ PaymentKeyInfo
      { paymentKeyInfoPair = utxoKeys
      , paymentKeyInfoAddr = Text.pack paymentAddr
      }

  portNumbersWithNodeOptions <- forM cardanoNodes
    (\nodeOption -> (nodeOption,) <$> H.randomPort testnetDefaultIpv4Address)

  let portNumbers = zip [1..] $ snd <$> portNumbersWithNodeOptions

  forM_ portNumbers $ \(i, portNumber) -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    H.evalIO $ IO.createDirectoryIfMissing True nodeDataDir
    H.writeFile (nodeDataDir </> "port") (show portNumber)

  let idToRemoteAddress (NodeId i) = case lookup i portNumbers of
        Just port -> pure $ RemoteAddress
          { raAddress = showIpv4Address testnetDefaultIpv4Address
          , raPort = port
          , raValency = 1
          }
        Nothing -> do
          H.note_ "Found node id that was unaccounted for"
          H.failure

  -- Implement concrete topology from abstract one, if necessary
  forM_ portNumbers $ \(i, _port) -> do
    let topologyPath = tmpAbsPath </> Defaults.defaultNodeDataDir i </> "topology.json"

    H.evalIO (decodeFileStrict topologyPath) >>= \case
      Just (abstractTopology :: NetworkTopology NodeId) -> do
        topology <- mapM idToRemoteAddress abstractTopology
        H.lbsWriteFile topologyPath $ encode topology
      Nothing ->
        -- There can be multiple reason for why the decoding has failed.
        -- Here we assume, very optimistically, that the user has already
        -- instantiated it with a concrete topology file.
        pure ()

  eTestnetNodes <- H.forConcurrently (zip [1..] portNumbersWithNodeOptions) $ \(i, (nodeOptions, port)) -> do
    let nodeName = Defaults.defaultNodeName i
        nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
        nodePoolKeysDir = tmpAbsPath </> Defaults.defaultSpoKeysDir i
    H.note_ $ "Node name: " <> nodeName
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

  let (failedNodes, testnetNodes') = partitionEithers eTestnetNodes
  unless (null failedNodes) $ do
    H.noteShow_ . vsep $ prettyError <$> failedNodes
    H.failure

  -- Interrupt cardano nodes when the main process is interrupted
  H.evalIO $ interruptNodesOnSigINT testnetNodes'
  H.annotateShow $ nodeSprocket <$> testnetNodes'

  -- FIXME: use foldEpochState waiting for chain extensions
  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 45 now
  forM_ (map nodeStdout testnetNodes') $ \nodeStdoutFile -> do
    assertChainExtended deadline nodeLoggingFormat nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  let runtime = TestnetRuntime
        { configurationFile = File nodeConfigFile
        , shelleyGenesisFile = tmpAbsPath </> Defaults.defaultGenesisFilepath ShelleyEra
        , testnetMagic
        , testnetNodes=testnetNodes'
        , wallets
        , delegators = []
        }

  let tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tmpAbsPath

  node1sprocket <- H.headM $ testnetSprockets runtime
  execConfig <- mkExecConfig tempBaseAbsPath node1sprocket testnetMagic

  forM_ wallets $ \wallet -> do
    H.cat . signingKeyFp $ paymentKeyInfoPair wallet
    H.cat . verificationKeyFp $ paymentKeyInfoPair wallet

    utxos <- execCli' execConfig
      [ "latest", "query", "utxo"
      , "--address", Text.unpack $ paymentKeyInfoAddr wallet
      , "--cardano-mode"
      ]

    H.note_ utxos

  stakePoolsFp <- H.note $ tmpAbsPath </> "current-stake-pools.json"

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

-- | A convenience wrapper around `createTestnetEnv` and `cardanoTestnet`
createAndRunTestnet :: ()
  => HasCallStack
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> Conf -- ^ Path to the test sandbox
  -> H.Integration TestnetRuntime
createAndRunTestnet testnetOptions genesisOptions conf = do
  createTestnetEnv
    testnetOptions genesisOptions
    NoUserProvidedData NoUserProvidedData NoUserProvidedData
    conf
  cardanoTestnet testnetOptions genesisOptions conf

-- | Retry an action when `NodeAddressAlreadyInUseError` gets thrown from an action
retryOnAddressInUseError
  :: forall m a. HasCallStack
  => MonadTest m
  => MonadIO m
  => ExceptT NodeStartFailure m a -- ^ action being retried
  -> ExceptT NodeStartFailure m a
retryOnAddressInUseError act = withFrozenCallStack $ go maximumTimeout retryTimeout
  where
    go :: HasCallStack => NominalDiffTime -> NominalDiffTime -> ExceptT NodeStartFailure m a
    go timeout interval
      | timeout <= 0 = withFrozenCallStack $ do
        H.note_ "Exceeded timeout when retrying node start"
        act
      | otherwise = withFrozenCallStack $ do
        !time <- liftIO DTC.getCurrentTime
        catchError act $ \case
          NodeAddressAlreadyInUseError _ -> do
            liftIO $ threadDelay (round $ interval * 1_000_000)
            !time' <- liftIO DTC.getCurrentTime
            let elapsedTime = time' `diffUTCTime` time
                newTimeout = timeout - elapsedTime
            H.note_ $ "Retrying on 'address in use' error, timeout: " <> show newTimeout
            go newTimeout interval
          e -> throwError e

    -- Retry timeout in seconds. This should be > 2 * net.inet.tcp.msl on darwin,
    -- net.inet.tcp.msl in RFC 793 determines TIME_WAIT socket timeout.
    -- Usually it's 30 or 60 seconds. We take two times that plus some extra time.
    maximumTimeout = 150
    -- Wait for that many seconds before retrying.
    retryTimeout = 5
