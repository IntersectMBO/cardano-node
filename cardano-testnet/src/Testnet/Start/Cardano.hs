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
  , TestnetNodeOptions(..)
  , cardanoDefaultTestnetNodeOptions

  , TestnetRuntime (..)

  , cardanoTestnet
  , cardanoTestnetDefault
  , getDefaultAlonzoGenesis
  , getDefaultShelleyGenesis
  , retryOnAddressInUseError
  ) where


import           Cardano.Api
import           Cardano.Api.Ledger (StandardCrypto)

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)
import           Cardano.Node.Configuration.Topology

import           Prelude hiding (lines)

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import           Data.Either
import           Data.Functor
import           Data.MonoTraversable (Element, MonoFunctor, omap)
import qualified Data.Text as Text
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as DTC
import           Data.Word (Word64)
import           GHC.Stack
import qualified GHC.Stack as GHC
import qualified System.Directory as IO
import           System.FilePath ((</>))
import qualified System.Info as OS

import           Testnet.Components.Configuration
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli', execCli_, mkExecConfig)
import           Testnet.Property.Assert (assertChainExtended, assertExpectedSposInLedgerState)
import           Testnet.Runtime as TR
import           Testnet.Start.Types
import           Testnet.Types as TR hiding (shelleyGenesis)

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H
import qualified Hedgehog.Extras.Stock.OS as OS

-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testMinimumConfigurationRequirements :: HasCallStack
                                        => MonadTest m
                                        => CardanoTestnetOptions -> m ()
testMinimumConfigurationRequirements CardanoTestnetOptions{cardanoNodes} = withFrozenCallStack $ do
  let nSpoNodes = length [ () | SpoNodeOptions{} <- cardanoNodes]
  when (nSpoNodes < 1) $ do
     H.note_ "Need at least one SPO node to produce blocks, but got none."
     H.failure

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

-- | Like 'cardanoTestnet', but using 'GenesisOptions' to obtain
-- the genesis files, instead of passing them directly.
-- See 'cardanoTestnet' for additional documentation.
-- TODO delete me? @cardano-testnet@ plus @getDefaultGenesisBatch@ suffice now.
cardanoTestnetDefault
  :: ()
  => HasCallStack
  => CardanoTestnetOptions
  -> GenesisOptions
  -> Conf
  -> H.Integration TestnetRuntime
cardanoTestnetDefault testnetOptions shelleyOptions conf = do
  AnyShelleyBasedEra sbe <- pure cardanoNodeEra
  alonzoGenesis <- getDefaultAlonzoGenesis sbe
  shelleyGenesis <- getDefaultShelleyGenesis cardanoNodeEra cardanoMaxSupply shelleyOptions
  cardanoTestnet
    testnetOptions conf UserNodeConfigNotSubmitted
    (shelleyGenesis, DefaultedOrigin) (alonzoGenesis, DefaultedOrigin) (Defaults.defaultConwayGenesis, DefaultedOrigin)
  where
    CardanoTestnetOptions{cardanoNodeEra, cardanoMaxSupply} = testnetOptions

-- | An 'AlonzoGenesis' value that is fit to pass to 'cardanoTestnet'
getDefaultAlonzoGenesis :: ()
  => HasCallStack
  => MonadTest m
  => ShelleyBasedEra era
  -> m AlonzoGenesis
getDefaultAlonzoGenesis sbe = H.evalEither $ first prettyError (Defaults.defaultAlonzoGenesis sbe)

-- | A start time and 'ShelleyGenesis' value that are fit to pass to 'cardanoTestnet'
getDefaultShelleyGenesis :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => AnyShelleyBasedEra
  -> Word64 -- ^ The max supply
  -> GenesisOptions
  -> m (ShelleyGenesis StandardCrypto)
getDefaultShelleyGenesis asbe maxSupply opts = do
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  return $ Defaults.defaultShelleyGenesis asbe startTime maxSupply opts

-- | Setup a number of credentials and nodes (SPOs and relays), like this:
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
-- > │   │   ├── key.{skey,vkey}
-- > │   │   └── key.vkey
-- > │   └── README.md
-- > ├── logs
-- > │   ├── node{1,2,3}
-- > │   │   └── {stderr,stdout}.log
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
-- > ├── alonzo-genesis.json
-- > ├── byron.genesis.json
-- > ├── byron.genesis.spec.json
-- > ├── configuration.yaml
-- > ├── conway-genesis.json
-- > ├── current-stake-pools.json
-- > ├── genesis.{alonzo,conway}.spec.json
-- > ├── module
-- > └── shelley-genesis.json
--
cardanoTestnet :: ()
  => HasCallStack
  => CardanoTestnetOptions -- ^ The options to use
  -> Conf
  -> UserNodeConfig -- ^ The node configuration file to use. If omitted it's generated.
  -> (ShelleyGenesis StandardCrypto, GenesisOrigin) -- ^ The shelley genesis to use, for example 'getDefaultShelleyGenesis' from this module.
                                                    --   Some fields are overridden by the accompanying 'CardanoTestnetOptions'.
  -> (AlonzoGenesis, GenesisOrigin)  -- ^ The alonzo genesis to use, for example 'getDefaultAlonzoGenesis' from this module.
  -> (ConwayGenesis StandardCrypto, GenesisOrigin) -- ^ The conway genesis to use, for example 'Defaults.defaultConwayGenesis'.
  -> H.Integration TestnetRuntime
cardanoTestnet
  testnetOptions Conf{tempAbsPath=TmpAbsolutePath tmpAbsPath} mNodeConfigFile
  shelleyPair@(shelleyGenesis, _) alonzoPair@(alonzoGenesis, _) conwayPair@(conwayGenesis, _) = do
  let CardanoTestnetOptions
        { cardanoNodeEra=asbe
        , cardanoMaxSupply=maxSupply
        , cardanoNodeLoggingFormat=nodeLoggingFormat
        , cardanoEnableNewEpochStateLogging=enableNewEpochStateLogging
        , cardanoNumDReps=nDReps
        , cardanoNodes
        } = testnetOptions
      testnetMagic = fromIntegral $ sgNetworkMagic shelleyGenesis
      nPools = cardanoNumPools testnetOptions
  AnyShelleyBasedEra sbe <- pure asbe

  testMinimumConfigurationRequirements testnetOptions

  H.note_ OS.os

  -- Write specification files. Those are the same as the genesis files
  -- used for launching the nodes, but omitting the content regarding stake, utxos, etc.
  -- They are used by benchmarking: as templates to CLI commands,
  -- as evidence of what was run, and as cache keys.
  writeGenesisSpecFile "alonzo" alonzoGenesis
  writeGenesisSpecFile "conway" conwayGenesis

  _ <- createSPOGenesisAndFiles nPools nDReps maxSupply asbe shelleyPair alonzoPair conwayPair (TmpAbsolutePath tmpAbsPath)

  -- TODO: This should come from the configuration!
  let makePathsAbsolute :: (Element a ~ FilePath, MonoFunctor a) => a -> a
      makePathsAbsolute = omap (tmpAbsPath </>)
      mkTestnetNodeKeyPaths :: Int -> SpoNodeKeys
      mkTestnetNodeKeyPaths n = makePathsAbsolute $ Defaults.defaultSpoKeys n

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

  _delegators <- forM [1..3] $ \(idx :: Int) -> do
    pure $ Delegator
      { paymentKeyPair = KeyPair
        { signingKey = File $ tmpAbsPath </> "stake-delegator-keys/payment" <> show idx <> ".skey"
        , verificationKey = File $ tmpAbsPath </> "stake-delegator-keys/payment" <> show idx <> ".vkey"
        }
      , stakingKeyPair = KeyPair
        { signingKey = File $ tmpAbsPath </> "stake-delegator-keys/staking" <> show idx <> ".skey"
        , verificationKey = File $ tmpAbsPath </> "stake-delegator-keys/staking" <> show idx <> ".vkey"
        }
      }

  nodeConfigFile <- case mNodeConfigFile of
    UserNodeConfigNotSubmitted -> do
      configurationFile <- H.noteShow $ tmpAbsPath </> "configuration.yaml"
      -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
      config <- createConfigJson (TmpAbsolutePath tmpAbsPath) sbe
      H.evalIO $ LBS.writeFile configurationFile config
      return configurationFile
    UserNodeConfig userSubmittedNodeConfigFile -> pure userSubmittedNodeConfigFile

  execCli_
    [ "debug", "check-node-configuration"
    , "--node-configuration-file", nodeConfigFile
    ]

  portNumbersWithNodeOptions <- forM cardanoNodes $ \nodeOption -> (nodeOption,) <$> H.randomPort testnetDefaultIpv4Address
  let portNumbers = snd <$> portNumbersWithNodeOptions

  -- Byron related
  forM_ (zip [1..] portNumbersWithNodeOptions) $ \(i, (_nodeOptions, portNumber)) -> do
    let nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
    H.evalIO $ IO.createDirectoryIfMissing True nodeDataDir
    H.writeFile (nodeDataDir </> "port") (show portNumber)

  -- Make Non P2P topology files
  forM_ (zip [1..] portNumbers) $ \(i, myPortNumber) -> do
    let producers = flip map (filter (/= myPortNumber) portNumbers) $ \otherProducerPort ->
          RemoteAddress
            { raAddress = showIpv4Address testnetDefaultIpv4Address
            , raPort = otherProducerPort
            , raValency = 1
            }

    H.lbsWriteFile (tmpAbsPath </> Defaults.defaultNodeDataDir i </> "topology.json") . encode $
      RealNodeTopology producers

  eTestnetNodes <- H.forConcurrently (zip [1..] portNumbersWithNodeOptions) $ \(i, (nodeOptions, port)) -> do
    let nodeName = Defaults.defaultNodeName i
        nodeDataDir = tmpAbsPath </> Defaults.defaultNodeDataDir i
        nodePoolKeysDir = tmpAbsPath </> Defaults.defaultSpoKeysDir i
    H.note_ $ "Node name: " <> nodeName
    let (mKeys, spoNodeCliArgs) =
          case nodeOptions of
            RelayNodeOptions{} -> (Nothing, [])
            SpoNodeOptions{} -> do
              let keys@SpoNodeKeys{poolNodeKeysVrf} = mkTestnetNodeKeyPaths i
                  -- provide keys' locations for SPO nodes
                  cliArgs =
                    [ "--shelley-kes-key", nodePoolKeysDir </> "kes.skey"
                    , "--shelley-vrf-key", unFile $ signingKey poolNodeKeysVrf
                    , "--byron-delegation-certificate", nodePoolKeysDir </> "byron-delegation.cert"
                    , "--byron-signing-key", nodePoolKeysDir </> "byron-delegate.key"
                    , "--shelley-operational-certificate", nodePoolKeysDir </> "opcert.cert"
                    ]
              (Just keys, cliArgs)

    eRuntime <- runExceptT . retryOnAddressInUseError $
      startNode (TmpAbsolutePath tmpAbsPath) nodeName testnetDefaultIpv4Address port testnetMagic $
        [ "run"
        , "--config", nodeConfigFile
        , "--topology", nodeDataDir </> "topology.json"
        , "--database-path", nodeDataDir </> "db"
        ]
        <> spoNodeCliArgs
        <> testnetNodeExtraCliArgs nodeOptions
    pure $ eRuntime <&> \rt -> rt{poolKeys=mKeys}

  let (failedNodes, testnetNodes') = partitionEithers eTestnetNodes
  unless (null failedNodes) $ do
    H.noteShow_ . vsep $ prettyError <$> failedNodes
    H.failure

  H.annotateShow $ nodeSprocket <$> testnetNodes'

  -- FIXME: use foldEpochState waiting for chain extensions
  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 45 now
  forM_ (map nodeStdout testnetNodes') $ \nodeStdoutFile -> do
    assertChainExtended deadline nodeLoggingFormat nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  forM_ wallets $ \wallet -> do
    H.cat . signingKeyFp $ paymentKeyInfoPair wallet
    H.cat . verificationKeyFp $ paymentKeyInfoPair wallet

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
  writeGenesisSpecFile :: (MonadTest m, MonadIO m, HasCallStack) => ToJSON a => String -> a -> m ()
  writeGenesisSpecFile eraName toWrite = GHC.withFrozenCallStack $ do
    genesisJsonFile <- H.noteShow $ tmpAbsPath </> "genesis." <> eraName <> ".spec.json"
    H.evalIO $ LBS.writeFile genesisJsonFile $ Aeson.encode toWrite

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