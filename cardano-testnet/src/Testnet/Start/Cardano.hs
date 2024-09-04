{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Cardano
  ( ForkPoint(..)
  , CardanoTestnetCliOptions(..)
  , CardanoTestnetOptions(..)
  , extraSpoNodeCliArgs
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
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as DTC
import           Data.Word (Word64)
import           GHC.Stack
import qualified GHC.Stack as GHC
import           System.FilePath ((</>))
import qualified System.Info as OS
import           Text.Printf (printf)

import           Testnet.Components.Configuration
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli', execCli_, mkExecConfig)
import           Testnet.Property.Assert (assertChainExtended, assertExpectedSposInLedgerState)
import           Testnet.Runtime as TR
import qualified Testnet.Start.Byron as Byron
import           Testnet.Start.Types
import           Testnet.Types as TR hiding (shelleyGenesis)

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H
import qualified Hedgehog.Extras.Stock.OS as OS

-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testnetMinimumConfigurationRequirements :: MonadTest m => NumPools -> m ()
testnetMinimumConfigurationRequirements (NumPools n) =
  when (n < 2) $ do
     H.noteShow_ ("Need at least two nodes to run a cluster, but got: " <> show n)
     H.failure

data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)


-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

-- | Like 'cardanoTestnet', but using 'ShelleyTestnetOptions' to obtain
-- the genesis files, instead of passing them directly.
-- See 'cardanoTestnet' for additional documentation.
cardanoTestnetDefault
  :: ()
  => HasCallStack
  => CardanoTestnetOptions
  -> ShelleyTestnetOptions
  -> Conf
  -> H.Integration TestnetRuntime
cardanoTestnetDefault testnetOptions shelleyOptions conf = do
  AnyShelleyBasedEra sbe <- pure cardanoNodeEra
  alonzoGenesis <- getDefaultAlonzoGenesis sbe
  shelleyGenesis <- getDefaultShelleyGenesis cardanoNodeEra cardanoMaxSupply shelleyOptions
  cardanoTestnet testnetOptions conf shelleyGenesis alonzoGenesis Defaults.defaultConwayGenesis
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
  -> ShelleyTestnetOptions
  -> m (ShelleyGenesis StandardCrypto)
getDefaultShelleyGenesis asbe maxSupply opts = do
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  return $ Defaults.defaultShelleyGenesis asbe startTime maxSupply opts

-- | Setup a number of credentials and pools, like this:
--
-- > ├── byron
-- > │   └── genesis.json
-- > ├── byron-gen-command
-- > │   └── genesis-keys.00{0,1,2}.key
-- > ├── byron.genesis.spec.json
-- > ├── configuration.yaml
-- > ├── current-stake-pools.json
-- > ├── delegate-keys
-- > │   ├── delegate{1,2,3}
-- > │   │   ├── kes.{skey,vkey}
-- > │   │   ├── key.{skey,vkey}
-- > │   │   ├── opcert.{cert,counter}
-- > │   │   └── vrf.{skey,vkey}
-- > │   └── README.md
-- > ├── drep-keys
-- > │   └── drep{1,2,3}
-- > │       └── drep.{skey,vkey}
-- > ├── genesis.{alonzo,conway}.spec.json
-- > ├── genesis-keys
-- > │   ├── genesis{1,2,3}
-- > │   │   └── key.{skey,vkey}
-- > │   └── README.md
-- > ├── pools-keys
-- > │   ├── pool{1,2,3}
-- > │   │   ├── byron-delegate.key
-- > │   │   ├── byron-delegation.cert
-- > │   │   ├── cold.{skey,vkey}
-- > │   │   ├── kes.{skey,vkey}
-- > │   │   ├── opcert.{cert,counter}
-- > │   │   ├── staking-reward.{skey,vkey}
-- > │   │   ├── topology.json
-- > │   │   └── vrf.{skey,vkey}
-- > │   └── README.md
-- > ├── shelley
-- > │   └── genesis.{alonzo,conway,shelley}.json
-- > ├── socket
-- > │   └── pool{1,2,3}
-- > │       └── sock
-- > ├── stake-delegators
-- > │   └── delegator{1,2,3}
-- > │       ├── payment.{skey,vkey}
-- > │       └── staking.{skey,vkey}
-- > └─── utxo-keys
-- >     ├── README.md
-- >     └── utxo{1,2,3}
-- >         └── utxo.{addr,skey,vkey}
cardanoTestnet :: ()
  => HasCallStack
  => CardanoTestnetOptions -- ^ The options to use
  -> Conf
  -> ShelleyGenesis StandardCrypto -- ^ The shelley genesis to use, for example 'getDefaultShelleyGenesis' from this module.
                                   --   Some fields are overridden by the accompanying 'CardanoTestnetOptions'.
  -> AlonzoGenesis -- ^ The alonzo genesis to use, for example 'getDefaultAlonzoGenesis' from this module.
  -> ConwayGenesis StandardCrypto -- ^ The conway genesis to use, for example 'Defaults.defaultConwayGenesis'.
  -> H.Integration TestnetRuntime
cardanoTestnet
  testnetOptions Conf {tempAbsPath=TmpAbsolutePath tmpAbsPath}
  shelleyGenesis alonzoGenesis conwayGenesis = do
  let (CardanoTestnetOptions _cardanoNodes asbe maxSupply _p2p nodeLoggingFormat _numDReps newEpochStateLogging) = testnetOptions
      startTime = sgSystemStart shelleyGenesis
      testnetMagic = fromIntegral $ sgNetworkMagic shelleyGenesis
      numPoolNodes = length $ cardanoNodes testnetOptions
      nPools = numPools testnetOptions
      nDReps = numDReps testnetOptions
  AnyShelleyBasedEra sbe <- pure asbe

  -- Sanity check
  testnetMinimumConfigurationRequirements nPools

  H.note_ OS.os

  if all isJust [mconfig | SpoTestnetNodeOptions mconfig _ <- cardanoNodes testnetOptions]
  then
    -- TODO: We need a very simple non-obscure way of generating the files necessary
    -- to run a testnet. "create-staked" is not a good way to do this especially because it
    -- makes assumptions about where things should go and where genesis template files should be.
    -- See all of the ad hoc file creation/renaming/dir creation etc below.
    H.failMessage GHC.callStack "Specifying node configuration files per node not supported yet."
  else do
    H.lbsWriteFile (tmpAbsPath </> "byron.genesis.spec.json")
      . encode $ Defaults.defaultByronProtocolParamsJsonValue

    -- Because in Conway the overlay schedule and decentralization parameter
    -- are deprecated, we must use the "create-staked" cli command to create
    -- SPOs in the ShelleyGenesis
    Byron.createByronGenesis
      testnetMagic
      startTime
      Byron.byronDefaultGenesisOptions
      (tmpAbsPath </> "byron.genesis.spec.json")
      (tmpAbsPath </> "byron-gen-command")

    -- Write specification files. Those are the same as the genesis files
    -- used for launching the nodes, but omitting the content regarding stake, utxos, etc.
    -- They are used by benchmarking: as templates to CLI commands,
    -- as evidence of what was run, and as cache keys.
    writeGenesisSpecFile "alonzo" alonzoGenesis
    writeGenesisSpecFile "conway" conwayGenesis

    configurationFile <- H.noteShow . File $ tmpAbsPath </> "configuration.yaml"

    _ <- createSPOGenesisAndFiles nPools nDReps maxSupply asbe shelleyGenesis alonzoGenesis conwayGenesis (TmpAbsolutePath tmpAbsPath)

    -- TODO: This should come from the configuration!
    let poolKeyDir :: Int -> FilePath
        poolKeyDir i = "pools-keys" </> mkNodeName i
        mkNodeName :: Int -> String
        mkNodeName i = "pool" <> show i

    poolKeys <- H.noteShow $ flip fmap [1..numPoolNodes] $ \n ->
      -- TODO: use Testnet.Defaults.defaultSpoKeys here
      PoolNodeKeys
        { poolNodeKeysCold =
          KeyPair
            { verificationKey = File $ tmpAbsPath </> poolKeyDir n </> "cold.vkey"
            , signingKey = File $ tmpAbsPath </> poolKeyDir n  </> "cold.skey"
            }
        , poolNodeKeysVrf =
          KeyPair
            { verificationKey = File $ tmpAbsPath </> poolKeyDir n  </> "vrf.vkey"
            , signingKey = File $ tmpAbsPath </> poolKeyDir n  </> "vrf.skey"
            }
        , poolNodeKeysStaking =
          KeyPair
            { verificationKey = File $ tmpAbsPath </> poolKeyDir n  </> "staking-reward.vkey"
            , signingKey = File $ tmpAbsPath </> poolKeyDir n  </> "staking-reward.skey"
            }
        }

    let makeUTxOVKeyFp :: Int -> FilePath
        makeUTxOVKeyFp n = tmpAbsPath </> "utxo-keys" </> "utxo" <> show n </> "utxo.vkey"

        makeUTxOSkeyFp :: Int -> FilePath
        makeUTxOSkeyFp n = tmpAbsPath </> "utxo-keys" </> "utxo" <> show n </> "utxo.skey"

    wallets <- forM [1..3] $ \idx -> do
      let paymentSKeyFile = makeUTxOSkeyFp idx
      let paymentVKeyFile = makeUTxOVKeyFp idx
      let paymentAddrFile = tmpAbsPath </> "utxo-keys" </> "utxo" <> show idx </> "utxo.addr"

      execCli_
        [ "address", "build"
        , "--payment-verification-key-file", makeUTxOVKeyFp idx
        , "--testnet-magic", show testnetMagic
        , "--out-file", paymentAddrFile
        ]

      paymentAddr <- H.readFile paymentAddrFile

      pure $ PaymentKeyInfo
        { paymentKeyInfoPair = KeyPair
          { signingKey = File paymentSKeyFile
          , verificationKey = File paymentVKeyFile
          }
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

    -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
    config <- createConfigJson (TmpAbsolutePath tmpAbsPath) sbe

    H.evalIO $ LBS.writeFile (unFile configurationFile) config

    portNumbers <- replicateM numPoolNodes $ H.randomPort testnetDefaultIpv4Address
    -- Byron related
    forM_ (zip [1..] portNumbers) $ \(i, portNumber) -> do
      let iStr = printf "%03d" (i - 1)
      H.renameFile (tmpAbsPath </> "byron-gen-command" </> "delegate-keys." <> iStr <> ".key") (tmpAbsPath </> poolKeyDir i </> "byron-delegate.key")
      H.renameFile (tmpAbsPath </> "byron-gen-command" </> "delegation-cert." <> iStr <> ".json") (tmpAbsPath </> poolKeyDir i </> "byron-delegation.cert")
      H.writeFile (tmpAbsPath </> poolKeyDir i </> "port") (show portNumber)

    -- Make topology files
    forM_ (zip [1..] portNumbers) $ \(i, myPortNumber) -> do
      let producers = flip map (filter (/= myPortNumber) portNumbers) $ \otherProducerPort ->
            RemoteAddress
              { raAddress = showIpv4Address testnetDefaultIpv4Address
              , raPort = otherProducerPort
              , raValency = 1
              }

      H.lbsWriteFile (tmpAbsPath </> poolKeyDir i </> "topology.json") . encode $
        RealNodeTopology producers

    let keysWithPorts = L.zip3 [1..] poolKeys portNumbers
    ePoolNodes <- H.forConcurrently keysWithPorts $ \(i, key, port) -> do
      let nodeName = mkNodeName i
          keyDir = tmpAbsPath </> poolKeyDir i
      H.note_ $ "Node name: " <> nodeName
      eRuntime <- runExceptT . retryOnAddressInUseError $
        startNode (TmpAbsolutePath tmpAbsPath) nodeName testnetDefaultIpv4Address port testnetMagic
          [ "run"
          , "--config", unFile configurationFile
          , "--topology", keyDir </> "topology.json"
          , "--database-path", keyDir </> "db"
          , "--shelley-kes-key", keyDir </> "kes.skey"
          , "--shelley-vrf-key", keyDir </> "vrf.skey"
          , "--byron-delegation-certificate", keyDir </> "byron-delegation.cert"
          , "--byron-signing-key", keyDir </> "byron-delegate.key"
          , "--shelley-operational-certificate", keyDir </> "opcert.cert"
          ]
      pure $ flip PoolNode key <$> eRuntime

    let (failedNodes, poolNodes) = partitionEithers ePoolNodes
    unless (null failedNodes) $ do
      H.noteShow_ . vsep $ prettyError <$> failedNodes
      H.failure

    -- FIXME: use foldEpochState waiting for chain extensions
    now <- H.noteShowIO DTC.getCurrentTime
    deadline <- H.noteShow $ DTC.addUTCTime 45 now
    forM_ (map (nodeStdout . poolRuntime) poolNodes) $ \nodeStdoutFile -> do
      assertChainExtended deadline nodeLoggingFormat nodeStdoutFile

    H.noteShowIO_ DTC.getCurrentTime

    forM_ wallets $ \wallet -> do
      H.cat . signingKeyFp $ paymentKeyInfoPair wallet
      H.cat . verificationKeyFp $ paymentKeyInfoPair wallet

    let runtime = TestnetRuntime
          { configurationFile
          , shelleyGenesisFile = tmpAbsPath </> Defaults.defaultGenesisFilepath ShelleyEra
          , testnetMagic
          , poolNodes
          , wallets
          , delegators = []
          }

    let tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tmpAbsPath

    node1sprocket <- H.headM $ poolSprockets runtime
    execConfig <- mkExecConfig tempBaseAbsPath node1sprocket testnetMagic

    forM_ wallets $ \wallet -> do
      H.cat . signingKeyFp $ paymentKeyInfoPair wallet
      H.cat . verificationKeyFp $ paymentKeyInfoPair wallet

      utxos <- execCli' execConfig
        [ "query", "utxo"
        , "--address", Text.unpack $ paymentKeyInfoAddr wallet
        , "--cardano-mode"
        ]

      H.note_ utxos

    stakePoolsFp <- H.note $ tmpAbsPath </> "current-stake-pools.json"

    assertExpectedSposInLedgerState stakePoolsFp nPools execConfig

    when newEpochStateLogging $
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


