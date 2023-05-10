{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}
{-# LANGUAGE NumericUnderscores #-}

module Testnet.Babbage
  ( TestnetRuntime (..)
  , PaymentKeyPair(..)

  , babbageTestnet
  ) where

import           Prelude

import           Control.Monad
import           Data.Aeson (encode, object, toJSON, (.=))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import           System.FilePath.Posix ((</>))
import qualified System.Info as OS

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as IO
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Functor (($>))
import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock (allocateRandomPorts)
import qualified Network.Socket as IO
import           Testnet.Commands.Genesis
import qualified Testnet.Conf as H
import           Testnet.Options
import qualified Testnet.Util.Assert as H
import           Testnet.Util.Process (execCli_)
import           Testnet.Util.Runtime (Delegator (..), NodeLoggingFormat (..), PaymentKeyPair (..),
                   PoolNode (PoolNode), PoolNodeKeys (..), StakingKeyPair (..), TestnetRuntime (..),
                   startNode)


{- HLINT ignore "Redundant flip" -}

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


-- | Check if a TCP port is open
isPortOpen :: Int -> IO Bool
isPortOpen port = do
  socketAddressInfos <- IO.getAddrInfo Nothing (Just "127.0.0.1") (Just (show port))
  case socketAddressInfos of
    socketAddressInfo:_ -> canConnect (IO.addrAddress socketAddressInfo) $> True
    []                  -> return False

-- | Check if it is possible to connect to a socket address
-- TODO: upstream fix to Hedgehog Extras
canConnect :: IO.SockAddr -> IO Bool
canConnect sockAddr = IO.bracket (IO.socket IO.AF_INET IO.Stream 6) IO.close' $ \sock -> do
  res <- IO.try $ IO.connect sock sockAddr
  case res of
    Left (_ :: IO.IOException) -> return False
    Right _                    -> return True

-- | Get random list of open ports. Timeout after 60seconds if unsuccessful.
getOpenPorts :: (MonadTest m, MonadIO m) => Int -> Int -> m [Int]
getOpenPorts n numberOfPorts = do
  when (n == 0) $ do
   error "getOpenPorts timeout"
  ports <- liftIO $ allocateRandomPorts numberOfPorts
  allOpen <- liftIO $ mapM isPortOpen ports
  unless (and allOpen) $ do
    H.annotate "Some ports are not open, trying again..."
    liftIO $ threadDelay 1_000_000 -- wait 1 sec
    void $ getOpenPorts (pred n) numberOfPorts
  pure ports

babbageTestnet :: BabbageTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
babbageTestnet testnetOptions H.Conf {..} = do
  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json")
    . encode $ defaultByronGenesisJsonValue

  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  createByronGenesis
    testnetMagic
    startTime
    testnetOptions
    (tempAbsPath </> "byron.genesis.spec.json")
    (tempAbsPath </> "byron-gen-command")


  -- Because in Babbage the overlay schedule and decentralization parameter
  -- are deprecated, we must use the "create-staked" cli command to create
  -- SPOs in the ShelleyGenesis

  alonzoBabbageTestGenesisJsonSourceFile <- H.noteShow $ base </> "scripts/babbage/alonzo-babbage-test-genesis.json"
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath </> "genesis.alonzo.spec.json"
  H.copyFile alonzoBabbageTestGenesisJsonSourceFile alonzoBabbageTestGenesisJsonTargetFile

  conwayBabbageTestGenesisJsonSourceFile <- H.noteShow $ base </> "scripts/babbage/conway-babbage-test-genesis.json"
  conwayBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath </> "genesis.conway.spec.json"
  H.copyFile conwayBabbageTestGenesisJsonSourceFile conwayBabbageTestGenesisJsonTargetFile

  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"

  case configurationTemplate of
    Just template -> H.readFile template >>= H.writeFile configurationFile
    Nothing ->
      -- TODO: We want to use a default value for the yaml file in this case.
      H.note "No configuration yaml template provided" >> H.failure

  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.delete "GenesisFile"
    . HM.insert "Protocol" (toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (toJSON @Double 0.6)
    . HM.insert "minSeverity" (toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (toJSON @String "genesis/byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (toJSON @String "genesis/shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (toJSON @String "genesis/shelley/genesis.alonzo.json")
    . HM.insert "ConwayGenesisFile" (toJSON @String "genesis/shelley/genesis.conway.json")
    . HM.insert "RequiresNetworkMagic" (toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (toJSON @Int 0)
    . HM.insert "TestShelleyHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAllegraHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestMaryHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAlonzoHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestBabbageHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "ExperimentalHardForksEnabled" (toJSON True)
    . flip HM.alter "setupScribes"
        ( fmap
          . J.rewriteArrayElements
            . J.rewriteObject
              . HM.insert "scFormat"
                $ case babbageNodeLoggingFormat testnetOptions of
                    NodeLoggingFormatAsJson -> "ScJson"
                    NodeLoggingFormatAsText -> "ScText")

  let numPoolNodes = 3 :: Int

  execCli_
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show @Int 3
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", "3"
    ]

  poolKeys <- H.noteShow $ flip fmap [1..numPoolNodes] $ \n ->
    PoolNodeKeys
      { poolNodeKeysColdVkey = tempAbsPath </> "pools" </> "cold" <> show n <> ".vkey"
      , poolNodeKeysColdSkey = tempAbsPath </> "pools" </> "cold" <> show n <> ".skey"
      , poolNodeKeysVrfVkey = tempAbsPath </> "node-spo" <> show n </> "vrf.vkey"
      , poolNodeKeysVrfSkey = tempAbsPath </> "node-spo" <> show n </> "vrf.skey"
      , poolNodeKeysStakingVkey = tempAbsPath </> "pools" </> "staking-reward" <> show n <> ".vkey"
      , poolNodeKeysStakingSkey = tempAbsPath </> "pools" </> "staking-reward" <> show n <> ".skey"
      }

  wallets <- forM [1..3] $ \idx -> do
    pure $ PaymentKeyPair
      { paymentSKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".skey"
      , paymentVKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".vkey"
      }

  delegators <- forM [1..3] $ \idx -> do
    pure $ Delegator
      { paymentKeyPair = PaymentKeyPair
        { paymentSKey = tempAbsPath </> "stake-delegator-keys/payment" <> show @Int idx <> ".skey"
        , paymentVKey = tempAbsPath </> "stake-delegator-keys/payment" <> show @Int idx <> ".vkey"
        }
      , stakingKeyPair = StakingKeyPair
        { stakingSKey = tempAbsPath </> "stake-delegator-keys/staking" <> show @Int idx <> ".skey"
        , stakingVKey = tempAbsPath </> "stake-delegator-keys/staking" <> show @Int idx <> ".vkey"
        }
      }

  let spoNodes :: [String] = ("node-spo" <>) . show <$> [1 .. babbageNumSpoNodes testnetOptions]

  -- Create the node directories

  forM_ spoNodes $ \node -> do
    H.createDirectoryIfMissing_ (tempAbsPath </> node)

  -- Here we move all of the keys etc generated by create-staked
  -- for the nodes to use

  -- Move all genesis related files

  genesisByronDir <- H.createDirectoryIfMissing $ tempAbsPath </> "genesis/byron"
  genesisShelleyDir <- H.createDirectoryIfMissing $ tempAbsPath </> "genesis/shelley"

  files <- H.listDirectory tempAbsPath
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath </> "byron-gen-command/genesis.json") (genesisByronDir </> "genesis.json")
  H.renameFile (tempAbsPath </> "genesis.alonzo.json") (genesisShelleyDir </> "genesis.alonzo.json")
  H.renameFile (tempAbsPath </> "genesis.conway.json") (genesisShelleyDir </> "genesis.conway.json")
  H.renameFile (tempAbsPath </> "genesis.json") (genesisShelleyDir </> "genesis.json")

  H.rewriteJsonFile (genesisByronDir </> "genesis.json") $ J.rewriteObject
    $ flip HM.adjust "protocolConsts"
      ( J.rewriteObject ( HM.insert "protocolMagic" (toJSON @Int testnetMagic)))

  H.rewriteJsonFile (genesisShelleyDir </> "genesis.json") $ J.rewriteObject
    ( HM.insert "slotLength"             (toJSON @Double 0.1)
    . HM.insert "activeSlotsCoeff"       (toJSON @Double 0.1)
    . HM.insert "securityParam"          (toJSON @Int $ babbageSecurityParam testnetOptions)
    . HM.insert "epochLength"            (toJSON @Int $ babbageEpochLength testnetOptions) -- Should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
    . HM.insert "maxLovelaceSupply"      (toJSON @Int 1000000000000)
    . HM.insert "minFeeA"                (toJSON @Int 44)
    . HM.insert "minFeeB"                (toJSON @Int 155381)
    . HM.insert "minUTxOValue"           (toJSON @Int 1000000)
    . HM.insert "decentralisationParam"  (toJSON @Double 0.7)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject ( HM.insert "major" (toJSON @Int $ babbageProtocolVersion testnetOptions)))
        )
      )
    . HM.insert "rho"                    (toJSON @Double 0.1)
    . HM.insert "tau"                    (toJSON @Double 0.1)
    . HM.insert "updateQuorum"           (toJSON @Int 2)
    )

  H.renameFile (tempAbsPath </> "pools/vrf1.skey") (tempAbsPath </> "node-spo1/vrf.skey")
  H.renameFile (tempAbsPath </> "pools/vrf2.skey") (tempAbsPath </> "node-spo2/vrf.skey")
  H.renameFile (tempAbsPath </> "pools/vrf3.skey") (tempAbsPath </> "node-spo3/vrf.skey")

  H.renameFile (tempAbsPath </> "pools/opcert1.cert") (tempAbsPath </> "node-spo1/opcert.cert")
  H.renameFile (tempAbsPath </> "pools/opcert2.cert") (tempAbsPath </> "node-spo2/opcert.cert")
  H.renameFile (tempAbsPath </> "pools/opcert3.cert") (tempAbsPath </> "node-spo3/opcert.cert")

  H.renameFile (tempAbsPath </> "pools/kes1.skey") (tempAbsPath </> "node-spo1/kes.skey")
  H.renameFile (tempAbsPath </> "pools/kes2.skey") (tempAbsPath </> "node-spo2/kes.skey")
  H.renameFile (tempAbsPath </> "pools/kes3.skey") (tempAbsPath </> "node-spo3/kes.skey")

  -- Byron related

  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.000.key") (tempAbsPath </> "node-spo1/byron-delegate.key")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.001.key") (tempAbsPath </> "node-spo2/byron-delegate.key")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.002.key") (tempAbsPath </> "node-spo3/byron-delegate.key")

  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.000.json") (tempAbsPath </> "node-spo1/byron-delegation.cert")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.001.json") (tempAbsPath </> "node-spo2/byron-delegation.cert")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.002.json") (tempAbsPath </> "node-spo3/byron-delegation.cert")

  [port1, port2, port3] <- getOpenPorts 60 $ babbageNumSpoNodes testnetOptions

  H.writeFile (tempAbsPath </> "node-spo1/port") (show port1)
  H.writeFile (tempAbsPath </> "node-spo2/port") (show port2)
  H.writeFile (tempAbsPath </> "node-spo3/port") (show port3)


  -- Make topology files
  -- TODO generalise this over the N BFT nodes and pool nodes

  H.lbsWriteFile (tempAbsPath </> "node-spo1/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port2
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port3
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo2/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port1
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port3
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo3/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port1
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port2
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  poolNodes <- forM (L.zip spoNodes poolKeys) $ \(node,key) -> do
    runtime <- startNode tempBaseAbsPath tempAbsPath logDir socketDir node
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--shelley-kes-key", tempAbsPath </> node </> "kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "vrf.skey"
        , "--byron-delegation-certificate", tempAbsPath </> node </> "byron-delegation.cert"
        , "--byron-signing-key", tempAbsPath </> node </> "byron-delegate.key"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "opcert.cert"
        ]
    return $ PoolNode runtime key

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ spoNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertChainExtended deadline (babbageNodeLoggingFormat testnetOptions) nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  forM_ wallets $ \wallet -> do
    H.cat $ paymentSKey wallet
    H.cat $ paymentVKey wallet

  return TestnetRuntime
    { configurationFile
    , shelleyGenesisFile = genesisShelleyDir </> "genesis.json"
    , testnetMagic
    , poolNodes
    , wallets = wallets
    , bftNodes = []
    , delegators = delegators
    }
