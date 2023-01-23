{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Babbage
  ( BabbageTestnetOptions(..)
  , defaultTestnetOptions
  , TestnetRuntime (..)
  , PaymentKeyPair(..)

  , babbageTestnet
  ) where

import           Control.Monad
import           Data.Aeson (encode, object, toJSON, (.=))
import           Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import           Prelude
import           System.FilePath.Posix ((</>))

import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified System.Info as OS

import           Testnet.Commands.Genesis
import qualified Testnet.Conf as H
import qualified Testnet.Util.Assert as H
import           Testnet.Util.Process (execCli_)
import           Testnet.Util.Runtime (Delegator (..), NodeLoggingFormat (..), PaymentKeyPair (..),
                   PoolNode (PoolNode), PoolNodeKeys (..), StakingKeyPair (..), TestnetRuntime (..),
                   startNode)


{- HLINT ignore "Redundant flip" -}

data BabbageTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes :: Int
  , babbageSlotDuration :: Int
  , babbageSecurityParam :: Int
  , babbageTotalBalance :: Int
  , babbageNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: BabbageTestnetOptions
defaultTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes = 3
  , babbageSlotDuration = 200
  , babbageSecurityParam = 10
  , babbageTotalBalance = 10020000000
  , babbageNodeLoggingFormat = NodeLoggingFormatAsJson
  }

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

babbageTestnet :: BabbageTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
babbageTestnet testnetOptions H.Conf {..} = do
  H.createDirectoryIfMissing (tempAbsPath </> "logs")

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json")
    . encode $ defaultByronGenesisJsonValue

  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int (babbageSecurityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (babbageNumSpoNodes testnetOptions)
    , "--total-balance", show @Int (babbageTotalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath </> "byron-gen-command"
    ]

  -- Because in Babbage the overlay schedule and decentralization parameter
  -- are deprecated, we must use the "create-staked" cli command to create
  -- SPOs in the ShelleyGenesis

  alonzoBabbageTestGenesisJsonSourceFile <- H.noteShow $ base </> "scripts/babbage/alonzo-babbage-test-genesis.json"
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath </> "genesis.alonzo.spec.json"

  H.copyFile alonzoBabbageTestGenesisJsonSourceFile alonzoBabbageTestGenesisJsonTargetFile

  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"

  H.readFile configurationTemplate >>= H.writeFile configurationFile

  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.delete "GenesisFile"
    . HM.insert "Protocol" (toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (toJSON @Double 0.6)
    . HM.insert "minSeverity" (toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (toJSON @String "genesis/byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (toJSON @String "genesis/shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (toJSON @String "genesis/shelley/genesis.alonzo.json")
    . HM.insert "RequiresNetworkMagic" (toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (toJSON @Int 0)
    . HM.insert "TestShelleyHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAllegraHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestMaryHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAlonzoHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestBabbageHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestEnableDevelopmentHardForkEras" (toJSON True)
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
    H.createDirectoryIfMissing (tempAbsPath </> node)

  -- Here we move all of the keys etc generated by create-staked
  -- for the nodes to use

  -- Move all genesis related files

  H.createDirectoryIfMissing $ tempAbsPath </> "genesis/byron"
  H.createDirectoryIfMissing $ tempAbsPath </> "genesis/shelley"

  files <- H.listDirectory tempAbsPath
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath </> "byron-gen-command/genesis.json") (tempAbsPath </> "genesis/byron/genesis.json")
  H.renameFile (tempAbsPath </> "genesis.alonzo.json") (tempAbsPath </> "genesis/shelley/genesis.alonzo.json")
  H.renameFile (tempAbsPath </> "genesis.json") (tempAbsPath </> "genesis/shelley/genesis.json")

  H.rewriteJsonFile (tempAbsPath </> "genesis/byron/genesis.json") $ J.rewriteObject
    $ flip HM.adjust "protocolConsts"
      ( J.rewriteObject ( HM.insert "protocolMagic" (toJSON @Int testnetMagic)))

  H.rewriteJsonFile (tempAbsPath </> "genesis/shelley/genesis.json") $ J.rewriteObject
    ( HM.insert "slotLength"             (toJSON @Double 0.1)
    . HM.insert "activeSlotsCoeff"       (toJSON @Double 0.1)
    . HM.insert "securityParam"          (toJSON @Int 10)     -- TODO: USE config parameter
    . HM.insert "epochLength"            (toJSON @Int 500)
    . HM.insert "maxLovelaceSupply"      (toJSON @Int 1000000000000)
    . HM.insert "minFeeA"                (toJSON @Int 44)
    . HM.insert "minFeeB"                (toJSON @Int 155381)
    . HM.insert "minUTxOValue"           (toJSON @Int 1000000)
    . HM.insert "decentralisationParam"  (toJSON @Double 0.7)
    . HM.insert "major"                  (toJSON @Int 7)
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

  H.writeFile (tempAbsPath </> "node-spo1/port") "3001"
  H.writeFile (tempAbsPath </> "node-spo2/port") "3002"
  H.writeFile (tempAbsPath </> "node-spo3/port") "3003"


  -- Make topology files
  -- TODO generalise this over the N BFT nodes and pool nodes

  H.lbsWriteFile (tempAbsPath </> "node-spo1/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3002
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3003
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo2/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3001
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3003
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo3/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3001
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3002
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
    , shelleyGenesisFile = tempAbsPath </> "genesis/shelley/genesis.json"
    , testnetMagic
    , poolNodes
    , wallets = wallets
    , bftNodes = []
    , delegators = delegators
    }
