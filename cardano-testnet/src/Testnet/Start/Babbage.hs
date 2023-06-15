{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Start.Babbage
  ( BabbageTestnetOptions(..)
  , TestnetRuntime (..)
  , PaymentKeyPair(..)

  , babbageTestnet
  , babbageDefaultTestnetOptions
  ) where

import           Prelude

import           Control.Monad
import           Data.Aeson (Value (..), encode, object, toJSON, (.=))
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import           System.FilePath.Posix ((</>))
import qualified System.Info as OS

import           Cardano.Api

import qualified Testnet.Conf as H
import           Testnet.Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli_)
import qualified Testnet.Property.Assert as H
import           Testnet.Property.Utils
import           Testnet.Runtime (Delegator (..), NodeLoggingFormat (..), PaymentKeyPair (..),
                   PoolNode (PoolNode), PoolNodeKeys (..), StakingKeyPair (..), TestnetRuntime (..),
                   startNode)
import qualified Testnet.Start.Byron as Byron

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Redundant flip" -}

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

data BabbageTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes :: Int
  , babbageSlotDuration :: Int
  , babbageSecurityParam :: Int
  , babbageTestnetMagic :: Int
  , babbageTotalBalance :: Int
  , babbageNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

babbageDefaultTestnetOptions :: BabbageTestnetOptions
babbageDefaultTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes = 3
  , babbageSlotDuration = 200
  , babbageSecurityParam = 10
  , babbageTestnetMagic = 42
  , babbageTotalBalance = 10020000000
  , babbageNodeLoggingFormat = NodeLoggingFormatAsJson
  }


babbageTestnet :: BabbageTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
babbageTestnet testnetOptions (H.Conf tempAbsPath) = do
  let logDir = makeLogDir tempAbsPath
      tempAbsPath' = unTmpAbsPath tempAbsPath
      testnetMagic = babbageTestnetMagic testnetOptions
  H.createDirectoryIfMissing_ logDir

  H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
    . encode $ defaultByronProtocolParamsJsonValue

  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  Byron.createByronGenesis
    testnetMagic
    startTime
    Byron.byronDefaultTestnetOptions
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "byron-gen-command")


  -- Because in Babbage the overlay schedule and decentralization parameter
  -- are deprecated, we must use the "create-staked" cli command to create
  -- SPOs in the ShelleyGenesis

  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> "genesis.alonzo.spec.json"
  gen <- H.evalEither $ first displayError defaultAlonzoGenesis
  H.evalIO $ LBS.writeFile alonzoBabbageTestGenesisJsonTargetFile $ encode gen

  conwayBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> "genesis.conway.spec.json"
  H.evalIO $ LBS.writeFile conwayBabbageTestGenesisJsonTargetFile $ encode defaultConwayGenesis

  configurationFile <- H.noteShow $ tempAbsPath' </> "configuration.yaml"

  let numPoolNodes = babbageNumSpoNodes testnetOptions

  execCli_
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath'
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show @Int 3
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", "3"
    ]

  poolKeys <- H.noteShow $ flip fmap [1..numPoolNodes] $ \n ->
    PoolNodeKeys
      { poolNodeKeysColdVkey = tempAbsPath' </> "pools" </> "cold" <> show n <> ".vkey"
      , poolNodeKeysColdSkey = tempAbsPath' </> "pools" </> "cold" <> show n <> ".skey"
      , poolNodeKeysVrfVkey = tempAbsPath' </> "node-spo" <> show n </> "vrf.vkey"
      , poolNodeKeysVrfSkey = tempAbsPath' </> "node-spo" <> show n </> "vrf.skey"
      , poolNodeKeysStakingVkey = tempAbsPath' </> "pools" </> "staking-reward" <> show n <> ".vkey"
      , poolNodeKeysStakingSkey = tempAbsPath' </> "pools" </> "staking-reward" <> show n <> ".skey"
      }

  wallets <- forM [1..3] $ \idx -> do
    pure $ PaymentKeyPair
      { paymentSKey = tempAbsPath' </> "utxo-keys/utxo" <> show @Int idx <> ".skey"
      , paymentVKey = tempAbsPath' </> "utxo-keys/utxo" <> show @Int idx <> ".vkey"
      }

  delegators <- forM [1..3] $ \idx -> do
    pure $ Delegator
      { paymentKeyPair = PaymentKeyPair
        { paymentSKey = tempAbsPath' </> "stake-delegator-keys/payment" <> show @Int idx <> ".skey"
        , paymentVKey = tempAbsPath' </> "stake-delegator-keys/payment" <> show @Int idx <> ".vkey"
        }
      , stakingKeyPair = StakingKeyPair
        { stakingSKey = tempAbsPath' </> "stake-delegator-keys/staking" <> show @Int idx <> ".skey"
        , stakingVKey = tempAbsPath' </> "stake-delegator-keys/staking" <> show @Int idx <> ".vkey"
        }
      }

  let spoNodes :: [String] = ("node-spo" <>) . show <$> [1 .. babbageNumSpoNodes testnetOptions]

  -- Create the node directories

  forM_ spoNodes $ \node -> do
    H.createDirectoryIfMissing_ (tempAbsPath' </> node)

  -- Here we move all of the keys etc generated by create-staked
  -- for the nodes to use

  -- Move all genesis related files

  genesisByronDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "byron"
  genesisShelleyDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "shelley"

  files <- H.listDirectory tempAbsPath'
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath' </> "byron-gen-command/genesis.json") (genesisByronDir </> "genesis.json")
  H.renameFile (tempAbsPath' </> "genesis.alonzo.json") (genesisShelleyDir </> "genesis.alonzo.json")
  H.renameFile (tempAbsPath' </> "genesis.conway.json") (genesisShelleyDir </> "genesis.conway.json")
  H.renameFile (tempAbsPath' </> "genesis.json") (genesisShelleyDir </> "genesis.json")

  H.rewriteJsonFile (genesisByronDir </> "genesis.json") $ J.rewriteObject
    $ flip HM.adjust "protocolConsts"
      ( J.rewriteObject ( HM.insert "protocolMagic" (toJSON @Int testnetMagic)))


  H.rewriteJsonFile (genesisShelleyDir </> "genesis.json") $ J.rewriteObject
    ( HM.insert "slotLength"             (toJSON @Double 0.1)
    . HM.insert "activeSlotsCoeff"       (toJSON @Double 0.1)
    . HM.insert "securityParam"          (toJSON @Int 6)    -- TODO: USE config parameter
    . HM.insert "epochLength"            (toJSON @Int 600)  -- Should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
    . HM.insert "maxLovelaceSupply"      (toJSON @Int 1000000000000)
    . HM.insert "minFeeA"                (toJSON @Int 44)
    . HM.insert "minFeeB"                (toJSON @Int 155381)
    . HM.insert "minUTxOValue"           (toJSON @Int 1000000)
    . HM.insert "decentralisationParam"  (toJSON @Double 0.7)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject ( HM.insert "major" (toJSON @Int 8)))
        )
      )
    . HM.insert "rho"                    (toJSON @Double 0.1)
    . HM.insert "tau"                    (toJSON @Double 0.1)
    . HM.insert "updateQuorum"           (toJSON @Int 2)
    )


  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  -- TODO: These genesis filepaths should not be hardcoded. Using the cli as a library
  -- rather as an executable will allow us to get the genesis files paths in a more
  -- direct fashion.

  byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.json") "ShelleyGenesisHash"
  alonzoGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.alonzo.json") "AlonzoGenesisHash"
  conwayGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.conway.json") "ConwayGenesisHash"


  let finalYamlConfig :: LBS.ByteString
      finalYamlConfig = encode . Object
                                 $ mconcat [ byronGenesisHash
                                           , shelleyGenesisHash
                                           , alonzoGenesisHash
                                           , conwayGenesisHash
                                           , defaultYamlHardforkViaConfig (AnyCardanoEra BabbageEra)]

  H.evalIO $ LBS.writeFile (tempAbsPath' </> "configuration.yaml") finalYamlConfig


  H.renameFile (tempAbsPath' </> "pools/vrf1.skey") (tempAbsPath' </> "node-spo1/vrf.skey")
  H.renameFile (tempAbsPath' </> "pools/vrf2.skey") (tempAbsPath' </> "node-spo2/vrf.skey")
  H.renameFile (tempAbsPath' </> "pools/vrf3.skey") (tempAbsPath' </> "node-spo3/vrf.skey")

  H.renameFile (tempAbsPath' </> "pools/opcert1.cert") (tempAbsPath' </> "node-spo1/opcert.cert")
  H.renameFile (tempAbsPath' </> "pools/opcert2.cert") (tempAbsPath' </> "node-spo2/opcert.cert")
  H.renameFile (tempAbsPath' </> "pools/opcert3.cert") (tempAbsPath' </> "node-spo3/opcert.cert")

  H.renameFile (tempAbsPath' </> "pools/kes1.skey") (tempAbsPath' </> "node-spo1/kes.skey")
  H.renameFile (tempAbsPath' </> "pools/kes2.skey") (tempAbsPath' </> "node-spo2/kes.skey")
  H.renameFile (tempAbsPath' </> "pools/kes3.skey") (tempAbsPath' </> "node-spo3/kes.skey")

  -- Byron related

  H.renameFile (tempAbsPath' </> "byron-gen-command/delegate-keys.000.key") (tempAbsPath' </> "node-spo1/byron-delegate.key")
  H.renameFile (tempAbsPath' </> "byron-gen-command/delegate-keys.001.key") (tempAbsPath' </> "node-spo2/byron-delegate.key")
  H.renameFile (tempAbsPath' </> "byron-gen-command/delegate-keys.002.key") (tempAbsPath' </> "node-spo3/byron-delegate.key")

  H.renameFile (tempAbsPath' </> "byron-gen-command/delegation-cert.000.json") (tempAbsPath' </> "node-spo1/byron-delegation.cert")
  H.renameFile (tempAbsPath' </> "byron-gen-command/delegation-cert.001.json") (tempAbsPath' </> "node-spo2/byron-delegation.cert")
  H.renameFile (tempAbsPath' </> "byron-gen-command/delegation-cert.002.json") (tempAbsPath' </> "node-spo3/byron-delegation.cert")

  H.writeFile (tempAbsPath' </> "node-spo1/port") "3001"
  H.writeFile (tempAbsPath' </> "node-spo2/port") "3002"
  H.writeFile (tempAbsPath' </> "node-spo3/port") "3003"


  -- Make topology files
  -- TODO generalise this over the N BFT nodes and pool nodes

  H.lbsWriteFile (tempAbsPath' </> "node-spo1/topology.json") $ encode $
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

  H.lbsWriteFile (tempAbsPath' </> "node-spo2/topology.json") $ encode $
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

  H.lbsWriteFile (tempAbsPath' </> "node-spo3/topology.json") $ encode $
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
    runtime <- startNode (TmpAbsolutePath tempAbsPath') node
        [ "run"
        , "--config", tempAbsPath' </> "configuration.yaml"
        , "--topology", tempAbsPath' </> node </> "topology.json"
        , "--database-path", tempAbsPath' </> node </> "db"
        , "--shelley-kes-key", tempAbsPath' </> node </> "kes.skey"
        , "--shelley-vrf-key", tempAbsPath' </> node </> "vrf.skey"
        , "--byron-delegation-certificate", tempAbsPath' </> node </> "byron-delegation.cert"
        , "--byron-signing-key", tempAbsPath' </> node </> "byron-delegate.key"
        , "--shelley-operational-certificate", tempAbsPath' </> node </> "opcert.cert"
        ]
    return $ PoolNode runtime key

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ spoNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile (makeLogDir $ TmpAbsolutePath tempAbsPath') $ node <> ".stdout.log"
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
