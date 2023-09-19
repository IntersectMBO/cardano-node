{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Start.Cardano
  ( ForkPoint(..)
  , CardanoTestnetOptions(..)
  , cardanoPoolNodes
  , cardanoBftNodes
  , cardanoNumPoolNodes
  , extraBftNodeCliArgs
  , TestnetNodeOptions(..)
  , cardanoDefaultTestnetOptions
  , cardanoDefaultTestnetNodeOptions

  , TestnetRuntime (..)
  , PaymentKeyPair(..)

  , cardanoTestnet
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Prelude hiding (lines)

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import           System.FilePath.Posix ((</>))
import qualified System.Info as OS

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import           Testnet.Components.Configuration
import qualified Testnet.Conf as H
import           Testnet.Defaults
import           Testnet.Filepath
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Assert as H
import           Testnet.Property.Checks
import           Testnet.Runtime as TR
import qualified Testnet.Start.Byron as Byron
import           Testnet.Start.Types

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}




data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)





-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


cardanoTestnet :: CardanoTestnetOptions -> H.Conf -> H.Integration TestnetRuntime
cardanoTestnet testnetOptions H.Conf {H.tempAbsPath} = do
  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  let testnetMagic = cardanoTestnetMagic testnetOptions

  let logDir = makeLogDir $ TmpAbsolutePath tempAbsPath'
  H.createDirectoryIfMissing_ logDir

  H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
    . J.encode $ defaultByronProtocolParamsJsonValue

  -- stuff
  Byron.createByronGenesis
    testnetMagic
    startTime
    Byron.byronDefaultTestnetOptions
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "byron-gen-command")
  -- Because in Conway the overlay schedule and decentralization parameter
  -- are deprecated, we must use the "create-staked" cli command to create
  -- SPOs in the ShelleyGenesis

  alonzoConwayTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> "genesis.alonzo.spec.json"
  gen <- H.evalEither $ first displayError defaultAlonzoGenesis
  H.evalIO $ LBS.writeFile alonzoConwayTestGenesisJsonTargetFile $ Aeson.encode gen

  conwayConwayTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> "genesis.conway.spec.json"
  H.evalIO $ LBS.writeFile conwayConwayTestGenesisJsonTargetFile $ Aeson.encode defaultConwayGenesis

  configurationFile <- H.noteShow $ tempAbsPath' </> "configuration.yaml"

  let numPoolNodes = cardanoNumPoolNodes $ cardanoNodes testnetOptions
  -- TODO: No need to use the executable directly. We need to wrap
  -- the function that create-staked called and parameterize it on CardanoTestnetOptions or TestnetOptions
  execCli_
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath'
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show @Int numPoolNodes
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", "3"
    ]

  poolKeys <- H.noteShow $ flip fmap [1..numPoolNodes] $ \n ->
    PoolNodeKeys
      { poolNodeKeysColdVkey = tempAbsPath' </> "pools" </> "cold" <> show n <> ".vkey"
      , poolNodeKeysColdSkey = tempAbsPath' </> "pools" </> "cold" <> show n <> ".skey"
      , poolNodeKeysVrfVkey = tempAbsPath' </> "pools" </> "vrf" <> show n <> ".vkey"
      , poolNodeKeysVrfSkey = tempAbsPath' </> "pools" </> "vrf" <> show n <> ".skey"
      , poolNodeKeysStakingVkey = tempAbsPath' </> "pools" </> "staking-reward" <> show n <> ".vkey"
      , poolNodeKeysStakingSkey = tempAbsPath' </> "pools" </> "staking-reward" <> show n <> ".skey"
      }

  wallets <- forM [1..3] $ \idx -> do
    let paymentSKeyFile = tempAbsPath' </> "utxo-keys/utxo" <> show @Int idx <> ".skey"
    let paymentVKeyFile = tempAbsPath' </> "utxo-keys/utxo" <> show @Int idx <> ".vkey"
    let paymentAddrFile = tempAbsPath' </> "utxo-keys/utxo" <> show @Int idx <> ".addr"

    execCli_
      [ "address", "build"
      , "--payment-verification-key-file", paymentVKeyFile
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", paymentAddrFile
      ]

    paymentAddr <- H.readFile paymentAddrFile

    pure $ PaymentKeyInfo
      { paymentKeyInfoPair = PaymentKeyPair
        { paymentSKey = paymentSKeyFile
        , paymentVKey = paymentVKeyFile
        }
      , paymentKeyInfoAddr = Text.pack paymentAddr
      }

  _delegators <- forM [1..3] $ \idx -> do
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

  let spoNodes :: [String] = ("node-spo" <>) . show <$> [1 .. numPoolNodes]

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
  finalYamlConfig <- createConfigYaml tempAbsPath $ cardanoEra testnetOptions

  H.evalIO $ LBS.writeFile configurationFile finalYamlConfig

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
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3005
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
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3005
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
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3005
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]
  let spoNodesWithPortNos = L.zip spoNodes [3001..]
  poolNodes <- forM (L.zip spoNodesWithPortNos poolKeys) $ \((node, port),key) -> do
    runtime <- startNode (TmpAbsolutePath tempAbsPath') node port
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
    H.assertChainExtended deadline (cardanoNodeLoggingFormat testnetOptions) nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  forM_ wallets $ \wallet -> do
    H.cat $ paymentSKey $ paymentKeyInfoPair wallet
    H.cat $ paymentVKey $ paymentKeyInfoPair wallet

  let runtime = TestnetRuntime
        { configurationFile
        , shelleyGenesisFile = genesisShelleyDir </> "genesis.json"
        , testnetMagic
        , poolNodes
        , wallets = wallets
        , bftNodes = []
        , delegators = []
        }

  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  execConfig <- H.headM (poolSprockets runtime) >>= H.mkExecConfig tempBaseAbsPath

  forM_ wallets $ \wallet -> do
    H.cat $ paymentSKey $ paymentKeyInfoPair wallet
    H.cat $ paymentVKey $ paymentKeyInfoPair wallet

    utxos <- execCli' execConfig
      [ "query", "utxo"
      , "--address", Text.unpack $ paymentKeyInfoAddr wallet
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      ]

    H.note_ utxos

  stakePoolsFp <- H.note $ tempAbsPath' </> "current-stake-pools.json"

  prop_spos_in_ledger_state stakePoolsFp testnetOptions execConfig

  pure runtime

