{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Start.Cardano
  ( ForkPoint(..)
  , CardanoTestnetOptions(..)
  , extraSpoNodeCliArgs
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
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import           Data.Either
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import qualified GHC.Stack as GHC
import           System.FilePath.Posix ((</>))
import qualified System.Info as OS

import qualified Hedgehog as H
import           Hedgehog.Extras (failMessage)
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import qualified Control.Monad.Class.MonadTimer.SI as MT
import           Control.Monad.IO.Class
import           Testnet.Components.Configuration
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


-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testnetMinimumConfigurationRequirements :: CardanoTestnetOptions -> H.Integration ()
testnetMinimumConfigurationRequirements cTestnetOpts = do
  when (length (cardanoNodes cTestnetOpts) < 2) $ do
     H.noteShow_ ("Need at least two nodes to run a cluster" :: String)
     H.noteShow_ cTestnetOpts
     H.assert False

data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)


-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15


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
-- > │   ├── delegate{1,2,3}.counter
-- > │   ├── delegate{1,2,3}.kes.{skey,vkey}
-- > │   ├── delegate{1,2,3}.{kes,vrf}.{skey,vkey}
-- > │   └── opcert{1,2,3}.cert
-- > ├── genesis.alonzo.spec.json
-- > ├── genesis.conway.spec.json
-- > ├── genesis-keys
-- > │   └── genesis{1,2,3}.{skey,vkey}
-- > ├── genesis.spec.json
-- > ├── node-spo{1,2,3}
-- > │   ├── byron-delegate.key
-- > │   ├── byron-delegation.cert
-- > │   ├── db
-- > │   │   └── ...
-- > │   ├── kes.skey
-- > │   ├── opcert.cert
-- > │   ├── port
-- > │   ├── topology.json
-- > │   └── vrf.skey
-- > ├── pools
-- > │   ├── cold{1,2,3}.{skey,vkey}
-- > │   ├── kes{1,2,3}.vkey
-- > │   ├── opcert{1,2,3}.counter
-- > │   ├── staking-reward{1,2,3}.{skey,vkey}
-- > │   └── vrf{1,2,3}.vkey
-- > ├── shelley
-- > │   ├── genesis.{alonzo,conway}.json
-- > │   └── genesis.json
-- > ├── socket
-- > │   └── node-spo{1,2,3}
-- > └── utxo-keys
-- >     └── utxo{1,2,3}.{addr,skey,vkey}
cardanoTestnet :: CardanoTestnetOptions -> Conf -> H.Integration TestnetRuntime
cardanoTestnet testnetOptions Conf {tempAbsPath} = do
  testnetMinimumConfigurationRequirements testnetOptions
  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      testnetMagic = cardanoTestnetMagic testnetOptions
      logDir = makeLogDir $ TmpAbsolutePath tempAbsPath'
      numPoolNodes = length $ cardanoNodes testnetOptions

  if all isJust [mconfig | SpoTestnetNodeOptions mconfig _ <- cardanoNodes testnetOptions]
  then
    -- TODO: We need a very simple non-obscure way of generating the files necessary
    -- to run a testnet. "create-staked" is not a good way to do this especially because it
    -- makes assumptions about where things should go and where genesis template files should be.
    -- See all of the ad hoc file creation/renaming/dir creation etc below.
    H.failMessage GHC.callStack "Specifying node configuration files per node not supported yet."
  else do
    startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

    H.createDirectoryIfMissing_ logDir

    H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
      . J.encode $ defaultByronProtocolParamsJsonValue

    -- Because in Conway the overlay schedule and decentralization parameter
    -- are deprecated, we must use the "create-staked" cli command to create
    -- SPOs in the ShelleyGenesis
    Byron.createByronGenesis
      testnetMagic
      startTime
      Byron.byronDefaultGenesisOptions
      (tempAbsPath' </> "byron.genesis.spec.json")
      (tempAbsPath' </> "byron-gen-command")


    alonzoConwayTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> "genesis.alonzo.spec.json"
    gen <- H.evalEither $ first prettyError defaultAlonzoGenesis
    H.evalIO $ LBS.writeFile alonzoConwayTestGenesisJsonTargetFile $ Aeson.encode gen

    conwayConwayTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath' </> "genesis.conway.spec.json"
    H.evalIO $ LBS.writeFile conwayConwayTestGenesisJsonTargetFile $ Aeson.encode defaultConwayGenesis

    configurationFile <- H.noteShow $ tempAbsPath' </> "configuration.yaml"

    genesisShelleyDir <- createSPOGenesisAndFiles testnetOptions startTime (TmpAbsolutePath tempAbsPath')

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


    -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
    finalYamlConfig <- createConfigYaml tempAbsPath $ cardanoNodeEra testnetOptions

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
    nodeStdoutFiles <- forM spoNodes $ \node -> do
                        H.noteTempFile (makeLogDir $ TmpAbsolutePath tempAbsPath') $ node <> ".stdout.log"

    let spoNodesWithPortNos = L.zip spoNodes [3001..]
        nodeConfigFile = tempAbsPath' </> "configuration.yaml"
    ePoolNodes <- forM (L.zip spoNodesWithPortNos poolKeys) $ \((node, port),key) -> do
      eRuntime <- lift . lift . runExceptT $ startNode (TmpAbsolutePath tempAbsPath') node port testnetMagic
                                  [ "run"
                                  , "--config", nodeConfigFile
                                  , "--topology", tempAbsPath' </> node </> "topology.json"
                                  , "--database-path", tempAbsPath' </> node </> "db"
                                  , "--shelley-kes-key", tempAbsPath' </> node </> "kes.skey"
                                  , "--shelley-vrf-key", tempAbsPath' </> node </> "vrf.skey"
                                  , "--byron-delegation-certificate", tempAbsPath' </> node </> "byron-delegation.cert"
                                  , "--byron-signing-key", tempAbsPath' </> node </> "byron-delegate.key"
                                  , "--shelley-operational-certificate", tempAbsPath' </> node </> "opcert.cert"
                                  ]
      return $ flip PoolNode key <$> eRuntime

    if any isLeft ePoolNodes
    -- TODO: We can render this in a nicer way
    then failMessage GHC.callStack . show . map show $ lefts ePoolNodes
    else do
      let (_ , poolNodes) = partitionEithers ePoolNodes

      -- FIXME: replace with ledger events waiting for chain extensions
      liftIO $ MT.threadDelay 10
      now <- H.noteShowIO DTC.getCurrentTime
      deadline <- H.noteShow $ DTC.addUTCTime 35 now

      forM_ nodeStdoutFiles $ \nodeStdoutFile -> do
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

