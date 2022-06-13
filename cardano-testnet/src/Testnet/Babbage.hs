{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Babbage
  ( TestnetOptions(..)
  , defaultTestnetOptions
  , TestnetNodeOptions(..)
  , defaultTestnetNodeOptions

  , TestnetRuntime (..)
  , TestnetNode (..)
  , Wallet(..)

  , testnet
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Lens ((.~))
import           Control.Monad (Monad (..), fmap, forM, forM_, return, void, when, (=<<))
import           Data.Aeson (encode, object, toJSON, (.=))
import           Data.Bool (Bool (..))
import           Data.Eq (Eq)
import           Data.Function (($), (.))
import           Data.Functor ((<$>), (<&>))
import           Data.Int (Int)
import           Data.List (unzip5, zipWith6, (++))
import           Data.Maybe (Maybe (Just))
import           Data.Ord (Ord ((<=)))
import           Data.Semigroup (Semigroup ((<>)))
import           Data.String (String)
import           GHC.Float (Double)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Show (Show (show))

import qualified Data.Aeson.Lens as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}


data TestnetOptions = TestnetOptions
  { numSpoNodes :: Int
  , slotDuration :: Int
  , securityParam :: Int
  , totalBalance :: Int
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { numSpoNodes = 3
  , slotDuration = 1000
  , securityParam = 10
  , totalBalance = 10020000000
  }

data TestnetNodeOptions = TestnetNodeOptions deriving (Eq, Show)

defaultTestnetNodeOptions :: TestnetNodeOptions
defaultTestnetNodeOptions = TestnetNodeOptions

data TestnetRuntime = TestnetRuntime
  { configurationFile :: FilePath
  , testnetMagic :: Int
  , poolNodes :: [TestnetNode]
  , wallets :: [Wallet]
  }

data TestnetNode = TestnetNode
  { nodeName :: String
  , nodeSprocket :: Sprocket
  , nodeStdinHandle :: IO.Handle
  , nodeStdout :: FilePath
  , nodeStderr :: FilePath
  , nodeProcessHandle :: IO.ProcessHandle
  }

data Wallet = Wallet
  { paymentVKey :: FilePath
  , paymentSKey :: FilePath
  } deriving (Eq, Show)

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

testnet :: TestnetOptions -> H.Conf -> H.Integration TestnetRuntime
testnet testnetOptions H.Conf {..} = do
  H.createDirectoryIfMissing (tempAbsPath </> "logs")

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json") . encode $ object
    [ "heavyDelThd"       .= ("300000000000" :: String)
    , "maxBlockSize"      .= ("2000000" :: String)
    , "maxTxSize"         .= ("4096" :: String)
    , "maxHeaderSize"     .= ("2000000" :: String)
    , "maxProposalSize"   .= ("700" :: String)
    , "mpcThd"            .= ("20000000000000" :: String)
    , "scriptVersion"     .= (0 :: Int)
    , "slotDuration"      .= show @Int (slotDuration testnetOptions)
    , "unlockStakeEpoch"  .= ("18446744073709551615" :: String)
    , "updateImplicit"    .= ("10000" :: String)
    , "updateProposalThd" .= ("100000000000000" :: String)
    , "updateVoteThd"     .= ("1000000000000" :: String)
    , "softforkRule" .= object
      [ "initThd" .= ("900000000000000" :: String)
      , "minThd" .= ("600000000000000" :: String)
      , "thdDecrement" .= ("50000000000000" :: String)
      ]
    , "txFeePolicy" .= object
      [ "multiplier" .= ("43946000000" :: String)
      , "summand" .= ("155381000000000" :: String)
      ]
    ]

  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  void . H.execCli $
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int (securityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (numSpoNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
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
    . HM.insert "Protocol"                              (toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold"                (toJSON @Double 0.6)
    . HM.insert "minSeverity"                           (toJSON @String "Debug")
    . HM.insert "ByronGenesisFile"                      (toJSON @String "genesis/byron/genesis.json")
    . HM.insert "ShelleyGenesisFile"                    (toJSON @String "genesis/shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile"                     (toJSON @String "genesis/shelley/genesis.alonzo.json")
    . HM.insert "RequiresNetworkMagic"                  (toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major"           (toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor"           (toJSON @Int 0)
    . HM.insert "TestShelleyHardForkAtEpoch"            (toJSON @Int 0)
    . HM.insert "TestAllegraHardForkAtEpoch"            (toJSON @Int 0)
    . HM.insert "TestMaryHardForkAtEpoch"               (toJSON @Int 0)
    . HM.insert "TestAlonzoHardForkAtEpoch"             (toJSON @Int 0)
    . HM.insert "TestBabbageHardForkAtEpoch"            (toJSON @Int 0)
    . HM.insert "TestEnableDevelopmentHardForkEras"     (toJSON True)
    . HM.insert "TestEnableDevelopmentNetworkProtocols" (toJSON True)

  -- Copy the cost model

  void . H.execCli $
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show @Int 3
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", "3"
    ]

  wallets <- forM [1..3] $ \idx -> do
    pure $ Wallet
      { paymentSKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".skey"
      , paymentVKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".vkey"
      }

  let spoNodes :: [String] = ("node-spo" ++) . show <$> [1 .. numSpoNodes testnetOptions]

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

  H.rewriteJsonFile (tempAbsPath </> "genesis/byron/genesis.json")
    $ J.key "protocolConsts" . J.key "protocolMagic" .~                              toJSON @Int testnetMagic

  H.rewriteJsonFile (tempAbsPath </> "genesis/shelley/genesis.json")
    ( (J.key "slotLength"             .~ toJSON @Double 0.1)
    . (J.key "activeSlotsCoeff"       .~ toJSON @Double 0.1)
    . (J.key "securityParam"          .~ toJSON @Int 10)
    . (J.key "epochLength"            .~ toJSON @Int 500)
    . (J.key "maxLovelaceSupply"      .~ toJSON @Int 1000000000000)
    . (J.key "minFeeA"                .~ toJSON @Int 44)
    . (J.key "minFeeB"                .~ toJSON @Int 155381)
    . (J.key "minUTxOValue"           .~ toJSON @Int 1000000)
    . (J.key "decentralisationParam"  .~ toJSON @Double 0.7)
    . (J.key "major"                  .~ toJSON @Int 7)
    . (J.key "rho"                    .~ toJSON @Double 0.1)
    . (J.key "tau"                    .~ toJSON @Double 0.1)
    . (J.key "updateQuorum"           .~ toJSON @Int 2)
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

  (poolSprockets, poolStdins, poolStdouts, poolStderrs, poolProcessHandles) <- fmap unzip5 . forM spoNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

    (Just stdIn, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath </> node </> "kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "vrf.skey"
        , "--byron-delegation-certificate", tempAbsPath </> node </> "byron-delegation.cert"
        , "--byron-signing-key", tempAbsPath </> node </> "byron-delegate.key"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "opcert.cert"
        , "--port",  portString
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

    return (sprocket, stdIn, nodeStdoutFile, nodeStderrFile, hProcess)

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ spoNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertByDeadlineIOCustom "stdout does not contain \"Chain extended\"" deadline $ IO.fileContains "Chain extended, new tip" nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  forM_ wallets $ \wallet -> do
    H.cat $ paymentSKey wallet
    H.cat $ paymentVKey wallet

  return TestnetRuntime
    { configurationFile
    , testnetMagic
    , poolNodes = zipWith6 TestnetNode
        spoNodes
        poolSprockets
        poolStdins
        poolStdouts
        poolStderrs
        poolProcessHandles
    , wallets = []
    }
