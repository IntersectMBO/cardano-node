{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.Cardano.Node.Chairman.Shelley
  ( tests
  ) where

import           Chairman.Aeson
import           Chairman.IO.Network.Sprocket (Sprocket (..))
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Bool
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.HashMap.Lazy (HashMap)
import           Data.Int
import           Data.Map (Map)
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String (String)
import           Data.Text (Text)
import           GHC.Float
import           GHC.Num
import           Hedgehog (Property, discover, (===))
import           System.Exit (ExitCode (..))
import           System.IO (IO)
import           Text.Read
import           Text.Show

import qualified Chairman.Base as H
import qualified Chairman.IO.File as IO
import qualified Chairman.IO.Network.Socket as IO
import qualified Chairman.IO.Network.Sprocket as IO
import qualified Chairman.Process as H
import qualified Chairman.String as S
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.FilePath.Posix as FP
import qualified System.IO as IO
import qualified System.Process as IO

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Reduce duplication" -}

rewriteGenesisSpec :: Int -> Value -> Value
rewriteGenesisSpec supply =
  rewriteObject
    $ HM.insert "activeSlotsCoeff" (toJSON @Double 0.1)
    . HM.insert "securityParam" (toJSON @Int 10)
    . HM.insert "epochLength" (toJSON @Int 1500)
    . HM.insert "maxLovelaceSupply" (toJSON supply)
    . flip HM.adjust "protocolParams"
      ( rewriteObject (HM.insert "decentralisationParam" (toJSON @Double 0.7))
      )

prop_spawnShelleyCluster :: Property
prop_spawnShelleyCluster = H.propertyOnce . H.workspace "chairman" $ \tempAbsPath -> do
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  base <- H.noteShowM H.getProjectBase
  baseConfig <- H.noteShow $ base <> "/configuration/chairman/defaults/simpleview"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 10 currentTime -- 10 seconds into the future
  socketDir <- H.noteShow $ tempRelPath <> "/socket"
  env <- H.evalIO IO.getEnvironment

  let praosNodes = ["node-praos1", "node-praos2"] :: [String]
  let praosNodesN = ["1", "2"] :: [String]
  let poolNodes = ["node-pool1"] :: [String]
  let allNodes = praosNodes <> poolNodes :: [String]
  let numPraosNodes = 3 :: Int

  let userAddrs = ["user1"]
  let poolAddrs = ["pool-owner1"]
  let addrs = userAddrs <> poolAddrs
  let testnetMagic = "42"

  H.copyFile
    (base <> "/configuration/chairman/shelly-only/configuration.yaml")
    (tempAbsPath <> "/configuration.yaml")

  -- Set up our template
  void $ H.execCli
    [ "shelley", "genesis", "create"
    , "--testnet-magic", testnetMagic
    , "--genesis-dir", tempAbsPath
    ]

  -- Then edit the genesis.spec.json ...
  let supply = 1000000000

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJson (tempAbsPath <> "/genesis.spec.json") (rewriteGenesisSpec supply)

  _ <- H.noteShowM . H.evalM $ eitherDecode @Value <$> H.lbsReadFile (tempAbsPath <> "/genesis.spec.json")

  -- Now generate for real
  void $ H.execCli
    [ "shelley", "genesis", "create"
    , "--testnet-magic", testnetMagic
    , "--genesis-dir", tempAbsPath
    , "--gen-genesis-keys", show numPraosNodes
    , "--gen-utxo-keys", "1"
    ]

  forM_ allNodes $ \p -> H.createDirectoryIfMissing $ tempAbsPath <> "/" <> p

  -- Make the pool operator cold keys
  -- This was done already for the BFT nodes as part of the genesis creation
  forM_ poolNodes $ \n -> do
    void $ H.execCli
      [ "shelley", "node", "key-gen"
      , "--cold-verification-key-file", tempAbsPath <> "/" <> n <> "/operator.vkey"
      , "--cold-signing-key-file", tempAbsPath <> "/" <> n <> "/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath <> "/" <> n <> "/operator.counter"
      ]

    void $ H.execCli
      [ "shelley", "node", "key-gen-VRF"
      , "--verification-key-file", tempAbsPath <> "/" <> n <> "/vrf.vkey"
      , "--signing-key-file", tempAbsPath <> "/" <> n <> "/vrf.skey"
      ]

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ praosNodesN $ \n -> do
    H.createFileLink (tempAbsPath <> "/delegate-keys/delegate" <> n <> ".skey") (tempAbsPath <> "/node-praos" <> n <> "/operator.skey")
    H.createFileLink (tempAbsPath <> "/delegate-keys/delegate" <> n <> ".vkey") (tempAbsPath <> "/node-praos" <> n <> "/operator.vkey")
    H.createFileLink (tempAbsPath <> "/delegate-keys/delegate" <> n <> ".counter") (tempAbsPath <> "/node-praos" <> n <> "/operator.counter")
    H.createFileLink (tempAbsPath <> "/delegate-keys/delegate" <> n <> ".vrf.vkey") (tempAbsPath <> "/node-praos" <> n <> "/vrf.vkey")
    H.createFileLink (tempAbsPath <> "/delegate-keys/delegate" <> n <> ".vrf.skey") (tempAbsPath <> "/node-praos" <> n <> "/vrf.skey")

  --  Make hot keys and for all nodes
  forM_ allNodes $ \node -> do
    void $ H.execCli
      [ "shelley", "node", "key-gen-KES"
      , "--verification-key-file", tempAbsPath <> "/" <> node <> "/kes.vkey"
      , "--signing-key-file", tempAbsPath <> "/" <> node <> "/kes.skey"
      ]

    void $ H.execCli
      [ "shelley", "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", tempAbsPath <> "/" <> node <> "/kes.vkey"
      , "--cold-signing-key-file", tempAbsPath <> "/" <> node <> "/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath <> "/" <> node <> "/operator.counter"
      , "--out-file", tempAbsPath <> "/" <> node <> "/node.cert"
      ]


  -- Make topology files
  -- TODO generalise this over the N BFT nodes and pool nodes
  H.writeFile (tempAbsPath <> "/node-praos1/topology.json") "\
    \{ \"Producers\":\
    \  [ { \"addr\": \"127.0.0.1\"\
    \    , \"port\": 3002\
    \    , \"valency\": 1\
    \    }\
    \  , { \"addr\": \"127.0.0.1\"\
    \    , \"port\": 3003\
    \    , \"valency\": 1\
    \    }\
    \  ]\
    \}"
  H.writeFile (tempAbsPath <> "/node-praos1/port") "3001"

  H.writeFile (tempAbsPath <> "/node-praos2/topology.json") "\
    \{ \"Producers\":\
    \  [ { \"addr\": \"127.0.0.1\"\
    \    , \"port\": 3001\
    \    , \"valency\": 1\
    \    }\
    \  , { \"addr\": \"127.0.0.1\"\
    \    , \"port\": 3003\
    \    , \"valency\": 1\
    \    }\
    \  ]\
    \}"
  H.writeFile (tempAbsPath <> "/node-praos2/port") "3002"

  H.writeFile (tempAbsPath <> "/node-pool1/topology.json") "\
    \{ \"Producers\":\
    \  [ { \"addr\": \"127.0.0.1\"\
    \    , \"port\": 3001\
    \    , \"valency\": 1\
    \    }\
    \  , { \"addr\": \"127.0.0.1\"\
    \    , \"port\": 3002\
    \    , \"valency\": 1\
    \    }\
    \  ]\
    \}"
  H.writeFile (tempAbsPath <> "/node-pool1/port") "3003"

  -- Generated node operator keys (cold, hot) and operational certs
  forM_ allNodes $ \n -> H.noteShowM_ . H.listDirectory $ tempAbsPath <> "/" <> n

  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards
  H.createDirectoryIfMissing $ tempAbsPath <> "/addresses"

  forM_ addrs $ \addr -> do
    -- Payment address keys
    void $ H.execCli
      [ "shelley", "address", "key-gen"
      , "--verification-key-file", tempAbsPath <> "/addresses/" <> addr <> ".vkey"
      , "--signing-key-file", tempAbsPath <> "/addresses/" <> addr <> ".skey"
      ]

    -- Stake address keys
    void $ H.execCli
      [ "shelley", "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.vkey"
      , "--signing-key-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.skey"
      ]

    -- Payment addresses
    void $ H.execCli
      [ "shelley", "address", "build"
      , "--payment-verification-key-file", tempAbsPath <> "/addresses/" <> addr <> ".vkey"
      , "--stake-verification-key-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", testnetMagic
      , "--out-file", tempAbsPath <> "/addresses/" <> addr <> ".addr"
      ]

    -- Stake addresses
    void $ H.execCli
      [ "shelley", "stake-address", "build"
      , "--stake-verification-key-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", testnetMagic
      , "--out-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.addr"
      ]

    -- Stake addresses registration certs
    void $ H.execCli
      [ "shelley", "stake-address", "registration-certificate"
      , "--stake-verification-key-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.vkey"
      , "--out-file", tempAbsPath <> "/addresses/" <> addr <> "-stake.reg.cert"
      ]

  -- User N will delegate to pool N
  let userPoolN = ["1"]

  forM_ userPoolN $ \n -> do
    -- Stake address delegation certs
    void $ H.execCli
      [ "shelley", "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tempAbsPath <> "/addresses/user" <> n <> "-stake.vkey"
      , "--cold-verification-key-file", tempAbsPath <> "/node-pool" <> n <> "/operator.vkey"
      , "--out-file", tempAbsPath <> "/addresses/user" <> n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tempAbsPath <> "/addresses/pool-owner" <> n <> "-stake.vkey") (tempAbsPath <> "/node-pool" <> n <> "/owner.vkey")
    H.createFileLink (tempAbsPath <> "/addresses/pool-owner" <> n <> "-stake.skey") (tempAbsPath <> "/node-pool" <> n <> "/owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address regitration certs, and stake address delegatation certs
  H.noteShowM_ . H.listDirectory $ tempAbsPath <> "/addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodes $ \node -> do
    void $ H.execCli
      [ "shelley", "stake-pool", "registration-certificate"
      , "--testnet-magic", testnetMagic
      , "--pool-pledge", "0"
      , "--pool-cost", "0"
      , "--pool-margin", "0"
      , "--cold-verification-key-file", tempAbsPath <> "/" <> node <> "/operator.vkey"
      , "--vrf-verification-key-file", tempAbsPath <> "/" <> node <> "/vrf.vkey"
      , "--reward-account-verification-key-file", tempAbsPath <> "/" <> node <> "/owner.vkey"
      , "--pool-owner-stake-verification-key-file", tempAbsPath <> "/" <> node <> "/owner.vkey"
      , "--out-file", tempAbsPath <> "/" <> node <> "/registration.cert"
      ]

  -- Generated stake pool registration certs:
  forM_ poolNodes $ \node -> H.assertIO . IO.doesFileExist $ tempAbsPath <> "/" <> node <> "/registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  -- We'll transfer all the funds to the user1, which delegates to pool1
  -- We'll register certs to:
  --  1. register the pool-owner1 stake address
  --  2. register the stake pool 1
  --  3. register the user1 stake address
  --  4. delegate from the user1 stake address to the stake pool
  genesisTxinResult <- H.noteShowM $ S.strip <$> H.execCli
    [ "shelley", "genesis", "initial-txin"
    , "--testnet-magic", testnetMagic
    , "--verification-key-file", tempAbsPath <> "/utxo-keys/utxo1.vkey"
    ]

  user1Addr <- H.readFile $ tempAbsPath <> "/addresses/user1.addr"

  void $ H.execCli
    [ "shelley", "transaction", "build-raw"
    , "--ttl", "1000"
    , "--fee", "0"
    , "--tx-in", genesisTxinResult
    , "--tx-out", user1Addr <> "+" <> show supply
    , "--certificate-file", tempAbsPath <> "/addresses/pool-owner1-stake.reg.cert"
    , "--certificate-file", tempAbsPath <> "/node-pool1/registration.cert"
    , "--certificate-file", tempAbsPath <> "/addresses/user1-stake.reg.cert"
    , "--certificate-file", tempAbsPath <> "/addresses/user1-stake.deleg.cert"
    , "--out-file", tempAbsPath <> "/tx1.txbody"
    ]

  -- So we'll need to sign this with a bunch of keys:
  -- 1. the initial utxo spending key, for the funds
  -- 2. the user1 stake address key, due to the delegatation cert
  -- 3. the pool1 owner key, due to the pool registration cert
  -- 3. the pool1 operator key, due to the pool registration cert

  void $ H.execCli
    [ "shelley", "transaction", "sign"
    , "--signing-key-file", tempAbsPath <> "/utxo-keys/utxo1.skey"
    , "--signing-key-file", tempAbsPath <> "/addresses/user1-stake.skey"
    , "--signing-key-file", tempAbsPath <> "/node-pool1/owner.skey"
    , "--signing-key-file", tempAbsPath <> "/node-pool1/operator.skey"
    , "--testnet-magic", testnetMagic
    , "--tx-body-file", tempAbsPath <> "/tx1.txbody"
    , "--out-file", tempAbsPath <> "/tx1.tx"
    ]

  -- Generated a signed 'do it all' transaction:
  H.assertIO . IO.doesFileExist $ tempAbsPath <> "/tx1.tx"

  --------------------------------
  -- Launch cluster of three nodes

  logDir <- H.noteTempFile tempAbsPath "/logs"

  H.createDirectoryIfMissing logDir

  forM_ allNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath <> "/db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.evalM . liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalM . liftIO $ IO.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- H.readFile $ tempAbsPath <> "/" <> node <> "/port"

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath <> "/configuration.yaml"
        , "--topology", tempAbsPath <> "/" <> node <> "/topology.json"
        , "--database-path", tempAbsPath <> "/" <> node <> "/db"
        , "--shelley-kes-key", tempAbsPath <> "/" <> node <> "/kes.skey"
        , "--shelley-vrf-key", tempAbsPath <> "/" <> node <> "/vrf.skey"
        , "--shelley-operational-certificate" , tempAbsPath <> "/" <> node <> "/node.cert"
        , "--port", portString
        , "--socket-path", IO.sprocketArgumentName sprocket
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

  H.noteShowIO_ DTC.getCurrentTime

  deadline <- H.noteShowIO $ DTC.addUTCTime 60 <$> DTC.getCurrentTime -- 60 seconds from now

  forM_ allNodes $ \node -> do
    portString <- H.noteShowM . H.readFile $ tempAbsPath <> "/" <> node <> "/port"
    H.assertByDeadlineIO deadline $ IO.isPortOpen (read portString)

  forM_ allNodes $ \node -> do
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.assertIO $ IO.doesSprocketExist sprocket

  forM_ allNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertByDeadlineIO deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile
    H.assertByDeadlineIO deadline $ IO.fileContains "Chain extended, new tip" nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  -- Run chairman
  forM_ (L.take 1 allNodes) $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.evalM . liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalM . liftIO $ IO.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procChairman
        [ "--timeout", "100"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--config", tempAbsPath <> "/configuration.yaml"
        , "--security-parameter", "2160"
        , "--testnet-magic", testnetMagic
        , "--slot-length", "20"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    void $ H.waitSecondsForProcess 110 hProcess

    H.cat nodeStdoutFile
    H.cat nodeStderrFile

tests :: IO Bool
tests = H.checkParallel $$discover
