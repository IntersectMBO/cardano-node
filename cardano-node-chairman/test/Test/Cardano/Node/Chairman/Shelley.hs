{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Node.Chairman.Shelley
  ( tests
  ) where

import           Chairman.IO.Network.Sprocket (Sprocket (..))
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.=))
import           Data.Bool
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           GHC.Num
import           Hedgehog (Property, discover)
import           Prelude (fromIntegral)
import           System.IO (IO)
import           Text.Show

import qualified Chairman.Base as H
import qualified Chairman.IO.Network.Socket as IO
import qualified Chairman.IO.Network.Sprocket as IO
import qualified Chairman.Process as H
import qualified Chairman.Time as H
import qualified Data.Aeson as J
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Data.Vector as DV
import qualified Hedgehog as H
import qualified System.FilePath.Posix as FP
import qualified System.IO as IO
import qualified System.Process as IO

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}

prop_spawnShelleyCluster :: Property
prop_spawnShelleyCluster = H.propertyOnce . H.workspace "chairman" $ \tempAbsPath -> do
  let nodeCount = 3
  let nodeIndexes = [1..nodeCount]
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  base <- H.noteShowM H.getProjectBase
  baseConfig <- H.noteShow $ base <> "/configuration/defaults/byron-mainnet"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 60 currentTime -- 10 seconds into the future
  formattedStartTime <- H.noteShow $ H.formatIso8601 startTime -- 10 seconds into the future
  socketDir <- H.noteShow $ tempRelPath <> "/socket"
  genesisDir <- H.noteShow $ tempAbsPath <> "/genesis"
  genesisKeysDir <- H.noteShow $ tempAbsPath <> "/genesis/genesis-keys"
  delegateKeysDir <- H.noteShow $ tempAbsPath <> "/genesis/delegate-keys"
  utxoKeysDir <- H.noteShow $ tempAbsPath <> "/genesis/utxo-keys"

  -- Generate genesis file and keys (Making a genesis file semi-automagically)
  void $ H.execCli
    [ "shelley", "genesis", "create"
    , "--genesis-dir", genesisDir
    , "--testnet-magic", "12345"
    , "--supply", "1000000000"
    , "--gen-genesis-keys", "3"
    , "--gen-utxo-keys", "3"
    , "--start-time", formattedStartTime
    ]

  _ <- H.noteShowM $ H.listDirectory genesisDir
  _ <- H.noteShowM $ H.listDirectory genesisKeysDir
  _ <- H.noteShowM $ H.listDirectory delegateKeysDir
  _ <- H.noteShowM $ H.listDirectory utxoKeysDir

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    nodeDir <- H.noteShow $ tempAbsPath <> "/node" <> si
    H.createDirectoryIfMissing $ tempAbsPath <> "/node" <> si

    -- KES Keys
    void $ H.execCli
      [ "shelley", "node", "key-gen-KES"
      , "--verification-key-file", nodeDir <> "/kes.vkey"
      , "--signing-key-file", nodeDir <> "/kes.skey"
      ]

    -- VRF Keys
    void $ H.execCli
      [ "shelley", "node", "key-gen-VRF"
      , "--verification-key-file", nodeDir <> "/vrf.vkey"
      , "--signing-key-file", nodeDir <> "/vrf.skey"
      ]

    -- Genesis delegate keys
    void $ H.execCli
      [ "shelley", "genesis", "key-gen-delegate"
      , "--verification-key-file", delegateKeysDir <> "/delegate" <> si <> ".vkey"
      , "--signing-key-file", delegateKeysDir <> "/delegate" <> si <> ".skey"
      , "--operational-certificate-issue-counter", delegateKeysDir <> "/delegate-opcert" <> si <> ".counter"
      ]

    void $ H.execCli
      [ "shelley", "node", "issue-op-cert"
      , "--hot-kes-verification-key-file", nodeDir <> "/kes.vkey"
      , "--cold-signing-key-file", delegateKeysDir <> "/delegate" <> si <> ".skey"
      , "--operational-certificate-issue-counter", delegateKeysDir <> "/delegate-opcert" <> si <> ".counter"
      , "--kes-period", "0"
      , "--out-file", nodeDir <> "/cert"
      ]

    void . H.noteShowM $ H.listDirectory nodeDir

    H.lbsWriteFile (nodeDir <> "/topology.json") . J.encode $ J.object
      [ "Producers" .= J.Array
        ( DV.fromList
          [ J.object
            [ "addr" .= J.String "127.0.0.1"
            , "port" .= J.Number (3000 + fromIntegral i)
            , "valency" .= J.Number 1
            ]
          ]
        )
      ]

    return ()

  configFile <- H.noteShow $ tempAbsPath <> "/configuration.yaml"

  H.readFile (baseConfig <> "/configuration.yaml")
    <&> rewriteConfiguration
    >>= H.writeFile configFile

  _ <- H.noteShowM $ H.listDirectory tempAbsPath

  handles <- forM nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    dbDir <- H.noteShow $ tempAbsPath <> "/db/node-" <> si
    nodeStdoutFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/node-" <> si)
    portString <- H.noteShow $ "300" <> si <> ""
    topologyFile <- H.noteShow $ tempAbsPath <> "/node" <> si <> "/topology.json"
    shelleyKesKeyFile <- H.noteShow $ tempAbsPath <> "/node" <> si <> "/kes.skey"
    shelleyVrfKeyFile <- H.noteShow $ tempAbsPath <> "/node" <> si <> "/vrf.skey"
    shelleyOperationalCertificateFile <- H.noteShow $ tempAbsPath <> "/node" <> si <> "/cert"

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.evalM . liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalM . liftIO $ IO.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    _ <- H.noteShowIO DTC.getCurrentTime

    (_, _, _, handle, _) <-  H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--database-path", dbDir
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--port", portString
        , "--topology", topologyFile
        , "--config", configFile
        , "--shelley-kes-key", shelleyKesKeyFile
        , "--shelley-vrf-key", shelleyVrfKeyFile
        , "--shelley-operational-certificate", shelleyOperationalCertificateFile
        , "--shutdown-ipc", "0"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    return handle

  deadline <- H.noteShowIO $ DTC.addUTCTime 70 <$> DTC.getCurrentTime -- 60 seconds from now

  forM_ nodeIndexes $ \i -> H.assertByDeadlineIO deadline $ IO.isPortOpen (3000 + i)

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/node-" <> si)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.assertIO $ IO.doesSprocketExist sprocket

  H.threadDelay 20000000

  -- The cluster should still be running at this point
  forM_ handles $ \handle -> do
    result <- H.evalIO $ IO.getProcessExitCode handle
    H.assert (isNothing result)

  _ <- H.noteShowIO DTC.getCurrentTime

  H.failure

rewriteConfiguration :: String -> String
rewriteConfiguration = L.unlines . fmap rewrite . L.lines
  where rewrite :: String -> String
        rewrite "Protocol: RealPBFT" = "Protocol: TPraos"
        rewrite "minSeverity: Info" = "minSeverity: Debug"
        rewrite "GenesisFile: genesis.json" = "GenesisFile: genesis/genesis.json"
        rewrite s = s

tests :: IO Bool
tests = H.checkParallel $$discover
