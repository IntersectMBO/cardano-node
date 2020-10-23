{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Byron
  ( testnet
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson ((.=))
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.List ((\\))
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           GHC.Float
import           GHC.Num
import           GHC.Real
import           Hedgehog (Property, discover, (===))
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time
import           System.Exit (ExitCode (..))
import           System.FilePath.Posix ((</>))
import           System.IO (IO)
import           Text.Read
import           Text.Show

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.FilePath.Posix as FP
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified System.Random as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

-- | Rewrite a line in the configuration file
rewriteConfiguration :: String -> String
rewriteConfiguration "TraceBlockchainTime: False" = "TraceBlockchainTime: True"
rewriteConfiguration s = s

testnet :: H.Conf -> H.Integration [String]
testnet H.Conf {..} = do
  void $ H.note OS.os
  let nodeCount = 3
  baseConfig <- H.noteShow $ base </> "configuration/chairman/defaults/simpleview"
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 10 currentTime -- 10 seconds into the future
  allPorts <- H.noteShowIO $ IO.allocateRandomPorts nodeCount

  -- Generate keys
  void $ H.execCli
    [ "genesis"
    , "--genesis-output-dir", tempAbsPath </> "genesis"
    , "--start-time", showUTCTimeSeconds startTime
    , "--protocol-parameters-file", base </> "scripts/protocol-params.json"
    , "--k", "2160"
    , "--protocol-magic", show @Int testnetMagic
    , "--n-poor-addresses", "128"
    , "--n-delegate-addresses", "7"
    , "--total-balance", "8000000000000000"
    , "--avvm-entry-count", "128"
    , "--avvm-entry-balance", "10000000000000"
    , "--delegate-share", "0.9"
    , "--real-pbft"
    , "--secret-seed", "2718281828"
    ]

  H.writeFile (tempAbsPath </> "genesis/GENHASH") . S.lastLine =<< H.execCli
    [ "print-genesis-hash"
    , "--genesis-json"
    , tempAbsPath </> "genesis/genesis.json"
    ]

  let nodeIndexes = [0..nodeCount - 1]
  let allNodes = fmap (\i -> "node-" <> show @Int i) nodeIndexes

  H.createDirectoryIfMissing logDir

  -- Launch cluster of three nodes
  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    dbDir <- H.noteShow $ tempAbsPath </> "db/node-" <> si
    nodeStdoutFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> "node-" <> si)
    portString <- H.noteShow $ "300" <> si <> ""
    topologyFile <- H.noteShow $ tempAbsPath </> "topology-node-" <> si <> ".json"
    configFile <- H.noteShow $ tempAbsPath </> "config-" <> si <> ".yaml"
    signingKeyFile <- H.noteShow $ tempAbsPath </> "genesis/delegate-keys.00" <> si <> ".key"
    delegationCertificateFile <- H.noteShow $ tempAbsPath </> "genesis/delegation-cert.00" <> si <> ".json"

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> "" <> socketDir

    H.copyFile (baseConfig </> "topology-node-" <> si <> ".json") (tempAbsPath </> "topology-node-" <> si <> ".json")
    H.writeFile (tempAbsPath </> "config-" <> si <> ".yaml") . L.unlines . fmap rewriteConfiguration . L.lines =<<
      H.readFile (baseConfig </> "config-" <> si <> ".yaml")

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--database-path", dbDir
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--port", portString
        , "--topology", topologyFile
        , "--config", configFile
        , "--signing-key", signingKeyFile
        , "--delegation-certificate", delegationCertificateFile
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

    H.onFailure . H.noteM_ $ H.readFile nodeStdoutFile
    H.onFailure . H.noteM_ $ H.readFile nodeStderrFile

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> "node-" <> si)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.assertByDeadlineM deadline $ H.doesSprocketExist sprocket

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    nodeStdoutFile <- H.noteTempFile tempAbsPath $ "cardano-node-" <> si <> ".stdout.log"
    H.assertByDeadlineIO deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile

  H.copyFile (tempAbsPath </> "config-1.yaml") (tempAbsPath </> "configuration.yaml")

  return allNodes
