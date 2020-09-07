{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches #-}

module Test.Cardano.Node.Chairman.ByronShelley
  ( tests
  ) where

import           Chairman.IO.Network.Sprocket (Sprocket (..))
import           Control.Monad
import           Data.Bool
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Hedgehog (Property, discover)
import           System.IO (IO)
import           Text.Read
import           Text.Show

import qualified Chairman.Hedgehog.Base as H
import qualified Chairman.Hedgehog.File as H
import qualified Chairman.Hedgehog.Process as H
import qualified Chairman.IO.File as IO
import qualified Chairman.IO.Network.Socket as IO
import qualified Chairman.IO.Network.Sprocket as IO
import qualified Chairman.OS as OS
import qualified Chairman.String as S
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.FilePath.Posix as FP
import qualified System.IO as IO
import qualified System.Process as IO

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

prop_chairman :: Property
prop_chairman = H.propertyOnce . H.workspace "chairman" $ \tempAbsPath -> unless OS.isWin32 $ do
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  base <- H.noteShowM H.getProjectBase
  logDir <- H.noteShow $ tempAbsPath <> "/logs"
  env <- H.evalIO IO.getEnvironment
  socketDir <- H.noteShow $ tempRelPath <> "/socket"

  let testnetMagic = "42" :: String

  H.createDirectoryIfMissing logDir

  do
    H.copyFile
      (base <> "/configuration/chairman/byron-shelley/configuration.yaml")
      (tempAbsPath <> "/configuration.yaml")

    nodeStdoutFile <- H.noteTempFile logDir "mkfiles.stdout.log"
    nodeStderrFile <- H.noteTempFile logDir "mkfiles.stderr.log"

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess $ (IO.proc "bash" [base <> "/cardano-node-chairman/mkfiles.sh"])
      { IO.std_in = IO.CreatePipe
      , IO.std_out = IO.UseHandle hNodeStdout
      , IO.std_err = IO.UseHandle hNodeStderr
      , IO.env = Just (("ROOT", tempAbsPath):env)
      }

    exitCode <- H.waitForProcess hProcess

    void $ H.noteShow exitCode

    H.assertIsJsonFile $ tempAbsPath <> "/byron/genesis.spec.json"

  --------------------------------
  -- Launch cluster of three nodes

  let bftNodes = ["node-bft1", "node-bft2"]
  let poolNodes = ["node-pool1"]
  let allNodes = bftNodes <> poolNodes

  forM_ bftNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath <> "/db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath <> "/" <> node <> "/port"

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config",  tempAbsPath <> "/configuration.yaml"
        , "--topology",  tempAbsPath <> "/" <> node <> "/topology.json"
        , "--database-path", tempAbsPath <> "/" <> node <> "/db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath <> "/" <> node <> "/shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath <> "/" <> node <> "/shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath <> "/" <> node <> "/shelley/node.cert"
        , "--port",  portString
        , "--delegation-certificate",  tempAbsPath <> "/" <> node <> "/byron/delegate.cert"
        , "--signing-key", tempAbsPath <> "/" <> node <> "/byron/delegate.key"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

  forM_ poolNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath <> "/db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath <> "/" <> node <> "/port"

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath <> "/configuration.yaml"
        , "--topology", tempAbsPath <> "/" <> node <> "/topology.json"
        , "--database-path", tempAbsPath <> "/" <> node <> "/db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath <> "/" <> node <> "/shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath <> "/" <> node <> "/shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath <> "/" <> node <> "/shelley/node.cert"
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
  forM_ (L.take 1 bftNodes) $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir <> "/" <> node)

    H.createDirectoryIfMissing $ tempBaseAbsPath <> "/" <> socketDir

    hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

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
