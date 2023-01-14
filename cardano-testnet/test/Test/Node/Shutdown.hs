{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Node.Shutdown
  ( hprop_shutdown
  ) where

import           Prelude
import           Control.Monad
import           Data.Functor ((<&>))
import qualified Data.List as L
import           Data.Maybe
import           Hedgehog (Property, (===))
import           System.FilePath ((</>))

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Exit as IO
import qualified System.IO as IO
import qualified System.Process as IO

import           Cardano.Testnet
import           Testnet.Util.Process (procNode)

{- HLINT ignore "Redundant <&>" -}

hprop_shutdown :: Property
hprop_shutdown = integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  Conf { tempBaseAbsPath, tempAbsPath, logDir, socketDir } <- H.noteShowM $
    mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  [port] <- H.noteShowIO $ IO.allocateRandomPorts 1

  H.createDirectoryIfMissing logDir

  sprocket <- H.noteShow $ IO.Sprocket tempBaseAbsPath (socketDir </> "node")

  H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

  H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

  nodeStdoutFile <- H.noteTempFile logDir "node.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "node.stderr.log"

  hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

  -- Run cardano-node with pipe as stdin.  Use 0 file descriptor as shutdown-ipc
  (mStdin, _mStdout, _mStderr, pHandle, _releaseKey) <- H.createProcess =<<
    ( procNode
      [ "run"
      , "--config", base </> "configuration/cardano/mainnet-config.json"
      , "--topology", base </> "configuration/cardano/mainnet-topology.json"
      , "--database-path", tempAbsPath </> "db"
      , "--socket-path", IO.sprocketArgumentName sprocket
      , "--host-addr", "127.0.0.1"
      , "--port", show @Int port
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

  H.threadDelay $ 10 * 1000000

  mExitCodeRunning <- H.evalIO $ IO.getProcessExitCode pHandle

  when (isJust mExitCodeRunning) $ do
    H.evalIO $ IO.hClose hNodeStdout
    H.evalIO $ IO.hClose hNodeStderr
    H.cat nodeStdoutFile
    H.cat nodeStderrFile

  mExitCodeRunning === Nothing

  forM_ mStdin $ \hStdin -> H.evalIO $ IO.hClose hStdin

  H.threadDelay $ 2 * 1000000

  mExitCode <- H.evalIO $ IO.getProcessExitCode pHandle

  mExitCode === Just IO.ExitSuccess

  return ()
