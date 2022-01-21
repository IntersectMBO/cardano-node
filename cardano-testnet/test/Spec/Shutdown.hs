{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Shutdown
  ( hprop_shutdown
  ) where

import           Control.Monad
import           Data.Ord
import           Data.Function
import           Data.Functor ((<&>))
import           Data.Int
import           Data.Maybe
import           GHC.Num
import           Hedgehog (Property, (===))
import           System.FilePath ((</>))
import           Text.Show (Show (..))

import qualified Data.List as L
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
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

hprop_shutdown :: Property
hprop_shutdown = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  H.Conf { H.tempBaseAbsPath, H.tempAbsPath, H.logDir, H.socketDir } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

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
    ( H.procNode
      [ "run"
      , "--config", projectBase </> "configuration/cardano/mainnet-config.json"
      , "--topology", projectBase </> "configuration/cardano/mainnet-topology.json"
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

  H.threadDelay $ 1 * 10000000

  mExitCode <- H.evalIO $ IO.getProcessExitCode pHandle

  mExitCode === Just IO.ExitSuccess

  return ()
