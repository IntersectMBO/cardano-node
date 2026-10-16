{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.SubmitApi
  ( SubmitApiConf (..)
  , withSubmitApi
  ) where

import           Cardano.Testnet

import           Prelude

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified System.IO as IO
import qualified System.Process as IO

import           Testnet.Process.Run (procSubmitApi)

import qualified Hedgehog as H
import           Hedgehog.Extras (Integration)
import           Hedgehog.Extras.Stock (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

data SubmitApiConf = SubmitApiConf
  { tempAbsPath   :: FilePath
  , epochSlots    :: Int
  , sprocket      :: Sprocket
  , testnetMagic  :: Int
  , maybePort     :: Maybe Int
  } deriving (Eq, Show)

withSubmitApi :: SubmitApiConf -> [String] -> (String -> Integration ()) -> Integration ()
withSubmitApi
    SubmitApiConf
      { tempAbsPath
      , epochSlots
      , sprocket
      , testnetMagic
      , maybePort
      } args f = do
  let logDir = makeLogDir $ TmpAbsolutePath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath

  nodeStdoutFile <- H.noteTempFile logDir "submit-api.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "submit-api.stderr.log"

  hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

  [submitApiPort] <- H.evalIO $ maybe (IO.allocateRandomPorts 1) (pure . (:[])) maybePort

  -- The submit API's configuration is a tracing configuration, not a node
  -- configuration. An empty @TraceOptions@ leaves the submit API's defaults.
  configFile <- H.noteTempFile tempAbsPath "submit-api-config.json"
  H.lbsWriteFile configFile . Aeson.encode $ Aeson.object ["TraceOptions" .= Aeson.object []]

  cp <- procSubmitApi $
    [ "--config", configFile
    , "--cardano-mode"
    , "--epoch-slots", show epochSlots
    , "--socket-path", IO.sprocketArgumentName sprocket
    , "--testnet-magic", show testnetMagic
    , "--port", show submitApiPort
    ] <> args

  (_, _, _, hProcess, _) <- H.createProcess $ cp
    { IO.std_in = IO.CreatePipe
    , IO.std_out = IO.UseHandle hNodeStdout
    , IO.std_err = IO.UseHandle hNodeStderr
    , IO.cwd = Just tempBaseAbsPath
    }

  H.onFailure $ H.evalIO $ IO.terminateProcess hProcess
  H.noteShow_ =<< H.getPid hProcess

  let uriBase = "http://localhost:" <> show submitApiPort
  f uriBase

  H.evalIO $ IO.terminateProcess hProcess
