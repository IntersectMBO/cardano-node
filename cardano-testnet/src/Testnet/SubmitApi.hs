{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use let" -}
{- HLINT ignore "Redundant <&>" -}

module Testnet.SubmitApi
  ( SubmitApiConf (..)
  , withSubmitApi
  ) where

import           Cardano.Testnet
import qualified Cardano.Testnet as H

import           Prelude

import           Data.Functor ((<&>))
import qualified System.IO as IO
import qualified System.Process as IO

import qualified Testnet.Process.Run as H

import qualified Hedgehog as H
import           Hedgehog.Extras (Integration)
import           Hedgehog.Extras.Stock (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H

data SubmitApiConf = SubmitApiConf
  { tempAbsPath   :: FilePath
  , configPath    :: FilePath
  , epochSlots    :: Int
  , sprocket      :: Sprocket
  , testnetMagic  :: Int
  , maybePort     :: Maybe Int
  } deriving (Eq, Show)

withSubmitApi :: SubmitApiConf -> [String] -> (String -> Integration ()) -> Integration ()
withSubmitApi
    SubmitApiConf
      { tempAbsPath
      , configPath
      , epochSlots
      , sprocket
      , testnetMagic
      , maybePort
      } args f = do
  let logDir = makeLogDir $ TmpAbsolutePath tempAbsPath
      tempBaseAbsPath = H.makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath

  nodeStdoutFile <- H.noteTempFile logDir "submit-api.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "submit-api.stderr.log"

  hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

  [submitApiPort] <- H.evalIO $ maybe (IO.allocateRandomPorts 1) (pure . (:[])) maybePort

  (_, _, _, hProcess, _) <- H.createProcess =<<
    ( H.procSubmitApi
      ( [ "--config", configPath
        , "--cardano-mode"
        , "--epoch-slots", show @Int epochSlots
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--testnet-magic", show @Int testnetMagic
        , "--port", show @Int submitApiPort
        ]
        <> args
      ) <&>
      ( \cp -> cp
        { IO.std_in = IO.CreatePipe
        , IO.std_out = IO.UseHandle hNodeStdout
        , IO.std_err = IO.UseHandle hNodeStderr
        , IO.cwd = Just tempBaseAbsPath
        }
      )
    )

  H.onFailure $ H.evalIO $ IO.terminateProcess hProcess

  H.noteShow_ =<< H.getPid hProcess

  let uriBase = "http://localhost:" <> show submitApiPort

  f uriBase

  H.evalIO $ IO.terminateProcess hProcess
