{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.Chairman
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, discover)

import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified Test.Common.Base as H
import qualified Test.Common.Process as H

prop_spawnOneNode :: Property
prop_spawnOneNode = H.propertyOnce . H.workspace "temp/chairman" $ \tempDir -> do
  let dbDir = tempDir <> "/db/node-2"
  let socketDir = tempDir <> "/socket"

  H.createDirectoryIfMissing dbDir
  H.createDirectoryIfMissing socketDir

  base <- H.getProjectBase

  dirContents <- liftIO $ IO.listDirectory base

  H.annotateShow $ dirContents

  (_mIn, _mOut, _mErr, hProcess) <- H.createProcess =<< H.procNode
    [ "run"
    , "--database-path", dbDir
    , "--socket-path", socketDir <> "/node-2-socket"
    , "--port", "3002"
    , "--topology", base <> "/chairman/configuration/defaults/simpleview/topology-node-2.json"
    , "--config", base <> "/chairman/configuration/defaults/simpleview/config-2.yaml"
    , "--signing-key", base <> "/chairman/configuration/defaults/simpleview/genesis/delegate-keys.002.key"
    , "--delegation-certificate", base <> "/chairman/configuration/defaults/simpleview/genesis/delegation-cert.002.json"
    ]

  H.threadDelay 10000000

  H.interruptProcessGroupOf hProcess

  void $ H.waitForProcess hProcess

tests :: IO Bool
tests = H.checkParallel $$discover
