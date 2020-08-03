{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.Chairman
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, discover)

import qualified Hedgehog as H
import qualified Test.Common.Base as H
import qualified Test.Common.Process as H

prop_spawnOneNode :: Property
prop_spawnOneNode = H.propertyOnce $ do
  (_mIn, _mOut, _mErr, hProcess) <- H.createProcess =<< H.procNode
    [ "run"
    , "--database-path", "../db/node-2/"
    , "--socket-path", "../socket/node-2-socket"
    , "--port", "3002"
    , "--topology", "../configuration/defaults/simpleview/topology-node-2.json"
    , "--config", "../configuration/defaults/simpleview/config-2.yaml"
    , "--signing-key", "../configuration/defaults/simpleview/genesis/delegate-keys.002.key"
    , "--delegation-certificate", "../configuration/defaults/simpleview/genesis/delegation-cert.002.json"
    ]

  H.threadDelay 10000000

  H.interruptProcessGroupOf hProcess

  void $ H.waitForProcess hProcess

tests :: IO Bool
tests = H.checkParallel $$discover
