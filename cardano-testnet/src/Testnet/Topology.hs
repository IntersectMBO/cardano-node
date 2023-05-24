{-# LANGUAGE OverloadedStrings #-}

module Testnet.Topology
  ( defaultMainnetTopology
  ) where

import           Cardano.Node.Configuration.Topology


defaultMainnetTopology :: NetworkTopology
defaultMainnetTopology =
  let single = RemoteAddress
         { raAddress  = "relays-new.cardano-mainnet.iohk.io"
         , raPort     = 3001
         , raValency= 2
         }
  in RealNodeTopology [single]
