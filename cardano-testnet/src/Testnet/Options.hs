{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Options
  ( runCardanoOptions
  ) where

import           Prelude

import           Parsers.Cardano as Cardano
import           Testnet.Property.Run
import           Testnet.Start.Cardano

runCardanoOptions :: CardanoOptions -> IO ()
runCardanoOptions options =
  runTestnet $ cardanoTestnet (Cardano.testnetOptions options)


