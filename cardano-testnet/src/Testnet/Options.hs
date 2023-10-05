{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Options
  ( CardanoTestnetOptions(..)
  , testnet
  , runCardanoOptions
  ) where

import           Prelude

import           Control.Monad

import qualified Hedgehog as H
import           Hedgehog.Extras.Test.Base (Integration, noteShow_)

import           Parsers.Cardano as Cardano
import           Testnet.Conf
import           Testnet.Property.Run
import           Testnet.Start.Cardano


{- HLINT ignore "Redundant flip" -}


testnet :: CardanoTestnetOptions -> Conf -> Integration TestnetRuntime
testnet o conf = do
  testnetMinimumConfigurationRequirements o
  cardanoTestnet o conf

runCardanoOptions :: CardanoOptions -> IO ()
runCardanoOptions options =
  runTestnet $ testnet (Cardano.testnetOptions options)


-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testnetMinimumConfigurationRequirements :: CardanoTestnetOptions -> Integration ()
testnetMinimumConfigurationRequirements cTestnetOpts = do
  when (length (cardanoNodes cTestnetOpts) < 2) $ do
     noteShow_ ("Need at least two nodes to run a cluster" :: String)
     noteShow_ cTestnetOpts
     H.assert False

  when (null $ cardanoNodes cTestnetOpts) $ do
     noteShow_ ("Need at least one SPO to run a cluster" :: String)
     noteShow_ cTestnetOpts
     H.assert False





