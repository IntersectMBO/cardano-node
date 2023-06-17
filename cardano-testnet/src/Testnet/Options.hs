{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Options
  ( BabbageTestnetOptions(..)
  , ConwayTestnetOptions(..)
  , TestnetOptions(..)
  , testnet
  , runCardanoOptions
  , runShelleyOptions
  ) where

import           Prelude

import           Control.Monad

import           Parsers.Cardano as Cardano
import           Parsers.Shelley as Shelley
import           Testnet.Conf
import           Testnet.Property.Run
import           Testnet.Start.Babbage
import           Testnet.Start.Cardano
import           Testnet.Start.Conway
import           Testnet.Start.Shelley

import qualified Hedgehog as H
import           Hedgehog.Extras.Test.Base (Integration, noteShow_)


{- HLINT ignore "Redundant flip" -}

data TestnetOptions
  = ShelleyOnlyTestnetOptions ShelleyTestnetOptions
  | BabbageOnlyTestnetOptions BabbageTestnetOptions
  | ConwayOnlyTestnetOptions ConwayTestnetOptions
  | CardanoOnlyTestnetOptions CardanoTestnetOptions
  deriving (Eq, Show)

testnet :: TestnetOptions -> Conf -> Integration TestnetRuntime
testnet options conf = case options of
  ShelleyOnlyTestnetOptions o -> shelleyTestnet o conf
  BabbageOnlyTestnetOptions o -> babbageTestnet o conf
  ConwayOnlyTestnetOptions o -> conwayTestnet o conf
  CardanoOnlyTestnetOptions o -> do
    testnetMinimumConfigurationRequirements o
    cardanoTestnet o conf

runCardanoOptions :: CardanoOptions -> IO ()
runCardanoOptions options =
  runTestnet $ testnet (CardanoOnlyTestnetOptions $ Cardano.testnetOptions options)

runShelleyOptions :: ShelleyOptions -> IO ()
runShelleyOptions options =
  runTestnet $ testnet (ShelleyOnlyTestnetOptions $ Shelley.testnetOptions options)


-- | There are certain conditions that need to be met in order to run
-- a valid node cluster.
testnetMinimumConfigurationRequirements :: CardanoTestnetOptions -> Integration ()
testnetMinimumConfigurationRequirements cTestnetOpts = do
  when (length (cardanoNodes cTestnetOpts) < 2) $ do
     noteShow_ ("Need at least two nodes to run a cluster" :: String)
     noteShow_ cTestnetOpts
     H.assert False

  when (cardanoNumPoolNodes (cardanoNodes cTestnetOpts) < 1) $ do
     noteShow_ ("Need at least one SPO to run a cluster" :: String)
     noteShow_ cTestnetOpts
     H.assert False





