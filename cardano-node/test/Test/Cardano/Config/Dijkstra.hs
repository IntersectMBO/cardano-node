{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Config.Dijkstra (tests) where

import           Cardano.Api (dijkstraGenesisDefaults)

import           Cardano.Node.Protocol.Dijkstra (emptyDijkstraGenesis, readGenesisMaybe)

import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Extras.Test.Base (propertyOnce)

-- The fallback must contain every integration field and the V4 cost model,
-- rather than an obsolete partial UpgradeDijkstraPParams record.
hprop_completeFallbackGenesis :: Property
hprop_completeFallbackGenesis = propertyOnce $ do
  emptyDijkstraGenesis === dijkstraGenesisDefaults
  Aeson.eitherDecode (Aeson.encode emptyDijkstraGenesis) === Right emptyDijkstraGenesis
  result <- H.evalIO $ runExceptT $ readGenesisMaybe Nothing Nothing
  case result of
    Left err -> H.annotateShow err >> H.failure
    Right (genesis, _) -> genesis === dijkstraGenesisDefaults

tests :: IO Bool
tests = H.checkSequential $ H.Group "Test.Config.Dijkstra"
  [ ("complete fallback genesis", hprop_completeFallbackGenesis)
  ]
