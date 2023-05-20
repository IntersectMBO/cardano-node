{-# LANGUAGE GADTs #-}

module Test.Golden.Testnet.Config where

import           Prelude

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api

import           Testnet.Options

import           Hedgehog
import           Hedgehog.Extras.Test.Base (propertyOnce)
import           Hedgehog.Extras.Test.Golden (diffVsGoldenFile)


goldenDefaultConfigYaml :: Property
goldenDefaultConfigYaml = propertyOnce $ do
  let allEras = map createConfigStringAndPath [minBound..maxBound]
  mapM_ (uncurry diffVsGoldenFile) allEras
 where
  createConfigStringAndPath :: AnyCardanoEra -> (String, FilePath)
  createConfigStringAndPath era =
    let configStr = createConfigYamlString era
        configPath = createGoldenFilePath era
    in (configStr, configPath)

  createGoldenFilePath :: AnyCardanoEra -> FilePath
  createGoldenFilePath (AnyCardanoEra ByronEra) =
    "test/cardano-testnet-test/files/golden/byron_node_default_config.json"
  createGoldenFilePath (AnyCardanoEra ShelleyEra) =
    "test/cardano-testnet-test/files/golden/shelley_node_default_config.json"
  createGoldenFilePath (AnyCardanoEra AllegraEra) =
    "test/cardano-testnet-test/files/golden/allegra_node_default_config.json"
  createGoldenFilePath (AnyCardanoEra MaryEra) =
    "test/cardano-testnet-test/files/golden/mary_node_default_config.json"
  createGoldenFilePath (AnyCardanoEra AlonzoEra) =
    "test/cardano-testnet-test/files/golden/alonzo_node_default_config.json"
  createGoldenFilePath (AnyCardanoEra BabbageEra) =
    "test/cardano-testnet-test/files/golden/babbage_node_default_config.json"
  createGoldenFilePath (AnyCardanoEra ConwayEra) =
    "test/cardano-testnet-test/files/golden/conway_node_default_config.json"

  createConfigYamlString :: AnyCardanoEra -> String
  createConfigYamlString era =
    let configBs = LB.toStrict . encodePretty
                   . Object . defaultYamlHardforkViaConfig $ era
        configStr = Text.unpack $ Text.decodeUtf8 configBs
    in configStr
