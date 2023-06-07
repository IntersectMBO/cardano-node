{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Test.Golden.Testnet.Config
  ( goldenDefaultConfigYaml
  ) where

import           Prelude

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.FilePath ((</>))

import           Cardano.Api

import           Testnet.Byron

import           Hedgehog
import           Hedgehog.Extras.Test.Base (propertyOnce)
import           Hedgehog.Extras.Test.Golden (diffVsGoldenFile)
import qualified Hedgehog.Extras.Test.Process as H

goldenDefaultConfigYaml :: Property
goldenDefaultConfigYaml = propertyOnce $ do
  base <- H.getProjectBase
  let allEras = map (createConfigStringAndPath base) [minBound..maxBound]
  mapM_ (uncurry diffVsGoldenFile) allEras

createConfigStringAndPath :: FilePath -> AnyCardanoEra -> (String, FilePath)
createConfigStringAndPath base era =
  let configStr = createConfigYamlString era
      configPath = base </> createGoldenFilePath era
  in (configStr, configPath)

createGoldenFilePath :: AnyCardanoEra -> FilePath
createGoldenFilePath = \case
  AnyCardanoEra ByronEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/byron_node_default_config.json"
  AnyCardanoEra ShelleyEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/shelley_node_default_config.json"
  AnyCardanoEra AllegraEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/allegra_node_default_config.json"
  AnyCardanoEra MaryEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/mary_node_default_config.json"
  AnyCardanoEra AlonzoEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/alonzo_node_default_config.json"
  AnyCardanoEra BabbageEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/babbage_node_default_config.json"
  AnyCardanoEra ConwayEra ->
    "cardano-testnet/test/cardano-testnet-test/files/golden/conway_node_default_config.json"

createConfigYamlString :: AnyCardanoEra -> String
createConfigYamlString era =
  let configBs = LB.toStrict . encodePretty
                  . Object . defaultYamlHardforkViaConfig $ era
      configStr = Text.unpack $ Text.decodeUtf8 configBs
  in configStr
