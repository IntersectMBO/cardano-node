{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Golden.Config
  ( goldenDefaultConfigYaml
  ) where

import           Cardano.Api

import           Prelude

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.FilePath ((</>))

import           Testnet.Defaults

import           Hedgehog
import           Hedgehog.Extras.Test.Base (propertyOnce)
import           Hedgehog.Extras.Test.Golden (diffVsGoldenFile)
import qualified Hedgehog.Extras.Test.Process as H
import Testnet.Components.Configuration (eraToString)

goldenDefaultConfigYaml :: Property
goldenDefaultConfigYaml = propertyOnce $ do
  base <- H.getProjectBase
  let asbes = [minBound..maxBound]
  let allEras = map (f base) asbes
  mapM_ (uncurry diffVsGoldenFile) allEras
  where
    f base (asbe :: AnyShelleyBasedEra) = do
      AnyShelleyBasedEra sbe <- pure asbe
      createConfigStringAndPath base sbe

createConfigStringAndPath :: FilePath -> ShelleyBasedEra era -> (String, FilePath)
createConfigStringAndPath base sbe =
  let configStr = createConfigYamlString sbe
      configPath = base </> createGoldenFilePath sbe
  in (configStr, configPath)

createGoldenFilePath :: ShelleyBasedEra era -> FilePath
createGoldenFilePath sbe =
  "cardano-testnet/test/cardano-testnet-golden/files/golden/"
    <> eraToString sbe
    <> "_node_default_config.json"

createConfigYamlString :: ShelleyBasedEra era -> String
createConfigYamlString sbe =
  let configBs = LB.toStrict . encodePretty
                  . Object . defaultYamlHardforkViaConfig $ sbe
      configStr = Text.unpack $ Text.decodeUtf8 configBs
  in configStr
