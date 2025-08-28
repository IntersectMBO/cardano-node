{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Golden.Config
  ( goldenDefaultConfigYaml
  ) where

import           Prelude

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.FilePath ((</>))

import           Testnet.Defaults (defaultEra, defaultYamlHardforkViaConfig)

import           Hedgehog
import           Hedgehog.Extras.Test.Base (propertyOnce)
import           Hedgehog.Extras.Test.Golden (diffVsGoldenFile)
import qualified Hedgehog.Extras.Test.Process as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_DefaultConfig/"'@
goldenDefaultConfigYaml :: Property
goldenDefaultConfigYaml = propertyOnce $ do
  base <- H.getProjectBase
  diffVsGoldenFile createConfigYamlString $ createConfigPath base

createConfigPath :: FilePath -> FilePath
createConfigPath base =
  base </> "cardano-testnet/test/cardano-testnet-golden/files/golden/node_default_config.json"

createConfigYamlString :: String
createConfigYamlString =
  let configBs = LB.toStrict $ encodePretty $ Object $ defaultYamlHardforkViaConfig defaultEra
  in Text.unpack $ Text.decodeUtf8 configBs