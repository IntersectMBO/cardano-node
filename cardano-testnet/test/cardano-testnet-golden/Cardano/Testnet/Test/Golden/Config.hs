{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Golden.Config
  ( goldenDefaultConfigYaml
  , dijkstraProtocolConfig
  ) where

import           Cardano.Api (EpochNo (..),
                   ShelleyBasedEra (ShelleyBasedEraConway, ShelleyBasedEraDijkstra))

import           Cardano.Node.Configuration.POM (PartialNodeConfiguration (..))
import           Cardano.Node.Types (NodeHardForkProtocolConfiguration (..),
                   NodeProtocolConfiguration (..))

import           Prelude

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe (isJust)
import           Data.Monoid (Last (..))
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

-- The hardfork trigger and advertised block version must match Dijkstra genesis
-- and the protected-address activation version.
dijkstraProtocolConfig :: Property
dijkstraProtocolConfig = propertyOnce $ do
  let config = defaultYamlHardforkViaConfig ShelleyBasedEraDijkstra
  KeyMap.lookup "LastKnownBlockVersion-Major" config === Just (Number 12)
  KeyMap.lookup "TestDijkstraHardForkAtEpoch" config === Just (Number 0)
  KeyMap.lookup "ExperimentalHardForksEnabled" config === Just (Bool True)
  (dijkstraGenesis, hardfork) <- parseProtocol config
  assert $ isJust dijkstraGenesis
  npcExperimentalHardForksEnabled hardfork === True
  npcTestDijkstraHardForkAtEpoch hardfork === Just (EpochNo 0)
  -- Removing the opt-in makes the real node parser discard the Dijkstra
  -- trigger and genesis, even though the epoch field remains present.
  (disabledGenesis, disabledHardfork) <- parseProtocol $ KeyMap.delete "ExperimentalHardForksEnabled" config
  disabledGenesis === Nothing
  npcExperimentalHardForksEnabled disabledHardfork === False
  npcTestDijkstraHardForkAtEpoch disabledHardfork === Nothing
  let conwayConfig = defaultYamlHardforkViaConfig ShelleyBasedEraConway
  KeyMap.lookup "ExperimentalHardForksEnabled" conwayConfig === Nothing
  (conwayGenesis, conwayHardfork) <- parseProtocol conwayConfig
  conwayGenesis === Nothing
  npcExperimentalHardForksEnabled conwayHardfork === False
  npcTestConwayHardForkAtEpoch conwayHardfork === Just (EpochNo 0)
 where
  parseProtocol config = do
    parsed <- evalEither $ case fromJSON (Object config) of
      Error err      -> Left err
      Success result -> Right (result :: PartialNodeConfiguration)
    case getLast (pncProtocolConfig parsed) of
      Just (NodeProtocolConfigurationCardano _ _ _ _ genesis hardfork _) -> pure (genesis, hardfork)
      Nothing -> failure
