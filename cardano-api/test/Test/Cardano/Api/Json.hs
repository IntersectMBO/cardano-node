{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Json
  ( tests
  ) where

import           Cardano.Api (EraInMode (..))
import           Cardano.Api.Orphans ()
import           Cardano.Prelude (Either (Right), ($), (==))
import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), eitherDecode, encode)
import           Data.Aeson.Types (Parser, parseEither)
import           Gen.Cardano.Api (genAlonzoGenesis)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover, forAll, tripping)
import           Test.Tasty (TestTree)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_alonzo_genesis :: Property
prop_roundtrip_alonzo_genesis = H.property $ do
  genesis <- forAll genAlonzoGenesis
  tripping genesis encode eitherDecode

prop_roundtrip_eraInMode :: Property
prop_roundtrip_eraInMode = H.property $ do
  H.assert $ parseEither rountripEraInModeParser ByronEraInByronMode == Right ByronEraInByronMode
  H.assert $ parseEither rountripEraInModeParser ShelleyEraInShelleyMode == Right ShelleyEraInShelleyMode
  H.assert $ parseEither rountripEraInModeParser ByronEraInCardanoMode == Right ByronEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser ShelleyEraInCardanoMode == Right ShelleyEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser AllegraEraInCardanoMode == Right AllegraEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser MaryEraInCardanoMode == Right MaryEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser AlonzoEraInCardanoMode == Right AlonzoEraInCardanoMode

  where
    -- Defined this way instead of using 'tripping' in order to warn the
    -- developer if there's ever a new constructor in 'EraInMode' and we would
    -- need to add a new 'FromJSON' instance.
    rountripEraInModeParser :: EraInMode era mode -> Parser (EraInMode era mode)
    rountripEraInModeParser = \case
      ByronEraInByronMode -> parseJSON $ toJSON ByronEraInByronMode
      ShelleyEraInShelleyMode -> parseJSON $ toJSON ShelleyEraInShelleyMode
      ByronEraInCardanoMode -> parseJSON $ toJSON ByronEraInCardanoMode
      ShelleyEraInCardanoMode -> parseJSON $ toJSON ShelleyEraInCardanoMode
      AllegraEraInCardanoMode -> parseJSON $ toJSON AllegraEraInCardanoMode
      MaryEraInCardanoMode -> parseJSON $ toJSON MaryEraInCardanoMode
      AlonzoEraInCardanoMode -> parseJSON $ toJSON AlonzoEraInCardanoMode


tests :: TestTree
tests = fromGroup $$discover
