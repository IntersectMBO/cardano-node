{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}
{-# OPTIONS_GHC -Werror=unused-record-wildcards #-}

module Testnet.Blockfrost
  ( BlockfrostParams
  , blockfrostToGenesis
  ) where

import           Prelude

import           Cardano.Api (ShelleyGenesis)
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)

import qualified Control.Lens as Lens
import           Control.Lens ((&), set)
import           Data.Aeson (FromJSON(..), (.:), withObject)


import           Testnet.Start.Types (UserProvidedGeneses(..))
import           Testnet.Utils (customLensRules)

-- TODO: complete with the rest of the fields
data BlockfrostParams = BlockfrostParams
  { bfgMaxValueSize :: Int
  , bfgRho :: Double
  } deriving (Eq, Show)

Lens.makeLensesWith customLensRules ''UserProvidedGeneses
Lens.makeLensesWith customLensRules ''ShelleyGenesis
Lens.makeLensesWith customLensRules ''AlonzoGenesis
Lens.makeLensesWith customLensRules ''ConwayGenesis

instance FromJSON BlockfrostParams where
  -- In this JSON instance, I don't use the applicative syntax to be independent from field order.
  -- The flag -Werror=missing-fields ensures that no field has been missed.
  parseJSON = withObject "BlockfrostParams" $ \o -> do
    _maxValueSize <- o .: "max_val_size"
    _rho <- o .: "rho"
    pure $ BlockfrostParams {..}


-- Edit a set of Genesis files with data from Blockfrost parameters
blockfrostToGenesis :: UserProvidedGeneses -> BlockfrostParams -> UserProvidedGeneses
blockfrostToGenesis geneses BlockfrostParams{..} =
  geneses &
    set (upgAlonzoGenesis_ . agMaxValSize_) bfgMaxValueSize
