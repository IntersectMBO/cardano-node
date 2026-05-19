module Cardano.Timeseries.AsText(AsText(..), showT) where

import           Cardano.Logging (showT)

import           Data.Text (Text)

-- | For the purpose of pretty-printing.
--   Result may include linebreaks.
class AsText a where
  asText :: a -> Text
