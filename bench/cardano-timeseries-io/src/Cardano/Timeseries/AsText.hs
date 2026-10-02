module Cardano.Timeseries.AsText(AsText(..), showT) where

import           Data.Text (Text)

import           Hermod.Tracing (showT)

-- | For the purpose of pretty-printing.
--   Result may include linebreaks.
class AsText a where
  asText :: a -> Text
