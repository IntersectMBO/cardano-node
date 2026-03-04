module Cardano.Timeseries.AsText where

import           Data.Text (Text, pack)

-- | For the purpose of pretty-printing.
--   Result may include linebreaks.
class AsText a where
  asText :: a -> Text

{-# INLINE showT #-}
showT :: Show a => a -> Text
showT = pack . show
