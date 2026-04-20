module Cardano.ReCon.Common where

import           Data.Aeson (Object, Value (..))
import           Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Text (Text)

-- | Extract scalar properties from a JSON object as Aeson Values, for display.
extractJsonProps :: Object -> Map Text Value
extractJsonProps = Map.delete "kind" . Map.mapMaybe f . Map.mapKeysMonotonic toText . KeyMap.toMap
  where
    f v@(Number _) = Just v
    f v@(String _) = Just v
    f _            = Nothing
