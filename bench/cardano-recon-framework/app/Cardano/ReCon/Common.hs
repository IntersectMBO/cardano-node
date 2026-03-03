module Cardano.ReCon.Common where
import           Cardano.ReCon.LTL.Lang.Formula

import           Data.Aeson
import           Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map
import           Data.Map.Strict (Map)

-- | Extract all accessible properties (fields) from the json object, non-recursively.
--   Accessible fields are all fields of one of the following types: number, string.
extractProps :: Object -> Map PropVarIdentifier PropValue
extractProps = Map.delete "kind" . Map.mapMaybe f . Map.mapKeysMonotonic toText . KeyMap.toMap
  where
    f (Number v)  = Just $ IntValue (truncate v)
    f (String v)  = Just $ TextValue v
    f _           = Nothing
