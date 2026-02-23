module TraceVerifier.Common where
import           Cardano.LTL.Lang.Formula
import           Data.Aeson
import           Data.Aeson.Key           (toText)
import qualified Data.Aeson.KeyMap        as KeyMap
import qualified Data.Map                 as Map
import           Data.Map.Strict          (Map)
import           Data.Maybe               (mapMaybe)

-- | Extract all accessible properties (fields) from the json object, non-recursively.
--   Accessible fields are all fields of one of the following types: number, string.
extractProps :: Object -> Map PropVarIdentifier PropValue
extractProps = Map.delete "kind" . Map.fromList . mapMaybe parse . KeyMap.toList
  where
    parse :: (Key, Value) -> Maybe (PropVarIdentifier, PropValue)
    parse (k, Number v) = Just (toText k, IntValue (truncate v))
    parse (k, String v) = Just (toText k, TextValue v)
    parse _             = Nothing
