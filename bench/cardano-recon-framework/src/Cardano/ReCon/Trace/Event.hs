{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.ReCon.Trace.Event where

import           Cardano.Logging.Types.TraceMessage (TraceMessage (..))
import           Cardano.ReCon.LTL.Formula
import           Cardano.ReCon.Trace.Feed (TemporalEvent (..))

import           Data.Aeson (Object, Value (..))
import           Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.List (find)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (isJust)
import           Data.Text (Text, unpack)

extractIntProps :: Object -> Map VariableIdentifier IntValue
extractIntProps = Map.delete "kind" . Map.mapMaybe f . Map.mapKeysMonotonic toText . KeyMap.toMap
  where
    f (Number v) = Just (truncate v)
    f _          = Nothing

extractTextProps :: Object -> Map VariableIdentifier Text
extractTextProps = Map.delete "kind" . Map.mapMaybe f . Map.mapKeysMonotonic toText . KeyMap.toMap
  where
    f (String v) = Just v
    f _          = Nothing

instance Event TemporalEvent Text where
  ofTy (TemporalEvent _ msgs) c = isJust $ find (\msg -> msg.tmsgNS == c) msgs
  intProps (TemporalEvent _ msgs) c =
    case find (\msg -> msg.tmsgNS == c) msgs of
      Just x  -> extractIntProps x.tmsgData
      Nothing -> error ("Not an event of type " <> unpack c)
  textProps (TemporalEvent _ msgs) c =
    case find (\msg -> msg.tmsgNS == c) msgs of
      Just x  -> Map.insert "host"   x.tmsgHost   $
                   Map.insert "thread" x.tmsgThread $
                     extractTextProps x.tmsgData
      Nothing -> error ("Not an event of type " <> unpack c)
  beg (TemporalEvent t _) = t
