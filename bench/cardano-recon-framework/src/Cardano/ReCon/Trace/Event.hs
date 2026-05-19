{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

class Extractable a where
  extract :: Value -> Maybe a

instance Extractable IntValue where
  extract (Number n) = Just (truncate n)
  extract _          = Nothing

instance Extractable Text where
  extract (String s) = Just s
  extract _          = Nothing

extractProps :: Extractable a => Object -> Map VariableIdentifier a
extractProps = Map.delete "kind" . go ""
  where
    go prefix = Map.foldlWithKey' (\acc k v ->
        let key = prefix <> k
        in case v of
          Object nested -> Map.union acc (go (key <> ".") nested)
          _             -> maybe acc (\val -> Map.insert key val acc) (extract v)
      ) Map.empty . Map.mapKeysMonotonic toText . KeyMap.toMap

instance Event TemporalEvent Text where
  ofTy (TemporalEvent _ msgs) c = isJust $ find (\msg -> msg.tmsgNS == c) msgs
  intProps (TemporalEvent _ msgs) c =
    case find (\msg -> msg.tmsgNS == c) msgs of
      Just x  -> extractProps x.tmsgData
      Nothing -> error ("Not an event of type " <> unpack c)
  textProps (TemporalEvent _ msgs) c =
    case find (\msg -> msg.tmsgNS == c) msgs of
      Just x  -> Map.insert "host"   x.tmsgHost   $
                   Map.insert "thread" x.tmsgThread $
                     extractProps x.tmsgData
      Nothing -> error ("Not an event of type " <> unpack c)
  beg (TemporalEvent t _) = t
