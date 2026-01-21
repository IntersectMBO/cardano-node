{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Logging.LogFormattingCodec
  ( LogFormattingCodec(..)
  , forMachineViaCodec
  , getSchema
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy (Proxy(..))
import Cardano.Logging.Types

-- From autodocodec:
import Autodocodec (HasCodec(..), JSONCodec, object, requiredField, toJSONVia, (.=))

-- | Every message needs this to define how to represent itself
class (LogFormatting a, HasCodec a) => LogFormattingCodec a where

  extraCodecMinimal :: Maybe Codec
  extraCodecMinimal = Nothing

  extraCodecDetailed :: Maybe Codec
  extraCodecDetailed = Nothing

  extraCodecMaximal :: Maybe Codec
  extraCodecMaximal = Nothing

  forMachine :: DetailLevel -> a -> AE.Object
  forMachine = forMachineViaCodec


-- | Emit an Aeson object for machine consumption (JSON logs, forwarder, etc.)
--   The default uses the codec-based JSON encoding, and optionally reshapes
--   it by detail level.
forMachineViaCodec :: (LogFormattingCodec a) => DetailLevel -> a -> AE.Object
forMachineViaCodec dl a =
    let codec = getCodec dl
        res = toJSONVia codec a in
    case res of
        AE.Object o -> o
        other       -> KM.singleton (Key.fromString "data") other

-- | Produce a JSON Schema for the type.
--   You can choose a Schema representation your codebase already uses.
getSchema :: (LogFormattingCodec a) => DetailLevel -> Proxy a -> Schema
getSchema dl p =
    let codec = getCodec dl
        res = toJSONVia codec a in

getCodec ::  => (LogFormattingCodec a) => DetailLevel -> a -> Codec
getCodec a dl = commonCardanoCodec <> getPayloadCodecFor a dl

getPayloadCodecFor :: (LogFormattingCodec a) => DetailLevel -> a -> Codec
getPayloadCodecFor _ DMinimal =
case extraCodecMinimal of
    Nothing -> codec
    Just c -> c
getPayloadCodecFor _ DNormal = codec
getPayloadCodecFor _ DDetailed =
case extraCodecDetailed of
    Nothing -> codec
    Just c -> c
getPayloadCodecFor _ DMaximum =
case extraCodecMaximal of
    Nothing -> codec
    Just c -> c

commonCardanoCodec :: JSONCodec TraceMessage
commonCardanoCodec =
  object "TraceMessage" $
    TraceMessage
      <$> requiredField "at" "Timestamp." .= tmsgAt
      <*> requiredField "ns" "Namespace." .= tmsgNS
      <*> requiredField "data" "Payload object." .= tmsgData
      <*> requiredField "sev" "Severity." .= tmsgSev
      <*> requiredField "thread" "Thread id." .= tmsgThread
      <*> requiredField "host" "Hostname." .= tmsgHost
