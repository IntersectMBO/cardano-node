{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Logging.LogFormattingCodec
  ( LogFormattingCodec(..)
  , forMachineViaCodec
  , getSchema
  ) where

import qualified Data.Aeson as AE
import Data.Proxy (Proxy(..))
import Cardano.Logging.Types
import Cardano.Logging.Types.TraceMessage

-- From autodocodec:
import Autodocodec
import Autodocodec.Schema(jsonObjectSchemaVia, ObjectSchema)


instance HasCodec SeverityS where
  codec = codecViaAeson "SeverityS"

-- | Every message needs this to define how to represent itself
class HasObjectCodec a => LogFormattingCodec a where

  extraCodecMinimal :: Maybe (JSONObjectCodec a)
  extraCodecMinimal = Nothing

  extraCodecDetailed :: Maybe (JSONObjectCodec a)
  extraCodecDetailed = Nothing

  extraCodecMaximal :: Maybe (JSONObjectCodec a)
  extraCodecMaximal = Nothing

-- | Emit an Aeson object for machine consumption (JSON logs, forwarder, etc.)
--   The default uses the codec-based JSON encoding, and optionally reshapes
--   it by detail level.
forMachineViaCodec :: LogFormattingCodec a => DetailLevel -> a -> AE.Object
forMachineViaCodec dl a =
    let payloadCodec = getPayloadCodecFor dl a
        res = toJSONObjectVia payloadCodec a in
    res

getSchema :: forall a . LogFormattingCodec a => DetailLevel -> Proxy a -> ObjectSchema
getSchema dl _ =
  let payloadCodec = getPayloadCodecFor dl (undefined :: a)
  in jsonObjectSchemaVia payloadCodec

getPayloadCodecFor :: LogFormattingCodec a => DetailLevel -> a -> JSONObjectCodec a
getPayloadCodecFor DMinimal _ =
    case extraCodecMinimal of
        Nothing -> objectCodec
        Just c -> c
getPayloadCodecFor DNormal _ = objectCodec
getPayloadCodecFor DDetailed _ =
    case extraCodecDetailed of
        Nothing -> objectCodec
        Just c -> c
getPayloadCodecFor DMaximum _ =
    case extraCodecMaximal of
        Nothing -> objectCodec
        Just c -> c

_commonCardanoCodec :: JSONCodec AE.Object -> JSONCodec TraceMessage
_commonCardanoCodec inner =
  object "TraceMessage" $
    TraceMessage
      <$> requiredField "at" "Timestamp." .= tmsgAt
      <*> requiredField "ns" "Namespace." .= tmsgNS
      <*> requiredFieldWith "data" inner "Payload object." .= tmsgData
      <*> requiredField "sev" "Severity." .= tmsgSev
      <*> requiredField "thread" "Thread id." .= tmsgThread
      <*> requiredField "host" "Hostname." .= tmsgHost

