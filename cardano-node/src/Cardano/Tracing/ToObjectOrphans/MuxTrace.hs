{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.Tracing.ToObjectOrphans.MuxTrace () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson hiding (Error)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject(..))
import           Cardano.BM.Data.Tracer ( definePrivacyAnnotation
                                        , mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..), Tracer(..)
                                    , TracingFormatting(..), Transformable(..)
                                    , traceWith, mkLOMeta)
import           Network.Mux (WithMuxBearer (..), MuxTrace (..))


instance DefinePrivacyAnnotation (WithMuxBearer peer MuxTrace)
instance DefineSeverity (WithMuxBearer peer MuxTrace) where
  defineSeverity (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart          -> Debug
    MuxTraceRecvHeaderEnd {}         -> Debug
    MuxTraceRecvPayloadStart {}      -> Debug
    MuxTraceRecvPayloadEnd {}        -> Debug
    MuxTraceRecvStart {}             -> Debug
    MuxTraceRecvEnd {}               -> Debug
    MuxTraceSendStart {}             -> Debug
    MuxTraceSendEnd                  -> Debug
    MuxTraceState {}                 -> Info
    MuxTraceCleanExit {}             -> Info
    MuxTraceExceptionExit {}         -> Info
    MuxTraceChannelRecvStart {}      -> Debug
    MuxTraceChannelRecvEnd {}        -> Debug
    MuxTraceChannelSendStart {}      -> Debug
    MuxTraceChannelSendEnd {}        -> Debug
    MuxTraceHandshakeStart           -> Debug
    MuxTraceHandshakeClientEnd {}    -> Info
    MuxTraceHandshakeServerEnd       -> Debug
    MuxTraceHandshakeClientError {}  -> Error
    MuxTraceHandshakeServerError {}  -> Error
    MuxTraceRecvDeltaQObservation {} -> Debug
    MuxTraceRecvDeltaQSample {}      -> Info

-- transform @MuxTrace@
instance (Show peer)
           => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage . toS $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

instance (Show peer)
      => ToObject (WithMuxBearer peer MuxTrace) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
