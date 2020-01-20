{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tracing.ToObjectOrphans.ErrorPolicyTrace () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson hiding (Error)
import qualified Network.Socket as Socket (SockAddr)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject(..))
import           Cardano.BM.Data.Tracer ( definePrivacyAnnotation
                                        , mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..), Tracer(..)
                                    , TracingFormatting(..), Transformable(..)
                                    , traceWith, mkLOMeta)
import           Ouroboros.Network.NodeToNode
                   (WithAddr(..), ErrorPolicyTrace(..))

instance DefinePrivacyAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)
instance DefineSeverity (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  defineSeverity (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error


-- transform @ErrorPolicyTrace@
instance Transformable Text IO (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage . toS $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr


instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= String "ErrorPolicyTrace"
             , "address" .= show addr
             , "event" .= show ev ]
