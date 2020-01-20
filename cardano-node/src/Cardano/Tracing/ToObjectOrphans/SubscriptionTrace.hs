{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.Tracing.ToObjectOrphans.SubscriptionTrace () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson hiding (Error)
import qualified Network.Socket as Socket (SockAddr)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject(..))
import           Cardano.BM.Data.Tracer ( definePrivacyAnnotation, trStructured
                                        , mkObject)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity (..), Tracer(..)
                                    , TracingFormatting(..), Transformable(..)
                                    , traceWith, mkLOMeta)
import           Ouroboros.Network.Subscription ( ConnectResult(..), SubscriptionTrace (..)
                                                , WithDomainName(..), WithIPList(..))

-- instances of @DefinePrivacyAnnotation@ and @DefineSeverity@
instance DefinePrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))
instance DefineSeverity (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  defineSeverity (WithIPList _ _ ev) = case ev of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess         -> Info
      ConnectSuccessLast     -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

instance DefinePrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))
instance DefineSeverity (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  defineSeverity (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Info
    SubscriptionTraceConnectEnd {} -> Info
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug

-- transform @SubscriptionTrace@
instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage . toS $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr


instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage . toS $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "WithIPList SubscriptionTrace"
             , "localAddresses" .= show localAddresses
             , "dests" .= show dests
             , "event" .= show ev ]

instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "SubscriptionTrace"
             , "domain" .= show dom
             , "event" .= show ev ]
