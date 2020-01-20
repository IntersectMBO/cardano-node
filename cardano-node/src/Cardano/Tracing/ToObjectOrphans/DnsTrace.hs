{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tracing.ToObjectOrphans.DnsTrace () where

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
import           Ouroboros.Network.Subscription (DnsTrace (..), WithDomainName (..))

instance DefinePrivacyAnnotation (WithDomainName DnsTrace)
instance DefineSeverity (WithDomainName DnsTrace) where
  defineSeverity (WithDomainName _ ev) = case ev of
    DnsTraceLookupException {} -> Error
    DnsTraceLookupAError {} -> Error
    DnsTraceLookupAAAAError {} -> Error
    DnsTraceLookupIPv6First -> Info
    DnsTraceLookupIPv4First -> Info
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug

-- transform @DnsTrace@
instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage . toS $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DnsTrace"
             , "domain" .= show dom
             , "event" .= show ev ]
