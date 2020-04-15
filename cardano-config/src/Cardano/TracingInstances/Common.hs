module Cardano.TracingInstances.Common
  ( 
    -- * ToObject and helpers
    ToObject(..)
  , TracingVerbosity(..)
  , mkObject
  , emptyObject
  , ToJSON
  , Value (..)
  , toJSON
  , (.=)

    -- * Transformable and helpers
  , Transformable(..)
  , trStructured
  , defaultTextTransformer
  , TracingFormatting(..)

    -- * Severity and Privacy
  , HasSeverityAnnotation(..)
  , Severity(..)
  , HasPrivacyAnnotation(..)

    -- * Tracer and related
  , Tracer
  , LogObject(..)
  , LOContent(..)
  , mkLOMeta
  ) where

import           Cardano.Prelude

import           Data.Aeson (ToJSON(..), toJSON, Value (..), (.=))

import           Cardano.BM.Tracing
                   (ToObject(..), TracingVerbosity(..), Transformable(..),
                    HasSeverityAnnotation(..), Severity(..),
                    HasPrivacyAnnotation(..),
                    Trace, TracingFormatting(..),
                    Tracer(..), traceWith)
import           Cardano.BM.Data.LogItem
                   (LOContent (..), LogObject (..), mkLOMeta)
import           Cardano.BM.Data.Tracer
                   (trStructured, emptyObject, mkObject)


defaultTextTransformer
  :: ( MonadIO m
     , HasPrivacyAnnotation b
     , HasSeverityAnnotation b
     , Show b
     , ToObject b)
  => TracingFormatting
  -> TracingVerbosity
  -> Trace m Text
  -> Tracer m b
defaultTextTransformer TextualRepresentation _verb tr =
  Tracer $ \s -> do
    meta <- mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s)
    traceWith tr (mempty, LogObject mempty meta (LogMessage $ show s))
defaultTextTransformer _ verb tr =
  trStructured verb tr

