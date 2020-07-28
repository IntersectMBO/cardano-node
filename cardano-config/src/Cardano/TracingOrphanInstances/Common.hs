{-# LANGUAGE EmptyCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TracingOrphanInstances.Common
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
  , trStructuredText
  , HasTextFormatter(..)

    -- * Severity and Privacy
  , HasSeverityAnnotation(..)
  , Severity(..)
  , HasPrivacyAnnotation(..)
  , PrivacyAnnotation(..)

    -- * Tracer and related
  , Tracer
  , LogObject(..)
  , LOContent(..)
  , mkLOMeta
  ) where

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import           Data.Void (Void)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                     PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Tracer (HasTextFormatter (..), emptyObject,
                     mkObject, trStructured, trStructuredText)
import           Cardano.BM.Tracing (HasPrivacyAnnotation (..),
                     HasSeverityAnnotation (..), Severity (..), ToObject (..),
                     Tracer (..), TracingVerbosity (..), Transformable (..))


-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantaited to 'Void', when there are
-- no cases needed.
--
instance ToObject Void where
  toObject _verb x = case x of {}
