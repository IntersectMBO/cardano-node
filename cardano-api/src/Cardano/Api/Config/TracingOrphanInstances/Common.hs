{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Config.TracingOrphanInstances.Common
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

    -- * Tracer and related
  , Tracer
  , LogObject(..)
  , LOContent(..)
  , mkLOMeta
  ) where

import           Data.Aeson (ToJSON(..), Value (..), (.=))
import           Data.Void (Void)

import           Cardano.BM.Tracing
                   (ToObject(..), TracingVerbosity(..), Transformable(..),
                    HasSeverityAnnotation(..), Severity(..),
                    HasPrivacyAnnotation(..), Tracer(..), )
import           Cardano.BM.Data.LogItem
                   (LOContent (..), LogObject (..), mkLOMeta)
import           Cardano.BM.Data.Tracer
                   (trStructured, HasTextFormatter (..), trStructuredText,
                    emptyObject, mkObject)


-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantaited to 'Void', when there are
-- no cases needed.
--
instance ToObject Void where
  toObject _verb x = case x of {}
