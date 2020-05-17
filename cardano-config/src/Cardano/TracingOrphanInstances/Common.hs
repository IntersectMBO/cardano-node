{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyCase                  #-}

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

    -- * Tracer and related
  , Tracer
  , LogObject(..)
  , LOContent(..)
  , mkLOMeta
  ) where

import           Data.Void (Void)
import           Data.Aeson (ToJSON(..), FromJSON(..), toJSON, Value (..), (.=))

import           Cardano.BM.Tracing
                   (ToObject(..), TracingVerbosity(..), Transformable(..),
                    HasSeverityAnnotation(..), Severity(..),
                    HasPrivacyAnnotation(..), Tracer(..), )
import           Cardano.BM.Data.LogItem
                   (LOContent (..), LogObject (..), mkLOMeta)
import           Cardano.BM.Data.Tracer
                   (trStructured, HasTextFormatter (..), trStructuredText,
                    emptyObject, mkObject)

import           Cardano.Slotting.Slot (SlotNo(..), EpochNo(..))



-- These ones are all just newtype wrappers of numbers,
-- so newtype deriving for the JSON format is ok.
deriving newtype instance ToJSON   SlotNo
deriving newtype instance FromJSON SlotNo

-- These ones are all just newtype wrappers of numbers,
-- so newtype deriving for the JSON format is ok.
deriving newtype instance ToJSON   EpochNo
deriving newtype instance FromJSON EpochNo


-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantaited to 'Void', when there are
-- no cases needed.
--
instance ToObject Void where
  toObject _verb x = case x of {}

