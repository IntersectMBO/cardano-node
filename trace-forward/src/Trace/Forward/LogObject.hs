{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Trace.Forward.LogObject () where

import           Codec.Serialise (Serialise (..))
import           Data.Aeson (Value)
import           Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import           GHC.Generics (Generic)

import           Cardano.BM.Data.Aggregated (Aggregated, BaseStats, EWMA, Measurable, Stats)
import           Cardano.BM.Data.BackendKind (BackendKind (..))
import           Cardano.BM.Data.Counter (Counter, CounterState, CounterType)
import           Cardano.BM.Data.LogItem (CommandValue (..), LogObject (..), LOContent (..),
                                          LOMeta (..), MonitorAction (..), PrivacyAnnotation (..))
import           Cardano.BM.Data.Severity (Severity)

-- Instances we need to serialize 'LogObject' from 'iohk-monitoring-framework'.

deriving instance Generic BackendKind
deriving instance Generic CommandValue
deriving instance Generic (LOContent a)
deriving instance Generic LOMeta
deriving instance Generic MonitorAction
deriving instance Generic PrivacyAnnotation
deriving instance Generic (LogObject a)

-- We cannot derive 'Scientific' instance automatically
-- because of hidden data constructors.
instance Serialise Scientific where
  encode s = encode (coefficient s, base10Exponent s)
  decode = uncurry scientific <$> decode

instance Serialise Aggregated
instance Serialise BackendKind
instance Serialise BaseStats
instance Serialise CommandValue
instance Serialise Counter
instance Serialise CounterState
instance Serialise CounterType
instance Serialise EWMA
instance (Serialise a) => Serialise (LOContent a)
instance Serialise LOMeta
instance Serialise Measurable
instance Serialise MonitorAction
instance Serialise PrivacyAnnotation
instance Serialise Severity
instance Serialise Stats
instance Serialise Value
instance (Serialise a) => Serialise (LogObject a)
