{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Trace.Forward.ReqResp
  ( Request (..)
  , Response (..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Aeson (Value)
import           Data.Word (Word16)
import           Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.Aggregated (Aggregated, BaseStats, EWMA, Measurable, Stats)
import           Cardano.BM.Data.BackendKind (BackendKind (..))
import           Cardano.BM.Data.Counter (Counter, CounterState, CounterType)
import           Cardano.BM.Data.LogItem (CommandValue (..), LogObject (..), LOContent (..),
                                          LOMeta (..), MonitorAction (..), PrivacyAnnotation (..))
import           Cardano.BM.Data.Severity (Severity)

-- | The request for N 'LogObject's.
-- The acceptor will send this request to the forwarder.
newtype Request = GetLogObjects Word16
  deriving (Eq, Generic, Show)

-- | The response with 'LogObject's.
-- The forwarder will send it to the acceptor as a reply for the request.
-- Please note that the list of 'LogObject's can be empty (for example,
-- if the forwarder's log queue is empty).
newtype Response a = ResponseLogObjects [LogObject a]
  deriving (Eq, Generic, Show)

instance ShowProxy Request
instance Serialise Request

-- Instances we need to serialize 'LogObject's.

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

instance (ShowProxy a, Typeable a) => ShowProxy (Response a)
-- instance (ShowProxy a) => ShowProxy (Response a)
instance (Serialise a) => Serialise (Response a)
