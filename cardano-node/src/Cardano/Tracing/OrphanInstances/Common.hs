{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracing.OrphanInstances.Common
  (
    -- * ToObject and helpers
    ToObject(..)
  , TracingVerbosity(..)
  , mkObject
  , emptyObject
  , ToJSON
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

import           Data.Aeson hiding (Value)
import           Data.Scientific (coefficient)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import           Network.Socket (PortNumber)
import           Text.Read (readMaybe)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                   mkLOMeta)
import           Cardano.BM.Data.Tracer (HasTextFormatter (..), emptyObject, mkObject, trStructured,
                   trStructuredText)
import           Cardano.BM.Stats
import           Cardano.BM.Tracing (HasPrivacyAnnotation (..), HasSeverityAnnotation (..),
                   Severity (..), ToObject (..), Tracer (..), TracingVerbosity (..),
                   Transformable (..))
import           Cardano.Node.Handlers.Shutdown ()
-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantiated to 'Void', when there are
-- no cases needed.
--
instance ToObject Void where
  toObject _verb x = case x of {}

deriving instance Show TracingVerbosity

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    invalid -> fail $ "Parsing of TracingVerbosity failed, "
                    <> Text.unpack invalid <> " is not a valid TracingVerbosity"
  parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
                            <> "Encountered: " <> show invalid

instance FromJSON PortNumber where
  parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
    Just port -> pure port
    Nothing -> fail $ show portNum <> " is not a valid port number."
  parseJSON invalid  = fail $ "Parsing of port number failed due to type mismatch. "
                            <> "Encountered: " <> show invalid

instance HasPrivacyAnnotation  ResourceStats
instance HasSeverityAnnotation ResourceStats where
  getSeverityAnnotation _ = Info
instance Transformable Text IO ResourceStats where
  trTransformer = trStructured

instance ToObject ResourceStats where
  toObject _verb stats =
    case toJSON stats of
      Object x -> x
      _ -> mempty
