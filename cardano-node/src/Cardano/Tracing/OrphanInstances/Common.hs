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

import           Cardano.Prelude
import           Prelude (fail)

import           Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS
import           Data.Scientific (coefficient)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                   mkLOMeta)
import           Cardano.BM.Data.Tracer (HasTextFormatter (..), emptyObject, mkObject, trStructured,
                   trStructuredText)
import           Cardano.BM.Stats
import           Cardano.BM.Tracing (HasPrivacyAnnotation (..), HasSeverityAnnotation (..),
                   Severity (..), ToObject (..), Tracer (..), TracingVerbosity (..),
                   Transformable (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Alonzo as Alonzo
import           Cardano.Ledger.Alonzo.PParams (PParamsUpdate)
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure)
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Slotting.Block (BlockNo (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Ouroboros.Network.Block (HeaderHash, Tip (..))
import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)

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

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    fail $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> show invalid

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

instance ToJSON (OneEraHash xs) where
  toJSON (OneEraHash bs) =
    toJSON . Text.decodeLatin1 . B16.encode . SBS.fromShort $ bs

deriving newtype instance ToJSON ByronHash
deriving newtype instance ToJSON BlockNo

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

instance (Ledger.Era era, Show (Ledger.Value era), ToJSON (Ledger.Value era))
    => ToJSON (TxOut era) where
  toJSON (Alonzo.TxOut addr v dataHash) =
    object [ "address" .= toJSON addr
           , "value" .= toJSON v
           , "datahash" .= case strictMaybeToMaybe dataHash of
                             Nothing -> Aeson.Null
                             Just dHash ->
                               Aeson.String . Crypto.hashToTextAsHex
                                 $ SafeHash.extractHash dHash
           ]


instance ToJSON (PParamsUpdate crypto) where
  toJSON _ = Aeson.Null -- TODO: Implement


instance ToObject (UtxoPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  toObject _ _ = panic "ToJSON: UtxoPredicateFailure not implemented yet"

instance ToObject (AlonzoBbodyPredFail (Alonzo.AlonzoEra StandardCrypto)) where
  toObject _ _ = panic "ToJSON: AlonzoBbodyPredFail not implemented yet"
