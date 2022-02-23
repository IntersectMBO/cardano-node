{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Prelude
import           Prelude (fail)

import           Cardano.Api.Orphans ()

import           Data.Aeson.Types
import qualified Data.Text as Text

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import qualified Cardano.Chain.Update as Update
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Shelley.CompactAddr as Shelley
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..))

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> fail $ "Parsing of TracingVerbosity failed, "
                <> Text.unpack err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
                           <> "Encountered: " <> show invalid

deriving instance Show TracingVerbosity

instance ToJSON (Shelley.CompactAddr StandardCrypto) where
  toJSON = toJSON . Shelley.decompactAddr

--Not currently needed, but if we do need it, this is the general instance.
--instance (ToJSON a, Ledger.Compactible a) => ToJSON (Ledger.CompactForm a) where
--  toJSON = toJSON  . Ledger.fromCompact

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    fail $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> show invalid

instance ToJSON AcceptedConnectionsLimit where
  toJSON AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit
          , acceptedConnectionsSoftLimit
          , acceptedConnectionsDelay
          } =
    object [ "AcceptedConnectionsLimit" .=
      object [ "hardLimit" .=
                  toJSON acceptedConnectionsHardLimit
             , "softLimit" .=
                  toJSON acceptedConnectionsSoftLimit
             , "delay" .=
                  toJSON acceptedConnectionsDelay
             ]
           ]

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"
