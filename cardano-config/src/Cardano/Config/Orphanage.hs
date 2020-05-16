{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage () where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson
import           Network.Socket (PortNumber)
import           Data.Scientific (coefficient)
import qualified Data.Text as Text

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Cardano.Chain.Update as Update
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId (..))

deriving instance Show TracingVerbosity

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> panic $ "Parsing of TracingVerbosity failed, "
                 <> err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = panic $ "Parsing of TracingVerbosity failed due to type mismatch. "
                             <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance FromJSON NodeId where
  parseJSON v = CoreId . CoreNodeId <$> parseJSON v


instance FromJSON PortNumber where
  parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
                                 Just port -> pure port
                                 Nothing -> panic $ (show portNum)
                                                  <> " is not a valid port number."
  parseJSON invalid  = panic $ "Parsing of port number failed due to type mismatch. "
                             <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    panic $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

