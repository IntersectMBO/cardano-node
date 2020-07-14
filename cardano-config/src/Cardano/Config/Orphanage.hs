{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage () where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS
import           Data.Scientific (coefficient)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber)

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Cardano.Chain.Update as Update
import           Cardano.Slotting.Block (BlockNo (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Network.Block (HeaderHash, Tip (..))


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

instance FromJSON CoreNodeId where
  parseJSON v = CoreNodeId <$> parseJSON v


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

-- This instance is temporarily duplicated in cardano-cli

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

instance ToJSON (OneEraHash xs) where
  toJSON (OneEraHash bs) = toJSON . Text.decodeLatin1 . B16.encode . SBS.fromShort $ bs

-- This instance is temporarily duplicated in cardano-cli\
deriving newtype instance ToJSON BlockNo
