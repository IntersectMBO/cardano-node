{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Topology.Types (
    Topology (..)
  , Node (..)
  , Location (..)
  , AWSRegion (..)
) where

import           Prelude

import           GHC.Generics

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

--------------------------------------------------------------------------------

-- | A topology as it's used to define benchmarking profiles.
data Topology = Topology
  { coreNodes  :: [Node]
  , relayNodes :: [Node]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Topology

instance Aeson.FromJSON Topology

--------------------------------------------------------------------------------

-- | A node as it's used to define benchmarking profiles.
--   These nodes don't have remote addresses allocated, are referenced by name.
data Node = Node
  { name      :: String
  , nodeId    :: Int -- TODO: Swith to Word64 like Cardano.Topology ?
  -- ^ Placement, not present in Cardano.Node.Configuration.Topology.
  , region    :: Location
  -- ^ As Cardano.Node.Configuration.Topology but names not addresses.
  , producers :: [String]
  -- ^ Actually always "IOHK".
  , org       :: String
  , pools     :: Maybe Int
  , stakePool :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Node where
{--
  toEncoding = Aeson.genericToEncoding
    Aeson.defaultOptions
    { Aeson.omitNothingFields = True }
--}

instance Aeson.FromJSON Node

--------------------------------------------------------------------------------

-- | Location is either "loopback" used for local runs or an AWS Region name
-- used for cloud runs.
data Location = Loopback | AWS AWSRegion
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Location where
  toJSON Loopback = Aeson.toJSON ("loopback" :: Text.Text)
  toJSON (AWS r)  = Aeson.toJSON r

instance Aeson.FromJSON Location where
  parseJSON v =
    (Aeson.withText "Location" $ \case
      -- Either "loopback" or an already defined AWS Region.
      "loopback" -> return Loopback
      _ -> AWS <$> Aeson.parseJSON v
    )
    v

-- | The AWS Regions we support (No Availability Zone, the "c" in "us-east-2c").
data AWSRegion = AP_SOUTHEAST_2 | EU_CENTRAL_1 | US_EAST_1 | US_EAST_2
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON AWSRegion where
  toJSON AP_SOUTHEAST_2 = Aeson.toJSON ("ap-southeast-2" :: Text.Text)
  toJSON EU_CENTRAL_1   = Aeson.toJSON ("eu-central-1"   :: Text.Text)
  toJSON US_EAST_1      = Aeson.toJSON ("us-east-1"      :: Text.Text)
  toJSON US_EAST_2      = Aeson.toJSON ("us-east-2"      :: Text.Text)

instance Aeson.FromJSON AWSRegion where
  parseJSON = Aeson.withText "AWSRegion" $ \t -> case t of
    "ap-southeast-2" -> return AP_SOUTHEAST_2
    "eu-central-1"   -> return EU_CENTRAL_1
    "us-east-1"      -> return US_EAST_1
    "us-east-2"      -> return US_EAST_2
    _                -> fail $ "Unknown AWSRegion: \"" ++ Text.unpack t ++ "\""
