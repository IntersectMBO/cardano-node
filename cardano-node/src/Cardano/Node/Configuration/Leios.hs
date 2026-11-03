{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Configuration.Leios(
  LeiosDbConfig(..)
  , LeiosMinOfferLead(..)
  , LeiosAdaptiveEndorsements(..)
  ) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), object,
                   withObject, (.!=), (.:), (.:?), (.=))
import           Data.Word (Word64)

import           LeiosAdaptiveEndorsements (LeiosAdaptiveEbConfig (..),
                   defaultLeiosAdaptiveEbConfig)

-- | Which LeiosDB backend to run. Where its files live is not configurable
-- here: the SQLite backend has a volatile and an immutable partition, and
-- each follows the node's own 'DatabasePath' the way the VolatileDB and the
-- ImmutableDB do. Naming them separately could only put them somewhere that
-- contradicts that.
data LeiosDbConfig = LeiosDbInMemory
   | LeiosDbSQLite
   deriving (Eq, Show)

instance FromJSON LeiosDbConfig where
  parseJSON = withObject "LeiosDbConfig" $ \o -> do
    backend :: String <- o .: "Backend"
    case backend of
      "InMemory" -> return LeiosDbInMemory
      "SQLite" -> return LeiosDbSQLite
      _ -> fail $ "Invalid LeiosDb backend " <> backend <> ", did you mean InMemory or SQLite?"

instance ToJSON LeiosDbConfig where
  toJSON LeiosDbInMemory =
    object
      [ "Backend" .=  String "InMemory"
      ]
  toJSON LeiosDbSQLite =
    object
      [ "Backend" .= String "SQLite"
      ]

-- | How much younger than this node's immutable tip an endorser block must be
-- for the node to offer it onward, in slots.
--
-- Absent from the configuration, the consensus default applies: an hour of
-- one-second slots. Not a protocol parameter, so two peers need not agree on
-- it, and the risk of choosing one too small falls on the node that chose it.
--
-- Every endorser block worth offering sits between the immutable tip and the
-- wall clock, so a lead approaching that distance offers nothing at all. That
-- is what a short-horizon network has to avoid: on one whose immutable tip
-- trails its selection by fewer slots than the lead, no peer ever obtains an
-- endorser block it did not forge, so no election reaches a quorum and nothing
-- is certified.
newtype LeiosMinOfferLead = LeiosMinOfferLead { unLeiosMinOfferLead :: Word64 }
  deriving (Eq, Show)

instance FromJSON LeiosMinOfferLead where
  parseJSON = fmap LeiosMinOfferLead . parseJSON

instance ToJSON LeiosMinOfferLead where
  toJSON = toJSON . unLeiosMinOfferLead

-- | Whether, and how, this node scales the endorser blocks it forges down
-- when announcements miss their quorum.
--
-- Absent from the configuration, or with @Enabled: false@, the node forges
-- endorser blocks at the capacity the protocol allows. Every other field is
-- optional and defaults to 'defaultLeiosAdaptiveEbConfig':
--
-- > LeiosAdaptiveEndorsements:
-- >   Enabled: true
-- >   Decrease: 0.8
-- >   Increase: 0.02
-- >   MinScale: 0.1
newtype LeiosAdaptiveEndorsements = LeiosAdaptiveEndorsements
  { unLeiosAdaptiveEndorsements :: Maybe LeiosAdaptiveEbConfig }
  deriving (Eq, Show)

instance FromJSON LeiosAdaptiveEndorsements where
  parseJSON = withObject "LeiosAdaptiveEndorsements" $ \o -> do
    enabled <- o .:? "Enabled" .!= True
    aebDecrease <- o .:? "Decrease" .!= aebDecrease defaults
    aebIncrease <- o .:? "Increase" .!= aebIncrease defaults
    aebMinScale <- o .:? "MinScale" .!= aebMinScale defaults
    pure . LeiosAdaptiveEndorsements $
      if enabled then Just MkLeiosAdaptiveEbConfig{..} else Nothing
   where
    defaults = defaultLeiosAdaptiveEbConfig

instance ToJSON LeiosAdaptiveEndorsements where
  toJSON (LeiosAdaptiveEndorsements Nothing) = object ["Enabled" .= False]
  toJSON (LeiosAdaptiveEndorsements (Just cfg)) =
    object
      [ "Enabled" .= True
      , "Decrease" .= aebDecrease cfg
      , "Increase" .= aebIncrease cfg
      , "MinScale" .= aebMinScale cfg
      ]
