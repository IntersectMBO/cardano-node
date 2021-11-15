{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace.Forward.Protocol.TraceObject.Item
  ( TraceItem (..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Text (Text)
import           GHC.Generics
import           Test.QuickCheck

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

data Severity = Debug | Info | Notice
  deriving (Show, Eq, Ord, Enum, Generic)

instance Arbitrary Severity where
  arbitrary = oneof
    [ pure Debug
    , pure Info
    , pure Notice
    ]

instance Serialise Severity

data DetailLevel = Brief | Regular
  deriving (Show, Eq, Ord, Enum, Generic)

instance Arbitrary DetailLevel where
  arbitrary = oneof
    [ pure Brief
    , pure Regular
    ]

instance Serialise DetailLevel

-- | Trace items that will be used during testing.
--   This type is an imitation of the real 'TraceObject' that will be used by the node.
data TraceItem = TraceItem
  { tiSeverity :: Severity
  , tiDetails  :: DetailLevel
  , tiHostname :: String
  , tiThreadId :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Serialise TraceItem
instance ShowProxy TraceItem

instance Arbitrary TraceItem where
  arbitrary = TraceItem
    <$> arbitrary
    <*> arbitrary
    <*> oneof [pure "nixos", pure "Darwin", pure "testHost"]
    <*> oneof [pure "1", pure "10", pure "14"]

instance Arbitrary (NonEmpty TraceItem) where
  arbitrary = fromList <$> listOf1 arbitrary
