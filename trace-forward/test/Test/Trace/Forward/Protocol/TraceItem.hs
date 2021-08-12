{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace.Forward.Protocol.TraceItem
  ( TraceItem (..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Text (Text)
import           Data.Time (UTCTime (..), fromGregorian)
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

instance Arbitrary UTCTime where
  arbitrary = oneof
    [ pure $ UTCTime (fromGregorian 2021 7 24) ((22 * 3600) + (15 * 60) +  1)
    , pure $ UTCTime (fromGregorian 2021 7 25) ((12 * 3600) +  (4 * 60) + 37)
    , pure $ UTCTime (fromGregorian 2021 7 26) ((23 * 3600) + (19 * 60) + 56)
    ]

-- | Trace items that will be used during testing.
--   This type is similar to the real 'TraceObject' that will be used by the node.
data TraceItem = TraceItem
  { tiHuman     :: Maybe String
  , tiNamespace :: [String]
  , tiSeverity  :: Severity
  , tiDetails   :: DetailLevel
  , tiTimestamp :: UTCTime
  , tiHostname  :: String
  , tiThreadId  :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Serialise TraceItem
instance ShowProxy TraceItem

instance Arbitrary TraceItem where
  arbitrary = TraceItem
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> oneof [pure "1", pure "10"]

instance Arbitrary (NonEmpty TraceItem) where
  arbitrary = fromList <$> listOf1 arbitrary
