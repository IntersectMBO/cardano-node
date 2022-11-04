{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module  Cardano.Benchmarking.Publish.Types where

import           Data.ByteString.Char8  (ByteString)
import           Data.Text              as T (Text, take, unpack)
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Clock.System
import           GHC.Generics           (Generic)

import           Data.Aeson


data ClusterRun
  = ClusterRun {
      runMeta        :: !ByteString
    , runBlockProp   :: !(Maybe ByteString)
    , runClusterPerf :: !(Maybe ByteString)
    , metaStub       :: !MetaStub
    }

-- we extract only very few attributes from meta.json to perform
-- a necessary minimum of data normalization to describe a run
data MetaStub
  = MetaStub {
      profile   :: Text
    , batch     :: Text
    , timestamp :: UTCTime
    }
    deriving (Generic)

instance Show MetaStub where
  show (MetaStub a b c)
    = concat [T.unpack (T.take 6 b), "--", show c, "--", T.unpack a]

instance FromJSON MetaStub where
    parseJSON = withObject "MetaStub" $ \o_ -> do
      o <- o_ .: "meta"
      pins <- o .: "pins"
      MetaStub
        <$> o .: "profile"
        <*> pins .: "cardano-node"
        <*> (systemToUTCTime <$> o .: "timestamp")

test :: FilePath -> IO ()
test metaJson = do
    stub :: Either String MetaStub <-
        eitherDecodeFileStrict metaJson
    print stub
