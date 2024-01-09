{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Beacon.Types
       ( module Cardano.Beacon.Types
       , module Text
       ) where


import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.List (intercalate)
import           Data.Text as Text (Text)
import qualified Data.Text as T (unpack)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)

import           Cardano.Beacon.SlotDataPoint (SortedDataPoints)


data EchoCommand =
    EchoCommand
  | DoNotEchoCommand
  deriving (Eq, Show)

newtype ChainName = ChainName Text
        deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON)
          via Text

data CommitInfo = CommitInfo
  { ciCommitSHA1  :: !String
  , ciCommitDate  :: !UTCTime
  }
  deriving (Show, Generic)

-- this instance also parses the GitHub API query result
instance FromJSON CommitInfo where
  parseJSON a =
    parseNative a <|> parseFromGitHub a
    where
      parseNative = withObject "CommitInfo" $ \o -> do
        ciCommitSHA1 <- o .: "ciCommitSHA1"
        ciCommitDate <- o .: "ciCommitDate"
        pure CommitInfo{..}

      parseFromGitHub = withObject "CommitInfo" $ \o -> do
        ciCommitSHA1 <- o .: "sha"
        commit       <- o .: "commit"
        author       <- commit .: "author"
        ciCommitDate <- author .: "date"
        pure CommitInfo{..}

instance ToJSON CommitInfo where
  toJSON = genericToJSON aesonNoTagFields

data Version = Version {
    -- | The git commit hash or tag or branch name to build db-analyser from.
    -- Commit must be publicly visible on GitHub; shortened hashes are valid.
    verGitRef   :: String
    -- | Compiler version used to compile db-analyser.
    --
    -- This comes from the 'ouroboros-consensus' 'nix' setup.
    -- Since relying on this is brittle anyway, we do not define a type for it, and rely instead on a free-form string.
  , verCompiler :: String
  }
  deriving (Show, Generic)

instance ToJSON Version where
  toJSON = genericToJSON aesonNoTagFields

instance FromJSON Version where
  parseJSON = genericParseJSON aesonNoTagFields

data InstallInfo = InstallInfo
  { installPath     :: FilePath
  , installNixPath  :: FilePath
  , installVersion  :: Version
  }
  deriving Show

data BeaconRunMeta = BeaconRunMeta {
    commit      :: CommitInfo
  , version     :: Version
  , chain       :: ChainName
  , nixPath     :: FilePath
  , host        :: String
  , date        :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON BeaconRunMeta where
  toJSON = genericToJSON aesonNoTagFields

instance FromJSON BeaconRunMeta where
  parseJSON = genericParseJSON aesonNoTagFields

data BeaconRun = BeaconRun {
    rMeta :: BeaconRunMeta
  , rData :: SortedDataPoints
  }
  deriving Show

instance FromJSON BeaconRun where
  parseJSON = withObject "BeaconRun" $ \o ->
    BeaconRun
      <$> o .: "meta"
      <*> o .: "data"

toSlug :: BeaconRunMeta -> String
toSlug BeaconRunMeta{..} =
  intercalate "-"
    [ commitShort commit
    , verCompiler version
    , chainShort  chain
    ]
  where
    commitShort = take 9 . ciCommitSHA1
    chainShort (ChainName name) = take 16 $ T.unpack name

aesonNoTagFields :: Options
aesonNoTagFields = defaultOptions { sumEncoding = ObjectWithSingleField }
