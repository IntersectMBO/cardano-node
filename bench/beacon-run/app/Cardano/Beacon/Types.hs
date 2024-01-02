{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Beacon.Types
       ( module Cardano.Beacon.Types
       , module Text
       ) where


import           Data.Aeson (FromJSON (..), FromJSONKey, withObject, (.:))
import           Data.Text as Text (Text)
import           Data.Time.Clock (UTCTime)


data EchoCommand =
    EchoCommand
  | DoNotEchoCommand
  deriving (Eq, Show)

data CommitInfo = CommitInfo
  { ciCommitSHA1  :: !String
  , ciCommitDate  :: !UTCTime
  }
  deriving Show

-- this instance parses the GitHub API query result
instance FromJSON CommitInfo where
  parseJSON = withObject "CommitInfo" $ \o -> do
    ciCommitSHA1 <- o .: "sha"
    commit       <- o .: "commit"
    author       <- commit .: "author"
    ciCommitDate <- author .: "date"
    pure CommitInfo{..}

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
  deriving Show

data InstallInfo = InstallInfo
  { installPath     :: FilePath
  , installNixPath  :: FilePath
  , installVersion  :: Version
  }
  deriving Show

data BeaconRunMeta = BeaconRunMeta {
    commitInfo  :: CommitInfo
  , nixPath     :: FilePath
  }
  deriving Show
