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

newtype Chain = Chain Text
        deriving (Eq, Ord, Show, FromJSON, FromJSONKey)
          via Text

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


data BeaconOptions_ = BeaconOptions_ {
        -- | Baseline version.
        versionA           :: !Version
        -- | Other version to compare.
      , versionB           :: !Version
        -- | Path to the Cardano node's 'mainnet/db' folder. If you built the
        -- node from source and run it from the `cardano-node` directory this
        -- path would point to this directory.
      , nodeHome           :: !FilePath
        -- | path for the config.json file. This is relative to @nodeHome@. When
        -- not present this defaults to
        -- "/configuration/cardano/mainnet-config.json"
      , configPath         :: !FilePath
        -- | path for the db passed to db-analyzer. This is relative to
        -- @nodeHome@. When not present this defaults to "/mainnet/db"
      , dbPath             :: !FilePath
      , analyseFromSlot    :: !Int
      , numBlocksToProcess :: !Int
    }
    deriving Show


data BeaconRunMeta = BeaconRunMeta {
    commitInfo  :: CommitInfo
  , nixPath     :: FilePath
  }
  deriving Show
