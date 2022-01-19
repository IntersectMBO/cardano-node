{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Cardano.Analysis.Version
  (Version (..), getVersion)
where

import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Text                (Text, pack)
import Data.Version             (showVersion)
import GHC.Generics             (Generic)
import Paths_locli              (version)
import Prelude                  (Show)
import Cardano.Config.Git.Rev qualified (gitRev)


data Version =
  Version
  { gitRev  :: Text
  , version :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)

getVersion :: Version
getVersion =
  Version
    Cardano.Config.Git.Rev.gitRev
    (pack (showVersion Paths_locli.version))
