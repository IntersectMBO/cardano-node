{-# LANGUAGE DeriveAnyClass #-}
module Cardano.Analysis.Version
  (Version (..), getVersion, renderProgramAndVersion)
where

import Cardano.Prelude (NFData, mconcat)
import Cardano.Git.Rev qualified (gitRev)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text, pack, take)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_locli (version)
import Prelude (Show)


data Version =
  Version
  { gitRev  :: Text
  , version :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
  deriving anyclass NFData

getVersion :: Version
getVersion =
  Version
    Cardano.Git.Rev.gitRev
    (pack (showVersion Paths_locli.version))

renderProgramAndVersion :: Version -> Text
renderProgramAndVersion v = mconcat
  [ "locli "
  , Cardano.Analysis.Version.version v
  , " (", take 6 (gitRev v), ")"
  ]
