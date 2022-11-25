{-# LANGUAGE DeriveAnyClass #-}
module Cardano.Analysis.API.LocliVersion (module Cardano.Analysis.API.LocliVersion) where

import Cardano.Prelude (NFData, mconcat)
import Cardano.Git.Rev qualified (gitRev)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text, pack, take)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_locli (version)
import Prelude (Show)


data LocliVersion =
  LocliVersion
  { gitRev  :: Text
  , version :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
  deriving anyclass NFData

getLocliVersion :: LocliVersion
getLocliVersion =
  LocliVersion
    Cardano.Git.Rev.gitRev
    (pack (showVersion Paths_locli.version))

renderProgramAndVersion :: LocliVersion -> Text
renderProgramAndVersion v = mconcat
  [ "locli "
  , Cardano.Analysis.API.LocliVersion.version v
  , " (", take 6 (gitRev v), ")"
  ]
