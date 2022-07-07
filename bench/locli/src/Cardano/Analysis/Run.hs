{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.Run (module Cardano.Analysis.Run) where

import Cardano.Prelude

import Control.Monad (fail)
import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON(..), Object, ToJSON(..), withObject, (.:), (.:?))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as T

import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Version
import Cardano.Util

-- | Explain the poor human a little bit of what was going on:
data Anchor
  = Anchor
  { aRuns    :: ![Text]
  , aFilters :: ![FilterName]
  , aVersion :: !Version
  }

runAnchor :: Run -> [FilterName] -> Anchor
runAnchor Run{..} = tagsAnchor [tag metadata]

tagsAnchor :: [Text] -> [FilterName] -> Anchor
tagsAnchor aRuns aFilters =
  Anchor { aVersion = getVersion, .. }

renderAnchor :: Anchor -> Text
renderAnchor Anchor{..} = mconcat
  [ "runs: ",    T.intercalate ", " aRuns, ", "
  , "filters: ", case aFilters of
                   [] -> "unfiltered"
                   xs -> T.intercalate ", " (unFilterName <$> xs)
  , ", "
  , renderProgramAndVersion aVersion
  ]

data AnalysisCmdError
  = AnalysisCmdError                         !Text
  | MissingRunContext
  | MissingLogfiles
  | RunMetaParseError      !JsonRunMetafile  !Text
  | GenesisParseError      !JsonGenesisFile  !Text
  | ChainFiltersParseError !JsonFilterFile   !Text
  deriving Show

data ARunWith a
  = Run
  { genesisSpec      :: GenesisSpec
  , generatorProfile :: GeneratorProfile
  , metadata         :: Metadata
  , genesis          :: a
  }
  deriving (Generic, Show, ToJSON)

type RunPartial = ARunWith ()
type Run        = ARunWith Genesis

instance FromJSON RunPartial where
  parseJSON = withObject "Run" $ \v -> do
    meta :: Object <- v .: "meta"
    profile_content <- meta .: "profile_content"
    generator <- profile_content .: "generator"
    --
    genesisSpec      <- profile_content .: "genesis"
    generatorProfile <- parseJSON $ Aeson.Object generator
    --
    tag       <- meta .: "tag"
    profile   <- meta .: "profile"

    eraGtor   <- generator       .:? "era"
    eraTop    <- profile_content .:? "era"
    era <- case eraGtor <|> eraTop of
      Just x -> pure x
      Nothing -> fail "While parsing run metafile:  missing era specification"
    --
    let metadata = Metadata{..}
        genesis  = ()
    pure Run{..}

readRun :: JsonGenesisFile -> JsonRunMetafile -> ExceptT AnalysisCmdError IO Run
readRun shelleyGenesis runmeta = do
  runPartial <- firstExceptT (RunMetaParseError runmeta . T.pack)
                       (newExceptT $
                        Aeson.eitherDecode @RunPartial <$> LBS.readFile (unJsonRunMetafile runmeta))
  progress "meta"    (Q $ unJsonRunMetafile runmeta)
  run        <- firstExceptT (GenesisParseError shelleyGenesis . T.pack)
                       (newExceptT $
                        Aeson.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile shelleyGenesis))
                <&> completeRun runPartial
  progress "genesis" (Q $ unJsonGenesisFile shelleyGenesis)
  progress "run"     (J run)
  pure run

 where
   completeRun :: RunPartial -> Genesis -> Run
   completeRun Run{..} g = Run { genesis = g, .. }
