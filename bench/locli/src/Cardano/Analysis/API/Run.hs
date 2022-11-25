{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.API.Run (module Cardano.Analysis.API.Run) where

import Cardano.Prelude

import Control.Monad (fail)
import Data.Aeson qualified as Aeson

import Cardano.Util
import Cardano.Analysis.API.ChainFilter
import Cardano.Analysis.API.Context
import Cardano.Analysis.API.Ground

data AnalysisCmdError
  = AnalysisCmdError                                   !Text
  | MissingRunContext
  | MissingLogfiles
  | RunMetaParseError      !(JsonInputFile RunPartial) !Text
  | GenesisParseError      !(JsonInputFile Genesis)    !Text
  | ChainFiltersParseError !JsonFilterFile             !Text
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
    batch     <- meta .: "batch"
    manifest  <- meta .: "manifest"

    eraGtor   <- generator       .:? "era"
    eraTop    <- profile_content .:? "era"
    era <- case eraGtor <|> eraTop of
      Just x -> pure x
      Nothing -> fail "While parsing run metafile:  missing era specification"
    --
    let metadata = Metadata{..}
        genesis  = ()
    pure Run{..}

readRun :: JsonInputFile Genesis -> JsonInputFile RunPartial -> ExceptT AnalysisCmdError IO Run
readRun shelleyGenesis runmeta = do
  runPartial <- readJsonData runmeta        (RunMetaParseError runmeta)
  progress "meta"    (Q $ unJsonInputFile runmeta)
  run        <- readJsonData shelleyGenesis (GenesisParseError shelleyGenesis)
                <&> completeRun runPartial
  progress "genesis" (Q $ unJsonInputFile shelleyGenesis)
  progress "run"     (J run)
  pure run

 where
   completeRun :: RunPartial -> Genesis -> Run
   completeRun Run{..} g = Run { genesis = g, .. }
