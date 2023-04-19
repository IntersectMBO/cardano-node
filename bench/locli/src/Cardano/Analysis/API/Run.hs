{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.API.Run (module Cardano.Analysis.API.Run) where

import           Cardano.Prelude

import           Control.Monad (fail)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import           Cardano.Analysis.API.ChainFilter
import           Cardano.Analysis.API.Context
import           Cardano.Analysis.API.Ground
import           Cardano.Analysis.API.Types
import           Cardano.Util

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

-- | Given a Summary object,
--   produce a JSON file readable by the above RunPartial FromJSON instance.
--   Keep in sync.  Better still, automate it so it's not necessary.
summaryMetaJson :: Summary f -> Value
summaryMetaJson Summary{sumMeta=Metadata{..}, ..} =
  object [ "meta" .= meta ]
 where meta =
         object $
         -- keep in sync with 'data Metadata'
         [ "tag"       .= tag
         , "batch"     .= batch
         , "profile"   .= profile
         , "era"       .= era
         , "manifest"  .= manifest
         ] <>
         -- keep in sync with the above instance
         [ "profile_content" .=
           object
           [ "generator" .= toJSON sumWorkload
           , "genesis"   .= toJSON sumGenesisSpec
           ]
         ]

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
