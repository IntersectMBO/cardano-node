{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.Run (module Cardano.Analysis.Run) where

import Prelude (String)
import Cardano.Prelude

import Control.Monad (fail)
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Aeson (FromJSON(..), Object, ToJSON(..), withObject, (.:), (.:?))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (intercalate)
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as Time
import Options.Applicative
import Options.Applicative qualified as Opt
import Text.Printf (printf)

import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground


data AnalysisCommand
  = MachineTimelineCmd
      MachineTimelineOutputFiles
  | BlockPropagationCmd
      BlockPropagationOutputFiles
  | SubstringKeysCmd
  | RunInfoCmd
  deriving (Show)

parseAnalysisCommand :: Parser AnalysisCommand
parseAnalysisCommand =
  Opt.subparser $
    mconcat
      [ Opt.command "machine-timeline"
          (Opt.info (MachineTimelineCmd
                       <$> parseMachineTimelineOutputFiles) $
            Opt.progDesc "Machine performance timeline")
      , Opt.command "block-propagation"
          (Opt.info (BlockPropagationCmd
                       <$> parseBlockPropagationOutputFiles) $
            Opt.progDesc "Block propagation")
      , Opt.command "substring-keys"
          (Opt.info (pure SubstringKeysCmd) $
            Opt.progDesc "Dump substrings that narrow logs to relevant subset")
      , Opt.command "chaininfo"
          (Opt.info (pure RunInfoCmd) $
            Opt.progDesc "Dump substrings that narrow logs to relevant subset")
      ]

renderAnalysisCommand :: AnalysisCommand -> Text
renderAnalysisCommand sc =
  case sc of
    MachineTimelineCmd {}  -> "analyse machine-timeline"
    BlockPropagationCmd {} -> "analyse block-propagation"
    SubstringKeysCmd {}    -> "analyse substring-keys"
    RunInfoCmd {}        -> "print extracted Run information"

data AnalysisCmdError
  = AnalysisCmdError                         !Text
  | MissingRunContext
  | MissingLogfiles
  | RunMetaParseError      !JsonRunMetafile  !Text
  | GenesisParseError      !JsonGenesisFile  !Text
  | ChainFiltersParseError !JsonFilterFile   !Text
  deriving Show

renderAnalysisCmdError :: AnalysisCommand -> AnalysisCmdError -> Text
renderAnalysisCmdError cmd err =
  case err of
    AnalysisCmdError  err' -> renderError cmd err'
      "Analysis command failed"
      pure
    MissingRunContext -> "Missing run context:  Shelley genesis and the run metafile are required."
    MissingLogfiles -> "Missing log files to analyse"
    RunMetaParseError (JsonRunMetafile fp) err' -> renderError cmd err'
      ("Benchmark run metafile parse failed: " <> T.pack fp)
      pure
    GenesisParseError (JsonGenesisFile fp) err' -> renderError cmd err'
      ("Genesis parse failed: " <> T.pack fp)
      pure
    ChainFiltersParseError (JsonFilterFile fp) err' -> renderError cmd err'
      ("Chain filter list parse failed: " <> T.pack fp)
      pure
 where
   renderError :: AnalysisCommand -> a -> Text -> (a -> [Text]) -> Text
   renderError cmd' cmdErr desc renderer =
      mconcat [ desc, ": "
              , renderAnalysisCommand cmd'
              , "  Error: "
              , mconcat (renderer cmdErr)
              ]

readRun :: JsonGenesisFile -> JsonRunMetafile -> ExceptT AnalysisCmdError IO Run
readRun shelleyGenesis runmeta = do
  progress "genesis" (Q $ unJsonGenesisFile shelleyGenesis)
  progress "meta"    (Q $ unJsonRunMetafile runmeta)
  runPartial <- firstExceptT (RunMetaParseError runmeta . T.pack)
                       (newExceptT $
                        Aeson.eitherDecode @RunPartial <$> LBS.readFile (unJsonRunMetafile runmeta))
  run        <- firstExceptT (GenesisParseError shelleyGenesis . T.pack)
                       (newExceptT $
                        Aeson.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile shelleyGenesis))
                <&> completeRun runPartial
  progress "run"     (J run)
  pure run

renderRunExport :: Run -> [Text]
renderRunExport Run{metadata=Metadata{..}, ..} =
  Data.Text.intercalate "," <$>
  [[ "Profile",    profile]
  ,[ "Era",        era ]
  ,[ "Date",       show timestamp]
  ,[ "Delegators", show $ delegators genesisSpec]
  ,[ "UTxO",       show $ utxo genesisSpec]
  ]

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

completeRun :: RunPartial -> Genesis -> Run
completeRun Run{..} g = Run { genesis = g, .. }

instance FromJSON GenesisSpec
instance FromJSON GeneratorProfile
instance FromJSON Genesis
instance FromJSON Metadata
instance FromJSON PParams
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
    era <- case eraGtor <> eraTop of
      Just x -> pure x
      Nothing -> fail "While parsing run metafile:  missing era specification"

    timestamp <- (meta .: "timestamp" :: Aeson.Parser Integer)
                  <&> Time.posixSecondsToUTCTime . realToFrac
    --
    let metadata = Metadata{..}
        genesis  = ()
    pure Run{..}

---
--- Run progress
---
data F
  = R String
  | Q String
  | L [String]
  | forall a. ToJSON a => J a

progress :: MonadIO m => String -> F -> m ()
progress key = putStrLn . T.pack . \case
  R x  -> printf "{ \"%s\":  %s }"    key x
  Q x  -> printf "{ \"%s\": \"%s\" }" key x
  L xs -> printf "{ \"%s\": \"%s\" }" key (Cardano.Prelude.intercalate "\", \"" xs)
  J x  -> printf "{ \"%s\": %s }" key (LBS.unpack $ Aeson.encode x)
