{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------

import           Prelude

import           Data.Function ((&))
import           System.Environment (lookupEnv)
import           GHC.Stack (HasCallStack)
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: aeson-pretty
import           Data.Aeson.Encode.Pretty as Aeson
-- Package: bytestring.
import qualified Data.ByteString.Lazy.Char8 as BSL8
-- Package: containers.
import qualified Data.Map.Strict as Map
-- Package: optparse-applicative-fork.
import qualified Options.Applicative as OA
-- Package: self.
import qualified Cardano.Benchmarking.Profile as Profile
import qualified Cardano.Benchmarking.Profile.NodeSpecs as NodeSpecs
import qualified Cardano.Benchmarking.Profile.Types as Types
-- Profiles to export!
-- Static / built-in / profiles part of the test-suite.
import           Cardano.Benchmarking.Profile.Builtin.Cloud               (profilesNoEraCloud)
import           Cardano.Benchmarking.Profile.Builtin.Empty               (profilesNoEraEmpty)
import           Cardano.Benchmarking.Profile.Builtin.ForgeStress         (profilesNoEraForgeStress)
import           Cardano.Benchmarking.Profile.Builtin.K3                  (profilesNoEraK3)
import           Cardano.Benchmarking.Profile.Builtin.Legacy.Dense        (profilesNoEraDense)
import           Cardano.Benchmarking.Profile.Builtin.Legacy.Dish         (profilesNoEraDish)
import           Cardano.Benchmarking.Profile.Builtin.Miniature           (profilesNoEraMiniature)
import           Cardano.Benchmarking.Profile.Builtin.Model               (profilesNoEraModel)
import           Cardano.Benchmarking.Profile.Builtin.Plutuscall          (profilesNoEraPlutuscall)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Chainsync  (profilesNoEraChainsync)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Idle       (profilesNoEraIdle)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.TracerOnly (profilesNoEraTracerOnly)
import           Cardano.Benchmarking.Profile.Extra.Scaling               (profilesNoEraScalingLocal, profilesNoEraScalingCloud)
import           Cardano.Benchmarking.Profile.Extra.Voting                (profilesNoEraVoting)
-- Dynamic profiles.
import           Cardano.Benchmarking.Profile.Playground                  (profilesNoEraPlayground)

--------------------------------------------------------------------------------

allProfiles :: [Types.Profile]
allProfiles = performanceAndTracingProfiles ++ playgroundProfiles

cloudProfiles :: [Types.Profile]
cloudProfiles = profilesNoEraCloud

-- "families" of profiles. Grouped by common properties or intentions.
performanceAndTracingProfiles :: [Types.Profile]
performanceAndTracingProfiles =
     cloudProfiles
  ++ profilesNoEraEmpty            -- Empty datasets running `FixedLoaded`.
  ++ profilesNoEraForgeStress      -- All the "forge-stress*" profiles.
  ++ profilesNoEraK3               -- K3
  -- Legacy.
  ++ profilesNoEraDense
  ++ profilesNoEraDish
  ++ profilesNoEraMiniature
  ++ profilesNoEraModel            --
  ++ profilesNoEraPlutuscall       --
  -- Empty datasets not running `FixedLoaded`.
  ++ profilesNoEraChainsync        -- Scenario `Chainsync`
  ++ profilesNoEraIdle             -- Scenario `Idle`
  ++ profilesNoEraTracerOnly       -- Scenario `TracerOnly`
  -- Extra modules
  ++ profilesNoEraScalingLocal
  ++ profilesNoEraScalingCloud
  ++ profilesNoEraVoting

-- Have fun!
playgroundProfiles :: [Types.Profile]
playgroundProfiles = profilesNoEraPlayground

--------------------------------------------------------------------------------

data Cli =
    NamesNoEra
  | NamesCloudNoEra
  | Names
  | All
  | ByName PrettyPrint String
  | LibMK
  | NodeSpecs FilePath FilePath
  | EpochTimeline Integer
  | ToJson String
  | FromJson String

data PrettyPrint =
    PrettyPrint
  | SingleLine

--------------------------------------------------------------------------------

-- | Construct Map with profile name as key, without eras (in name and object).
toMap :: HasCallStack => Maybe Aeson.Object -> [Types.Profile] -> Map.Map String Types.Profile
toMap maybeObj ps = Map.fromList $ map
  (\p ->
    ( Types.name p
    , Profile.realize $
        case maybeObj of
          Nothing    ->  p
          (Just obj) -> (p {Types.overlay = obj})
    )
  )
  ps

-- All profiles are created/defined in the Conway era.
addEras :: Map.Map String Types.Profile -> Map.Map String Types.Profile
addEras = foldMap
  (\profile -> Map.fromList $
    let
        -- TODO: Profiles properties other than the "name" and "era" of
        --       type string are the only thing that change ??? Remove the
        --       concept of eras from the profile definitions and make it a
        --       workbench-level feature (???).
        addEra p era suffix =
          let name = Types.name p
              newName = name ++ "-" ++ suffix
          in  (newName, p {Types.name = newName, Types.era = era})
    in 
        [ addEra profile Types.Allegra "alra"
        , addEra profile Types.Shelley "shey"
        , addEra profile Types.Mary    "mary"
        , addEra profile Types.Alonzo  "alzo"
        , addEra profile Types.Babbage "bage"
        , addEra profile Types.Conway  "coay"
        ]
  )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cli <- getOpts
  case cli of
    -- Print all profile names without the era suffix (does not apply overlays).
    NamesNoEra -> BSL8.putStrLn $ Aeson.encode $ map Types.name allProfiles
    -- Print all cloud profile (-nomadperf) names (does not apply overlays).
    NamesCloudNoEra -> BSL8.putStrLn $ Aeson.encode $ map Types.name cloudProfiles
    -- Print all profile names (applies overlays!!!!!).
    Names -> do
      maybeObj <- lookupOverlay -- Ignored by `NamesNoEra` and `NamesCloudNoEra`.
      BSL8.putStrLn $ Aeson.encode $ Map.keys $ addEras $ toMap maybeObj allProfiles
    -- Print a map with all profiles, with an optional overlay.
    All -> do
      maybeObj <- lookupOverlay -- Ignored by `NamesNoEra` and `NamesCloudNoEra`.
      BSL8.putStrLn $ Aeson.encode $ addEras $ toMap maybeObj allProfiles
    -- Print a single profiles, with an optional overlay.
    (ByName prettyPrint profileName) -> do
      maybeObj <- lookupOverlay -- Ignored by `NamesNoEra` and `NamesCloudNoEra`.
      let profiles = addEras $ toMap maybeObj allProfiles
      case Map.lookup profileName profiles of
        Nothing -> error $ "No profile named \"" ++ profileName ++ "\""
        (Just profile) ->
          let
            prettyConf  = defConfig { confCompare = compare, confTrailingNewline = True }
            aeson       = profile & case prettyPrint of
              PrettyPrint -> Aeson.encodePretty' prettyConf
              SingleLine  -> Aeson.encode
          in BSL8.putStrLn aeson
    LibMK -> do
      mapM_ putStrLn libMk
    (NodeSpecs profilePath topologyPath) -> do
      eitherProfile <- Aeson.eitherDecodeFileStrict profilePath
      let profile = case eitherProfile of
                      (Left errorMsg) ->
                        error $ "Not a valid profile: " ++ errorMsg
                      (Right value) -> value
      eitherTopology <- Aeson.eitherDecodeFileStrict topologyPath
      let topology = case eitherTopology of
                      (Left errorMsg) ->
                        error $ "Not a valid topology: " ++ errorMsg
                      (Right value) -> value
      BSL8.putStrLn $ Aeson.encode $ NodeSpecs.nodeSpecs profile topology
    (EpochTimeline upToEpochNumber) -> do
      genesisAeson <- Profile.epochTimeline upToEpochNumber
      let prettyConf  = defConfig { confCompare = compare, confTrailingNewline = True }
      BSL8.putStrLn $ Aeson.encodePretty' prettyConf genesisAeson
    -- Print a single profiles, with an optional overlay.
    (ToJson filePath) -> print filePath
--      str <- readFile filePath
--      case (readEither str) of
--        (Left errorMsg) -> fail errorMsg
--        (Right profile) -> putStrLn $ show (profile :: Profile.Profile)
    (FromJson filePath) -> do
      eitherProfile <- Aeson.eitherDecodeFileStrict filePath
      case eitherProfile of
        (Left errorMsg) -> fail errorMsg
        (Right profile) -> print (profile :: Types.Profile)

lookupOverlay :: IO (Maybe Aeson.Object)
lookupOverlay = do
  maybeOverlay <- lookupEnv "WB_PROFILE_OVERLAY"
  return $ case maybeOverlay of
    Nothing -> Nothing
    (Just str) -> case Aeson.decode (BSL8.pack str) of
                    (Just (Aeson.Object keyMap)) -> Just keyMap
                    _ -> error ""

getOpts :: IO Cli
getOpts = OA.execParser $
  OA.info
    (cliParser OA.<**> OA.helper)
    (OA.fullDesc <> OA.progDesc "Cardano benchmarking profile generator (-h for help)")

--------------------------------------------------------------------------------

cliParser :: OA.Parser Cli
cliParser = OA.hsubparser $
      OA.command "names-noera"
        (OA.info
          (pure NamesNoEra)
          (OA.fullDesc <> OA.header "names-noera" <> OA.progDesc "All profiles names (no era suffix)")
        )
  <>
      OA.command "names-cloud-noera"
        (OA.info
          (pure NamesCloudNoEra)
          (OA.fullDesc <> OA.header "names-cloud-noera" <> OA.progDesc "All cloud profiles names (no era suffix)")
        )
  <>
      OA.command "names"
        (OA.info
          (pure Names)
          (OA.fullDesc <> OA.header "names" <> OA.progDesc "All profiles names")
        )
  <>
      OA.command "all"
        (OA.info
          (pure All)
          (OA.fullDesc <> OA.header "all" <> OA.progDesc "Create all profiles")
        )
  <>
      OA.command "by-name"
        (OA.info
          (ByName SingleLine <$> OA.argument OA.str (OA.metavar "PROFILE-NAME"))
          (OA.fullDesc <> OA.header "by-name" <> OA.progDesc "Create profile")
        )
  <>
      OA.command "by-name-pretty"
        (OA.info
          (ByName PrettyPrint <$> OA.argument OA.str (OA.metavar "PROFILE-NAME"))
          (OA.fullDesc <> OA.header "by-name-pretty" <> OA.progDesc "Create profile (pretty-printed)")
        )
  <>
      OA.command "lib-make"
        (OA.info
          (pure LibMK)
          (OA.fullDesc <> OA.header "lib-make" <> OA.progDesc "Makefile include")
        )
  <>
      OA.command "node-specs"
        (OA.info
          (     NodeSpecs
            <$> OA.argument OA.str (OA.metavar "PROFILE-JSON-FILEPATH" )
            <*> OA.argument OA.str (OA.metavar "TOPOLOGY-JSON-FILEPATH")
          )
          (OA.fullDesc <> OA.header "node-specs" <> OA.progDesc "Create the profile's node-specs.json file")
        )
  <>
      OA.command "epoch-timeline"
        (OA.info
          (EpochTimeline <$> OA.argument OA.auto (OA.metavar "EPOCH-NUMBER"))
          (OA.fullDesc <> OA.header "epoch-timeline" <> OA.progDesc "Construct the genesis object")
        )
  <>
      OA.command "to-json"
        (OA.info
          (ToJson <$> OA.argument OA.str (OA.metavar "FILE"))
          (OA.fullDesc <> OA.header "to-json" <> OA.progDesc "Data type to JSON")
        )
  <>
      OA.command "from-json"
        (OA.info
          (FromJson <$> OA.argument OA.str (OA.metavar "FILE"))
          (OA.fullDesc <> OA.header "from-json" <> OA.progDesc "JSON to data type")
        )

-- Makefile utils.
--------------------------------------------------------------------------------

libMKLocations :: [ (String, [ (String, [Types.Profile]) ] ) ]
libMKLocations =
  [
    -- Local profiles.
    ("LOCAL_PROFILES", [
      -- Families of local profiles.
      ("PROFILES_EMPTY"        , profilesNoEraEmpty)
    , ("PROFILES_MINIATURE"    , profilesNoEraMiniature)
    , ("PROFILES_FORGE_STRESS" , profilesNoEraForgeStress)
    , ("PROFILES_PLUTUSCALL"   , profilesNoEraPlutuscall)
    , ("PROFILES_MODEL"        , profilesNoEraModel)
    , ("PROFILES_K3"           , profilesNoEraK3)
    , ("PROFILES_SCENARIOS"    ,
         profilesNoEraChainsync
      ++ profilesNoEraIdle
      ++ profilesNoEraTracerOnly
      )
    , ("PROFILES_LEGACY"       ,
         profilesNoEraDense
      ++ profilesNoEraDish
      )
    , ("PROFILES_SCALING"      , profilesNoEraScalingLocal)
    ])
  -- Cloud profiles.
  , ("CLOUD_PROFILES", [
      -- Families of cloud profiles.
      ("PROFILES_NOMAD_PERF"   , profilesNoEraCloud)
    , ("PROFILES_NOMAD_PERFSSD", profilesNoEraScalingCloud)
    ])
  ]

libMk :: [String]
libMk =
    foldMap
      (\(_, families) ->
        map
          (\(familyName, ps) ->
            let profileNames = map Types.name ps
            in
              -- For example:
              -- PROFILES_EMPTY := fast fast-solo ...
              -- PROFILES_MINIATURE := ci-bench ...
              familyName ++ " := " ++ unwords profileNames
          )
          families
      )
      libMKLocations
  ++
    [""] -- Empty line.
  ++
    foldMap
      (\(locationName, families) ->
        map
          (\(familyName, _) ->
            -- LOCAL_PROFILES += $(PROFILES_VENDOR)
            -- ...
            -- CLOUD_PROFILES += $(PROFILES_NOMAD_PERF)
            -- ...
            locationName ++ " += $(" ++ familyName ++ ")"
          )
          families
      )
      libMKLocations

--------------------------------------------------------------------------------

--- * To JSON profile
---
--writeProfile :: [Topo.Node] -> [Topo.Node] -> FilePath -> IO ()
--writeProfile cores relays f = Aeson.encodeFile f (Topo.Topology cores relays)
