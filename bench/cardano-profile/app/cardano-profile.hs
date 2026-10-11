{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------

-- base.
import           Prelude
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
-- Package: optparse-applicative.
import qualified Options.Applicative as OA
-- Package: self.
import qualified Cardano.Benchmarking.Profile as Profile
import qualified Cardano.Benchmarking.Profile.Genesis as Genesis
import qualified Cardano.Benchmarking.Profile.NodeSpecs as NodeSpecs
import qualified Cardano.Benchmarking.Profile.Types as Types
-- Profiles to export!
-- Static / built-in / profiles part of the test-suite.
import           Cardano.Benchmarking.Profile.Builtin.Cloud               (profilesCloud)
import           Cardano.Benchmarking.Profile.Builtin.Empty               (profilesEmpty)
import           Cardano.Benchmarking.Profile.Builtin.ForgeStress         (profilesForgeStress)
import           Cardano.Benchmarking.Profile.Builtin.K3                  (profilesK3)
import           Cardano.Benchmarking.Profile.Builtin.Legacy.Dense        (profilesDense)
import           Cardano.Benchmarking.Profile.Builtin.Legacy.Dish         (profilesDish)
import           Cardano.Benchmarking.Profile.Builtin.Miniature           (profilesMiniature)
import           Cardano.Benchmarking.Profile.Builtin.Model               (profilesModel)
import           Cardano.Benchmarking.Profile.Builtin.Plutuscall          (profilesPlutuscall)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Chainsync  (profilesChainsync)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Idle       (profilesIdle)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.TracerOnly (profilesTracerOnly)
import           Cardano.Benchmarking.Profile.Extra.Scaling               (profilesScalingLocal, profilesScalingCloud)
-- Dynamic profiles.
import           Cardano.Benchmarking.Profile.Playground                  (profilesPlayground)

--------------------------------------------------------------------------------

allProfiles :: [Types.Profile]
allProfiles = performanceAndTracingProfiles ++ playgroundProfiles

cloudProfiles :: [Types.Profile]
cloudProfiles = profilesCloud

-- "families" of profiles. Grouped by common properties or intentions.
performanceAndTracingProfiles :: [Types.Profile]
performanceAndTracingProfiles =
     cloudProfiles
  -- Empty datasets running `FixedLoaded`.
  ++ profilesEmpty
  -- All the "forge-stress*" profiles.
  ++ profilesForgeStress
  -- K3.
  ++ profilesK3
  -- Legacy.
  ++ profilesDense
  ++ profilesDish
  -- Light dataset.
  ++ profilesMiniature
  ++ profilesModel
  ++ profilesPlutuscall
  -- Empty datasets not running `FixedLoaded`.
  ++ profilesChainsync  -- Scenario `Chainsync`
  ++ profilesIdle       -- Scenario `Idle`
  ++ profilesTracerOnly -- Scenario `TracerOnly`
  -- Extra profiles.
  ++ profilesScalingLocal
  ++ profilesScalingCloud

-- Have fun!
playgroundProfiles :: [Types.Profile]
playgroundProfiles = profilesPlayground

--------------------------------------------------------------------------------

data Cli =
    Names
  | NamesCloud
  | All [CliOptions]
  | ByName [CliOptions] String
  | LibMK
  | NodeSpecs FilePath FilePath
  | EpochTimeline Integer
  | ToJson String
  | FromJson String

data CliOptions =
    OptPrettyPrint
  | OptWithPlayground
  deriving Eq

--------------------------------------------------------------------------------

-- | Construct Map of `Profile` with the key being the profile name after
-- applying an optional overlay.
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

--------------------------------------------------------------------------------

encoder :: Aeson.ToJSON a => [CliOptions] -> a -> BSL8.ByteString
encoder opts
  | OptPrettyPrint `elem` opts = Aeson.encodePretty' prettyConf
  | otherwise                  = Aeson.encode
  where
    prettyConf  = defConfig
      { confCompare = compare
      , confTrailingNewline = True
      , confIndent = Spaces 2
      }

main :: IO ()
main = do
  cli <- getOpts
  case cli of
    -- Print all profile names, with an optional overlay.
    Names -> do
      maybeObj <- lookupOverlay -- Ignored by `NamesCloud`.
      BSL8.putStrLn $ Aeson.encode $ Map.keys $ toMap maybeObj allProfiles
    -- Print all cloud (-nomadperf) profile names, does not apply overlays.
    NamesCloud -> BSL8.putStrLn $ Aeson.encode $ map Types.name cloudProfiles
    -- Print a map with all profiles, with an optional overlay.
    All cliOptions -> do
      let
        targetProfiles = if OptWithPlayground `elem` cliOptions
                         then allProfiles
                         else performanceAndTracingProfiles
        enc            = encoder cliOptions
      maybeObj <- lookupOverlay -- Ignored by `NamesCloud`.
      BSL8.putStrLn $ enc $ toMap maybeObj targetProfiles
    -- Print a single profiles, with an optional overlay.
    ByName cliOptions profileName -> do
      maybeObj <- lookupOverlay -- Ignored by `NamesCloud`.
      let
        enc      = encoder cliOptions
        profiles = toMap maybeObj allProfiles
      case Map.lookup profileName profiles of
        Nothing      -> error $ "No profile named \"" ++ profileName ++ "\""
        Just profile -> BSL8.putStrLn $ enc profile
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
      genesisAeson <- Genesis.epochTimeline upToEpochNumber
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
      OA.command "names"
        (OA.info
          (pure Names)
          (OA.fullDesc <> OA.header "names" <> OA.progDesc "All profiles names")
        )
  <>
      OA.command "names-cloud"
        (OA.info
          (pure NamesCloud)
          (OA.fullDesc <> OA.header "names-cloud" <> OA.progDesc "All cloud profiles names")
        )
  <>
      OA.command "all"
        (OA.info
          (pure $ All [OptWithPlayground])
          (OA.fullDesc <> OA.header "all" <> OA.progDesc "Create all profiles")
        )
  <>
      OA.command "allpt"
        (OA.info
          (pure $ All [OptPrettyPrint])
          (OA.fullDesc <> OA.header "allpt" <> OA.progDesc "Create P&T profiles")
        )
  <>
      OA.command "by-name"
        (OA.info
          (ByName [] <$> OA.argument OA.str (OA.metavar "PROFILE-NAME"))
          (OA.fullDesc <> OA.header "by-name" <> OA.progDesc "Create profile")
        )
  <>
      OA.command "by-name-pretty"
        (OA.info
          (ByName [OptPrettyPrint] <$> OA.argument OA.str (OA.metavar "PROFILE-NAME"))
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
      ("PROFILES_EMPTY"        , profilesEmpty)
    , ("PROFILES_MINIATURE"    , profilesMiniature)
    , ("PROFILES_FORGE_STRESS" , profilesForgeStress)
    , ("PROFILES_PLUTUSCALL"   , profilesPlutuscall)
    , ("PROFILES_MODEL"        , profilesModel)
    , ("PROFILES_K3"           , profilesK3)
    , ("PROFILES_SCENARIOS"    ,
         profilesChainsync
      ++ profilesIdle
      ++ profilesTracerOnly
      )
    , ("PROFILES_LEGACY"       ,
         profilesDense
      ++ profilesDish
      )
    , ("PROFILES_SCALING"      , profilesScalingLocal)
    ])
  -- Cloud profiles.
  , ("CLOUD_PROFILES", [
      -- Families of cloud profiles.
      ("PROFILES_NOMAD_PERF"   , profilesCloud)
    , ("PROFILES_NOMAD_PERFSSD", profilesScalingCloud)
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
