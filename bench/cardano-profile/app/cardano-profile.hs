{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------

import           Prelude

import           Data.Function ((&))
import           System.Environment (lookupEnv)
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: aeson-pretty
import           Data.Aeson.Encode.Pretty as Aeson
-- Package: bytestring.
import qualified Data.ByteString.Lazy.Char8 as BSL8
-- Package: optparse-applicative-fork.
import qualified Options.Applicative as OA
-- Package: self.
import qualified Cardano.Benchmarking.Profile as Profiles
import qualified Cardano.Benchmarking.Profile.NodeSpecs as NodeSpecs
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

data Cli =
    Names
  | NamesNoEra
  | NamesCloudNoEra
  | All
  | ByName PrettyPrint String
  | LibMK
  | NodeSpecs FilePath FilePath
  | ToJson String
  | FromJson String

data PrettyPrint =
    PrettyPrint
  | SingleLine

--------------------------------------------------------------------------------

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
    -- Print all profile names (does not apply overlays).
    Names -> BSL8.putStrLn $ Aeson.encode $ addEras $ Profiles.profilesNoEra mempty
    -- Print all profile names without the era suffix (does not apply overlays).
    NamesNoEra -> BSL8.putStrLn $ Map.keys $ Profiles.profilesNoEra mempty
    -- Print all cloud profile (-nomadperf) names.
    NamesCloudNoEra -> BSL8.putStrLn $ Aeson.encode Profiles.namesCloudNoEra
    -- Print a map with all profiles, with an optional overlay.
    All -> do
      obj <- lookupOverlay
      BSL8.putStrLn $ Aeson.encode $ Profiles.addEras $ Profiles.profilesNoEra obj
    -- Print a single profiles, with an optional overlay.
    (ByName prettyPrint profileName) -> do
      obj <- lookupOverlay
      let profiles = Profiles.addEras $ Profiles.profilesNoEra obj
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
      mapM_ putStrLn Profiles.libMk
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

lookupOverlay :: IO Aeson.Object
lookupOverlay = do
  maybeOverlay <- lookupEnv "WB_PROFILE_OVERLAY"
  case maybeOverlay of
    Nothing -> mempty
    (Just str) -> case Aeson.decode (BSL8.pack str) of
                    (Just (Aeson.Object keyMap)) -> return keyMap
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

--------------------------------------------------------------------------------

--- * To JSON profile
---
--writeProfile :: [Topo.Node] -> [Topo.Node] -> FilePath -> IO ()
--writeProfile cores relays f = Aeson.encodeFile f (Topo.Topology cores relays)
