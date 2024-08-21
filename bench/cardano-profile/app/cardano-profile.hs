{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------

import           Prelude
import           System.Environment (lookupEnv)
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: bytestring.
import qualified Data.ByteString.Lazy.Char8 as BSL8
-- Package: optparse-applicative-fork.
import qualified Options.Applicative as OA
-- Package: self.
import qualified Cardano.Benchmarking.Profile as Profiles
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

data Cli = Names | All | ByName String | LibMK | ToJson String | FromJson String

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cli <- getOpts
  case cli of
    -- Print all profile names.
    Names -> BSL8.putStrLn $ Aeson.encode Profiles.names
    -- Print a map with all profiles, with an optional overlay.
    All -> do
      obj <- lookupOverlay
      BSL8.putStrLn $ Aeson.encode $ Profiles.profiles obj
    -- Print a single profiles, with an optional overlay.
    (ByName profileName) -> do
      obj <- lookupOverlay
      case Profiles.byName profileName obj of
        Nothing -> error $ "No profile named \"" ++ profileName ++ "\""
        (Just profile) ->
          let aeson = Aeson.encode profile
          in BSL8.putStrLn aeson
    LibMK -> do
      mapM_ putStrLn Profiles.libMk
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
      OA.command "all"
        (OA.info
          (pure All)
          (OA.fullDesc <> OA.header "all" <> OA.progDesc "Create all profiles")
        )
  <>
      OA.command "by-name"
        (OA.info
          (ByName <$> OA.argument OA.str (OA.metavar "PROFILE-NAME"))
          (OA.fullDesc <> OA.header "by-name" <> OA.progDesc "Create profile")
        )
  <>
      OA.command "lib-make"
        (OA.info
          (pure LibMK)
          (OA.fullDesc <> OA.header "lib-make" <> OA.progDesc "Makefile include")
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
