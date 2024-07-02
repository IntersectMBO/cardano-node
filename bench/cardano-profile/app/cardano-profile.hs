{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------

import           Prelude
--import           Text.Read (readEither)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Options.Applicative as OA

import qualified Cardano.Benchmarking.Profile as Profile
import qualified Cardano.Benchmarking.Profile.Map as Map

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cli <- OA.execParser
    (OA.info
      cliParser
      (   OA.fullDesc
       <> OA.progDesc "Cardano benchmarking profile generator"
       <> OA.header "all | make PROFILE_NAME | from-json FILE.json | to-json FILE.hs"
      )
    )
  case cli of
    Names -> BSL8.putStrLn $ Aeson.encode $ Map.names
    All -> BSL8.putStrLn $ Aeson.encode $ Map.profiles
    (Make profileName) -> BSL8.putStrLn $ Aeson.encode $ Map.byName profileName
    (ToJson filePath) -> print filePath
--      str <- readFile filePath
--      case (readEither str) of
--        (Left errorMsg) -> fail errorMsg
--        (Right profile) -> putStrLn $ show (profile :: Profile.Profile)
    (FromJson filePath) -> do
      eitherProfile <- Aeson.eitherDecodeFileStrict filePath
      case eitherProfile of
        (Left errorMsg) -> fail errorMsg
        (Right profile) -> putStrLn $ show (profile :: Profile.Profile)
  -- writeProfile cores relays jsonPath

--------------------------------------------------------------------------------

data Cli = Names | All | Make String | ToJson String | FromJson String

cliParser :: OA.Parser Cli
cliParser = OA.hsubparser $
  (OA.command "names"
    (OA.info
      (pure Names)
      (OA.fullDesc <> OA.header "names" <> OA.progDesc "All profiles names")
    )
  )
  <>
  (OA.command "all"
    (OA.info
      (pure All)
      (OA.fullDesc <> OA.header "all" <> OA.progDesc "All profiles")
    )
  )
  <>
  (OA.command "make"
    (OA.info
      (Make <$> OA.argument OA.str (OA.metavar "PROFILE-NAME"))
      (OA.fullDesc <> OA.header "make" <> OA.progDesc "Create profile")
    )
  )
  <>
  (OA.command "to-json"
    (OA.info
      (ToJson <$> OA.argument OA.str (OA.metavar "FILE"))
      (OA.fullDesc <> OA.header "to-json" <> OA.progDesc "Data type to JSON")
    )
  )
  <>
  (OA.command "from-json"
    (OA.info
      (FromJson <$> OA.argument OA.str (OA.metavar "FILE"))
      (OA.fullDesc <> OA.header "from-json" <> OA.progDesc "JSON to data type")
    )
  )

--------------------------------------------------------------------------------

--- * To JSON profile
---
--writeProfile :: [Topo.Node] -> [Topo.Node] -> FilePath -> IO ()
--writeProfile cores relays f = Aeson.encodeFile f (Topo.Topology cores relays)
