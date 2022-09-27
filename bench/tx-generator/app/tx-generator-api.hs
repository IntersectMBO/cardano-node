{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import           Options.Applicative as Opt
import           System.Exit (die)
import           System.FilePath (isRelative, (</>))

import           Cardano.Node.Types (AdjustFilePaths (..))

import           Cardano.TxGenerator.Setup.NixService

import           Cardano.Benchmarking.Script.Aeson (prettyPrint, prettyPrintYaml)
import           Cardano.Benchmarking.Script.Selftest (testScript)
import           Cardano.Benchmarking.Script.Types (SubmitMode (..))


data CommandLine = CommandLine {
      runPath           :: FilePath
    , nixServiceJson    :: FilePath
    }
    deriving Show


main :: IO ()
main
  = do
    CommandLine{..} <- parseCommandLine
    let pathModifier p = if isRelative p then runPath </> p else p

    nixService <- adjustFilePaths pathModifier <$> decodeFileStrict' nixServiceJson
    print nixService

    let (a, b, c) = (txGenTxParams nixService, txGenConfig nixService, txGenPlutusParams nixService)
    print a
    print b
    print c

    let script = testScript "/dev/zero" DiscardTX
    putStrLn "--- JSON serialisation ----------------"
    BSL.putStrLn $ prettyPrint script
    putStrLn "--- YAML serialisation ----------------"
    BSL.putStrLn $ prettyPrintYaml script


decodeFileStrict' :: FromJSON a => FilePath -> IO a
decodeFileStrict' f
  = eitherDecodeFileStrict' f >>= either die pure



parseCommandLine :: IO CommandLine
parseCommandLine
  = Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parserCommandLine mempty

parserCommandLine :: Parser CommandLine
parserCommandLine
  = CommandLine
      <$> parseRunPath
      <*> parseJsonLocation
  where
    parseRunPath = strOption
        ( long "run"
            <> metavar "PATH"
            <> help "Path to the run containing node config and genesis"
            <> completer (bashCompleter "directory")
        )
    parseJsonLocation = strOption
        ( long "nix-json"
            <> metavar "FILE"
            <> help "The Nix service definition JSON file"
            <> completer (bashCompleter "file")
        )


