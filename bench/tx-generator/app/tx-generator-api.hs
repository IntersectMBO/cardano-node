{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import           Options.Applicative as Opt
import           System.Exit (die)
import           System.FilePath ((</>))

import           Cardano.CLI.Types (SigningKeyFile (..))

import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Types

import           Cardano.Benchmarking.Script.Aeson (prettyPrint, prettyPrintYaml)
import           Cardano.Benchmarking.Script.Selftest (testScript)
import           Cardano.Benchmarking.Script.Types (SubmitMode(..))


data CommandLine = CommandLine {
      runPath           :: FilePath
    , nixServiceJson    :: FilePath
    }
    deriving Show


main :: IO ()
main
  = do
    CommandLine{..} <- parseCommandLine
    nixService <- adjustFilePath (runPath </>) <$> decodeFileStrict' nixServiceJson
    print nixService

    let txParams = txGenTxParams nixService
    print txParams

    let script = testScript "/dev/zero" DiscardTX
    putStrLn "--- JSON serialisation ----------------"
    BSL.putStrLn $ prettyPrint script
    putStrLn "--- YAML serialisation ----------------"
    BSL.putStrLn $ prettyPrintYaml script


decodeFileStrict' :: FromJSON a => FilePath -> IO a
decodeFileStrict' f
  = eitherDecodeFileStrict' f >>= either die pure

adjustFilePath :: (FilePath -> FilePath) -> NixServiceOptions -> NixServiceOptions
adjustFilePath f opts
  = opts {
      _nix_nodeConfigFile = f <$> _nix_nodeConfigFile opts
    , _nix_sigKey = SigningKeyFile . f . unSigningKeyFile $ _nix_sigKey opts
    }

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


---- mapping of Nix service options to API types

txGenTxParams :: NixServiceOptions -> TxGenTxParams
txGenTxParams NixServiceOptions{..}
  = TxGenTxParams {
    txParamFee = _nix_tx_fee
  , txParamAddTxSize = _nix_add_tx_size
  , txParamInputs = _nix_inputs_per_tx
  , txParamOutputs = _nix_outputs_per_tx
  , txParamTTL = txParamTTL defaultTxGenTxParams
  }
