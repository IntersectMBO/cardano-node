{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Options.Applicative as Opt
import           System.FilePath ((</>))

import           Cardano.CLI.Types (SigningKeyFile (..))

import           Cardano.Benchmarking.NixOptions
import           Cardano.TxGenerator.Types


data CommandLine = CommandLine {
      runPath           :: FilePath
    , nixServiceJson    :: FilePath
    }
    deriving Show


main :: IO ()
main
  = do
    CommandLine{..} <- parseCommandLine
    nixService <- adjustFilePath (runPath </>) <$> parseNixServiceOptions nixServiceJson
    print nixService

    let txParams = txGenTxParams nixService
    print txParams

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
