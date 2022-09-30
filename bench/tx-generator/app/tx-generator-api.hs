{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main (main) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import           Options.Applicative as Opt
import           System.Exit (die)
import           System.FilePath (isRelative, (</>))

import           Cardano.Api
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types (AdjustFilePaths (..), GenesisFile (..))

import           Cardano.TxGenerator.Genesis
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Types

import           Cardano.Benchmarking.GeneratorTx (readSigningKey)

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

    {-
    let script = testScript "/dev/zero" DiscardTX
    putStrLn "--- JSON serialisation ----------------"
    BSL.putStrLn $ prettyPrint script
    putStrLn "--- YAML serialisation ----------------"
    BSL.putStrLn $ prettyPrintYaml script
    -}

    setup  <- runExceptT $ do
      nixService :: NixServiceOptions <-
        adjustFilePaths pathModifier <$> readFileJson nixServiceJson

      ncFile <- hoistMaybe (TxGenError "nodeConfigFile not specified") $
        getNodeConfigFile nixService
      nc :: NodeConfiguration <-
        hoistEither =<< handleIOExceptT (TxGenError . show) (mkNodeConfig ncFile)

      GenesisFile sgFile <- hoistMaybe (TxGenError "npcShelleyGenesisFile not specified") $
        getGenesisPath nc
      genesis :: ShelleyGenesis <-
        readFileJson sgFile
      _ <- firstExceptT TxGenError $ hoistEither $
        genesisValidate genesis

      sigKey <- readSigningKey $ _nix_sigKey nixService

      pure (nixService, nc, genesis, sigKey)

    case setup of
      Right (_nixService, _nc, _genesis, _sigKey) ->
        print $ checkFund _genesis _sigKey
      err -> die (show err)

checkFund ::
     ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra BabbageEra, Lovelace)
checkFund = genesisInitialFundForKey Mainnet

{-
decodeFileStrict' :: FromJSON a => FilePath -> IO a
decodeFileStrict' f
  = eitherDecodeFileStrict' f >>= either die pure
-}

readFileJson :: FromJSON a => FilePath -> ExceptT TxGenError IO a
readFileJson f = handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' f) >>= firstExceptT TxGenError . hoistEither

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


