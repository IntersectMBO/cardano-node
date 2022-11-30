{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main (main) where

import           Control.Monad (when)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import           Options.Applicative as Opt
import           Options.Applicative.Common as Opt (runParserInfo)

import           System.Environment (getArgs)
import           System.Exit (die, exitSuccess)
import           System.FilePath

import           Cardano.Api
import           Cardano.Api.Shelley (protocolParamMaxTxExUnits)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types (AdjustFilePaths (..), GenesisFile (..))

import           Cardano.TxGenerator.Genesis
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Setup.Plutus
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types

import           Cardano.Benchmarking.Script.Aeson (prettyPrint, prettyPrintYaml)
import           Cardano.Benchmarking.Script.Selftest (testScript)
import           Cardano.Benchmarking.Script.Types (SubmitMode (..))

import           Cardano.Node.Protocol.Types

import           Paths_tx_generator


data CommandLine = CommandLine {
      runPath           :: FilePath
    , nixServiceJson    :: FilePath
    }
    deriving Show


main :: IO ()
main
  = do
    args <- getArgs
    when (null args) $ do
        putStrLn "--> no command line arguments provided -- skipping test"
        -- Manually create the helptext, since optparse-applicative emits an exitFailure.
        -- At this stage, we simply want to skip this test if there's lack of test data
        -- such as genesis / node config .json
        let
            msg = case Opt.execParserPure Opt.defaultPrefs infoCommandLine [] of
                Opt.Failure f   -> fst $ Opt.renderFailure f "tx-generator-apitest"
                _               -> ""
        putStrLn msg
        exitSuccess

    CommandLine{..} <- parseCommandLine
    let pathModifier p = if isRelative p then runPath </> p else p

    setup <- runExceptT $ do
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

      sigKey :: SigningKey PaymentKey <-
        hoistEither =<< handleIOExceptT (TxGenError . show) (readSigningKeyFile $ _nix_sigKey nixService)

      pure (nixService, nc, genesis, sigKey)

    case setup of
      Left err -> die (show err)
      Right (nixService, _nc, genesis, sigKey) -> do
        putStrLn $ "* Did I manage to extract a genesis fund?\n--> " ++ show (checkFund genesis sigKey)
        putStrLn "* Can I pre-execute a plutus script?"
        checkPlutusLoop (_nix_plutus nixService)
        exitSuccess

checkFund ::
     ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra BabbageEra, Lovelace)
checkFund = genesisInitialFundForKey Mainnet

checkPlutusLoop ::
     Maybe TxGenPlutusParams
  -> IO ()
checkPlutusLoop (Just PlutusOn{..})
  = do
    parametersFile <- getDataFileName "data/protocol-parameters-v8.json"
    protocolParameters <- either die pure =<< eitherDecodeFileStrict' parametersFile
    script <- either (die . show) pure =<< readPlutusScript plutusScript
    putStrLn $ "--> Read plutus script: " ++ plutusScript

    let count = 1792        -- arbitrary counter for a loop script; should respect mainnet limits

    redeemerFile <- getRedeemerFile
    redeemer <- readScriptData redeemerFile >>= \case
      Left err -> die (show err)
      Right redeemer -> do
        putStrLn $ "--> read redeemer: " ++ redeemerFile
        return $ scriptDataModifyNumber (+ count) redeemer

    case preExecutePlutusScript protocolParameters script (ScriptDataNumber 0) redeemer of
      Left err -> putStrLn $ "--> execution failed: " ++ show err
      Right units -> putStrLn $ "--> execution successful; got units: " ++ show units

    putStrLn "* What does the redeemer look like when the loop counter is maxed out?"
    let
        ~(Just budget) = protocolParamMaxTxExUnits protocolParameters
        autoBudget = PlutusAutoBudget
          { autoBudgetUnits = budget
          , autoBudgetDatum = ScriptDataNumber 0
          , autoBudgetRedeemer = scriptDataModifyNumber (const 1_000_000) redeemer
          }
    putStrLn $ "--> " ++ show (plutusAutoBudgetMaxOut protocolParameters script autoBudget)

  where
    getRedeemerFile =
      let redeemerPath = (<.> ".redeemer.json") $ dropExtension $ takeFileName plutusScript
      in getDataFileName $ "data" </> redeemerPath
checkPlutusLoop _
  = putStrLn "--> No plutus script defined."

readFileJson :: FromJSON a => FilePath -> ExceptT TxGenError IO a
readFileJson f = handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' f) >>= firstExceptT TxGenError . hoistEither

parseCommandLine :: IO CommandLine
parseCommandLine
  = Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parserCommandLine mempty

infoCommandLine :: ParserInfo CommandLine
infoCommandLine
  = Opt.info parserCommandLine Opt.fullDesc

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
