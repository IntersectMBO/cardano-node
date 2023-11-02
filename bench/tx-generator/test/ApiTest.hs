{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- HLINT ignore "Use map" -}

module Main (main) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson (FromJSON, eitherDecodeFileStrict', encode)
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import           Data.List (sortOn)
import           Data.Ord (comparing)
import           GHC.Natural (Natural)
import           Options.Applicative as Opt
import           Options.Applicative.Common as Opt (runParserInfo)

import           System.Environment (getArgs)
import           System.Exit (die, exitSuccess)
import           System.FilePath

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters (..), fromPlutusData)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types (AdjustFilePaths (..), GenesisFile (..))

import           Cardano.TxGenerator.Genesis
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Setup.Plutus
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types

import           Cardano.Benchmarking.Script.Aeson (prettyPrint, prettyPrintOrdered,
                   prettyPrintYaml)
import           Cardano.Benchmarking.Script.Selftest (testScript)
import           Cardano.Benchmarking.Script.Types (SubmitMode (..))

#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts
import           Cardano.Benchmarking.PlutusScripts.CustomCallTypes
#endif

import           Cardano.Node.Protocol.Types

import qualified PlutusTx

import           Paths_tx_generator


data CommandLine = CommandLine {
      runPath           :: FilePath
    , nixServiceJson    :: FilePath
    , protoParamPath    :: FilePath
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
        putStrLn $ "* Did I manage to extract a genesis fund?\n--> " ++ checkFund nixService genesis sigKey
        putStrLn "* Can I pre-execute a plutus script?"
        let plutus = _nix_plutus nixService
        case plutusType <$> plutus of
          Just LimitSaturationLoop  -> checkPlutusLoop protoParamPath plutus
          Just BenchCustomCall      -> checkPlutusBuiltin protoParamPath
          _                         -> putStrLn $ "plutusType "
                                                   ++ show plutus
                                                   ++ " unrecognised"
        exitSuccess

-- The type annotations within patterns or expressions that would be
-- the alternatives would make lines exceed 80 columns, so these
-- helper functions move them out-of-line, with an extra helper to
-- avoid repeating the failure message.
showFundCore :: IsShelleyBasedEra era => Maybe (AddressInEra era, Lovelace) -> String
showFundCore = maybe "fund check failed" show

showBabbage :: Maybe (AddressInEra BabbageEra, Lovelace) -> String
showBabbage = ("Babbage: " ++) . showFundCore

showConway :: Maybe (AddressInEra ConwayEra, Lovelace) -> String
showConway = ("Conway: " ++) . showFundCore

checkFund ::
     NixServiceOptions
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> String
checkFund nixService shelleyGenesis signingKey
  | AnyCardanoEra BabbageEra <- _nix_era nixService
  = showBabbage $ checkFundCore shelleyGenesis signingKey
  | AnyCardanoEra ConwayEra <- _nix_era nixService
  = showConway $ checkFundCore shelleyGenesis signingKey
  | otherwise
  = "ApiTest: unrecognized era"

checkFundCore ::
  IsShelleyBasedEra era
  => ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, Lovelace)
checkFundCore = genesisInitialFundForKey Mainnet

checkPlutusBuiltin :: FilePath -> IO ()
#ifndef WITH_LIBRARY
checkPlutusBuiltin _protoParamFile
  = putStrLn "* checkPlutusBuiltin: skipped - no library available"
#else
checkPlutusBuiltin protoParamFile
  = do
    let script = case findPlutusScript "CustomCall.hs" of
                    Just x -> x
                    Nothing -> error "Error: CustomCall.hs not found"

    putStrLn "* serialisation of built-in Plutus script:"
    BSL.putStrLn $ encodePlutusScript script

    putStrLn "* reading protocol parameters"
    protocolParameters <- readProtocolParametersOrDie protoParamFile
    putStrLn "* done reading protocol parameters"
    forM_ bArgs $ \bArg -> do
      let apiData = unsafeHashableScriptData $ toApiData bArg
      putStrLn $ "* executing with mode: " ++ show (fst bArg)
      putStrLn "* custom script data in Cardano API format:"
      BSL.putStrLn $ encode $ scriptDataToJson ScriptDataJsonDetailedSchema apiData
      case preExecutePlutusScript protocolParameters script (getScriptData apiData) apiData of
        Left err -> putStrLn $ "--> execution failed: " ++ show err
        Right units -> putStrLn $ "--> execution successful; got budget: " ++ show units
  where
    bData :: [CustomCallData]
    bData = [CCNone, CCInteger 42, CCConcat "test123ABC" ["test", "123", "ABC"]]
    -- bData = replicate 300 CCNone

    bArgs :: [CustomCallArg]
    bArgs = zip [EvalSpine, EvalValues, EvalAndValidate] (repeat bData)

    toApiData :: CustomCallArg -> ScriptData
    toApiData = fromPlutusData . PlutusTx.toData
#endif

checkPlutusLoop ::
     FilePath
  -> Maybe TxGenPlutusParams
  -> IO ()
checkPlutusLoop protoParamFile (Just PlutusOn{..})
  = do
    script <- either (die . show) pure =<< readPlutusScript plutusScript
    putStrLn $ "--> Read plutus script: " ++ (id ||| id) plutusScript
    protocolParameters <- readProtocolParametersOrDie protoParamFile

    let count = 1_792        -- arbitrary counter for a loop script; should respect mainnet limits

    redeemerFile <- getRedeemerFile
    redeemer <- readScriptData redeemerFile >>= \case
      Left err -> die (show err)
      Right redeemer -> do
        putStrLn $ "--> read redeemer: " ++ redeemerFile
        return $ scriptDataModifyNumber (+ count) $ getScriptData redeemer

    case preExecutePlutusScript protocolParameters script (ScriptDataNumber 0) (unsafeHashableScriptData redeemer) of
      Left err -> putStrLn $ "--> execution failed: " ++ show err
      Right units -> putStrLn $ "--> execution successful; got budget: " ++ show units

    putStrLn "* What does the redeemer look like when the loop counter is maxed out?"
    let budget = case protocolParamMaxTxExUnits protocolParameters of
                    Just x -> x
                    Nothing -> error "Cannot find protocolParamMaxTxExUnits"
        autoBudget = PlutusAutoBudget
          { autoBudgetUnits = budget
          , autoBudgetDatum = ScriptDataNumber 0
          , autoBudgetRedeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) redeemer
          }

        pparamsStepFraction d = case protocolParamMaxBlockExUnits protocolParameters of
          Just u  -> protocolParameters {protocolParamMaxBlockExUnits = Just u
              { executionSteps = executionSteps u `mul` d
              }
            }
          Nothing -> protocolParameters
    putStrLn $ "--> " ++ show (plutusAutoBudgetMaxOut protocolParameters script autoBudget TargetTxExpenditure 1)

    let
      blockMaxOut b d =
        case plutusAutoScaleBlockfit (pparamsStepFraction d) ("factor for block execution steps: " ++ show d) script b (TargetTxsPerBlock 8) 1 of
          Right (summary, _, _) -> BSL.putStrLn $ prettyPrintOrdered summary
          Left err              -> print err

    putStrLn "--> summaries for block budget fits:"
    mapM_ (blockMaxOut autoBudget) [1.0, 0.5, 2.0]

  where
    mul :: Natural -> Double -> Natural
    mul n d = floor $ d * fromIntegral n

    getRedeemerFile
      = case plutusScript of
          Right file -> let redeemerPath = (<.> ".redeemer.json") $ dropExtension $ takeFileName file
                        in getDataFileName $ "data" </> redeemerPath
          Left _ -> getDataFileName "data/loop.redeemer.json"
checkPlutusLoop _ _
  = putStrLn "--> No plutus script defined."


--
-- helpers
--

readFileJson :: FromJSON a => FilePath -> ExceptT TxGenError IO a
readFileJson f = handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' f) >>= firstExceptT TxGenError . hoistEither

readProtocolParametersOrDie :: FilePath -> IO ProtocolParameters
readProtocolParametersOrDie filePath
  = do
    parametersFile <- getDataFileName filePath
    either die pure =<< eitherDecodeFileStrict' parametersFile


--
-- command line parsing
--

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
      <*> parseParamPath
  where
    parseParamPath = strOption
        ( long "param"
            <> metavar "PARAM"
            <> help "Path to protocol parameter file"
            <> completer (bashCompleter "file")
            <> value "data/protocol-parameters-v8.json"
        )
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
