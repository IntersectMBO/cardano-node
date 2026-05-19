{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (module Main) where

import           Cardano.Api hiding (ShelleyGenesis)
import qualified Cardano.Api.Ledger as Api
import           Cardano.Api (fromPlutusData, sgNetworkMagic)

#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts
import           Cardano.Benchmarking.PlutusScripts.CustomCallTypes
#endif
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types (AdjustFilePaths (..), GenesisFile (..))
import           Cardano.TxGenerator.Calibrate.Utils
import           Cardano.TxGenerator.Genesis
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters(..))
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Setup.Plutus
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types

import           Control.Monad
import           Data.Aeson (eitherDecodeFileStrict')
import qualified Data.ByteString as BS (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack, putStrLn, writeFile)
import           Data.Either (rights)
import           Data.Either.Extra (fromEither)
import           Data.Maybe (fromMaybe)
import           Numeric.Natural (Natural)
import           Options.Applicative as Opt
import           System.Environment (getArgs)
import           System.Exit (die, exitSuccess)
import           System.FilePath

import qualified PlutusTx (toData)


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
    let
      pathModifier p = if isRelative p then runPath </> p else p
      protoParamPath' = if null protoParamPath then Nothing else Just protoParamPath

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
          Just BenchCustomCall      -> checkPlutusBuiltin protoParamPath'
          Just{}                    -> checkPlutusLoop protoParamPath' plutus
          Nothing                   -> putStrLn "--> no Plutus configuration found - skipping"
        exitSuccess

-- The type annotations within patterns or expressions that would be
-- the alternatives would make lines exceed 80 columns, so these
-- helper functions move them out-of-line, with an extra helper to
-- avoid repeating the failure message.
showFundCore :: IsShelleyBasedEra era => Maybe (AddressInEra era, Api.Coin) -> String
showFundCore = maybe "no fund found for given key in genesis" show

showBabbage :: Maybe (AddressInEra BabbageEra, Api.Coin) -> String
showBabbage = ("Babbage: " ++) . showFundCore

showConway :: Maybe (AddressInEra ConwayEra, Api.Coin) -> String
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
  = "ApiTest.checkFund: unrecognized era"

checkFundCore ::
  IsShelleyBasedEra era
  => ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, Api.Coin)
checkFundCore sg = genesisInitialFundForKey networkId sg
  where
    networkId = fromNetworkMagic $ NetworkMagic $ sgNetworkMagic sg

checkPlutusBuiltin :: Maybe FilePath -> IO ()
#ifndef WITH_LIBRARY
checkPlutusBuiltin _protoParamFile
  = putStrLn "* checkPlutusBuiltin: skipped - no library available"
#else
checkPlutusBuiltin protoParamFile
  = do
    let script = fromMaybe (error "checkPlutusBuiltin: script CustomCall not found") (findPlutusScript "CustomCall")

    putStrLn "* serialisation of built-in Plutus script:"
    BSL.putStrLn $ encodePlutusScript script

    putStrLn "* reading protocol parameters"
    protocolParameters <- readProtocolParametersOrDie protoParamFile
    putStrLn "* done reading protocol parameters"
    forM_ bArgs $ \bArg -> do
      let
        apiData :: ScriptData
        apiData = toApiData bArg
      putStrLn $ "* executing with mode: " ++ show (fst bArg)
      putStrLn "* custom script data in Cardano API format:"
      printScriptData apiData
      case preExecutePlutusScript protocolParameters script apiData (unsafeHashableScriptData apiData) of
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
     Maybe FilePath
  -> Maybe TxGenPlutusParams
  -> IO ()
checkPlutusLoop protoParamFile (Just _plutusDef@PlutusOn{..})
  = do
    (script, resolvedTo) <- either (die . show) pure =<< readPlutusScript plutusScript
    putStrLn $ "--> Got script " ++ show resolvedTo
    protocolParameters <- readProtocolParametersOrDie protoParamFile

    let count = 1_792       -- arbitrary counter for a loop script; should respect mainnet limits

    let redeemerDef = Right _plutusDef
    -- let redeemerDef = Left hashAndAddG2_redeemer

    redeemer :: ScriptData <-
      resolveRedeemer redeemerDef >>= either
        (die . show)
        (pure . scriptDataModifyNumber (+ count) . getScriptData)
    printScriptData redeemer

    case preExecutePlutusScript protocolParameters script (ScriptDataNumber 0) (unsafeHashableScriptData redeemer) of
      Left err -> putStrLn $ "--> execution failed: " ++ show err
      Right units -> putStrLn $ "--> execution successful; got budget: " ++ show units

    let autoBudget = PlutusAutoBudget
          { autoBudgetUnits = fromMaybe (error "autoBudget: cannot find protocolParamMaxTxExUnits") (protocolParamMaxTxExUnits protocolParameters)
          , autoBudgetDatum = ScriptDataNumber 0
          , autoBudgetRedeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) redeemer
          , autoBudgetUpperBoundHint = Nothing
          }

        pparamsScaleBlockBudget factor = case protocolParamMaxBlockExUnits protocolParameters of
          Just u  -> protocolParameters {protocolParamMaxBlockExUnits = Just u
              { executionSteps  = executionSteps u  `mul` factor
              , executionMemory = executionMemory u `mul` factor
              }
            }
          Nothing -> protocolParameters

    -- putStrLn "* What does the redeemer look like when the loop counter is maxed out?"
    -- putStrLn $ "--> " ++ show (plutusAutoBudgetMaxOut protocolParameters script autoBudget TargetTxExpenditure 1)

    let
      blockMaxOut b d =
        case plutusAutoScaleBlockfit (pparamsScaleBlockBudget d) (scriptName, idPath) script b (TargetTxsPerBlock 8) 1 of
          Right (summary, _, _) -> Right summary
          Left err              -> Left $ BSL.pack $ show err
        where
          idPath = "blockbudget" ++ if d == 1.0 then "" else "/steps_x" ++ show d

      txMaxOut b =
        case plutusAutoScaleBlockfit protocolParameters (scriptName, "txbudget") script b TargetTxExpenditure 1 of
          Right (summary, _, _) -> Right summary
          Left err              -> Left $ BSL.pack $ show err

    putStrLn "--> summary for tx budget fit:"
    let txBudg = txMaxOut autoBudget
    BSL.putStrLn $ either id encodePrettySorted txBudg

    putStrLn "--> summaries for block budget fits:"
    blockBudgs <- forM [1.0, 0.5, 2.0] $ \factor -> do
      let blockBudg = blockMaxOut autoBudget factor
      BSL.putStrLn $ either id encodePrettySorted blockBudg
      pure blockBudg

    let
      summaries :: [PlutusBudgetSummary]
      summaries = rights (txBudg : blockBudgs)
      summaryName = "summaries_" ++ scriptName <.> "json"
    putStrLn $ "--> writing summaries to " ++ summaryName
    BSL.writeFile summaryName (encodePrettySorted summaries)

  where
    mul :: Natural -> Double -> Natural
    mul n d = floor $ d * fromIntegral n

    scriptName = fromEither plutusScript

checkPlutusLoop _ _
  = putStrLn "--> No plutus script defined."


--
-- helpers
--

readFileJson :: FromJSON a => FilePath -> ExceptT TxGenError IO a
readFileJson f = handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' f) >>= firstExceptT TxGenError . hoistEither

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
            <> value "protocol-parameters-v8.json"
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

--
-- test data
--

hashAndAddG2_redeemer :: ScriptData
hashAndAddG2_redeemer =
  ScriptDataConstructor 0
    [ ScriptDataNumber 1_000_000
    , ScriptDataList
      [ mkBytes [ 113,  72 ,   5, 198 ]
      , mkBytes [ 196,  19 ,  17,  30 ]
      -- , mkBytes [  45,  126, 184, 112 ]
      -- , mkBytes [  78,  203, 214, 161 ]
      ]
    ]
  where
    mkBytes = ScriptDataBytes . BS.pack
