{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.Api
import           Cardano.Api.Internal.ProtocolParameters (ProtocolParameters (..))

#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts
import           Cardano.Benchmarking.PlutusScripts.CustomCallTypes
#endif
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.Setup.Plutus
import           Cardano.TxGenerator.Types

import           Control.Exception (SomeException (..), evaluate, try)
import           Control.Monad
import           Data.Aeson (eitherDecodeFileStrict')
import           Data.Aeson.Encode.Pretty
import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, pack, putStrLn, writeFile)
import           Data.Char
import           Data.Either (rights)
import           Data.Functor ((<&>))
import           Data.List (sort, transpose)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Natural (Natural)
import           Options.Applicative as Opt
import           System.Directory
import           System.FilePath

import           Paths_tx_generator

-- import qualified PlutusTx


data CommandLine
    = CLIList
    | CLICalibrate
      { cName      :: FilePath
      , cStrategy  :: PlutusBudgetFittingStrategy
      , cParams    :: Maybe FilePath
      }
    deriving Show

main :: IO ()
main = parseCommandLine >>= \case
  CLIList           -> runCommandList
  CLICalibrate{..}  -> do
    scriptNames <- knownScriptNames
    let
      p = PlutusOn LimitSaturationLoop s Nothing Nothing Nothing Nothing
      s = if cName `elem` scriptNames then Left cName else Right cName
    checkPlutusLoop cParams p

runCommandList :: IO ()
runCommandList = do
  putStrLn "--> known strategies:"
  putStrLn $ unlines
    [ "txbudget         -- such that the transaction budget is exhausted"
    , "txperblock_<d>   -- such that the block budget is exhausted,"
    , "                      AND there are <d> script transactions per block"
    , "                      (default: 8, as this is already supported in workbench profiles)"
    , "blockbudget_<r>  -- such that the block budget is exhausted (TBD)"
    , ""
    ]
  putStrLn "--> known script names:"
  mapM_ putStrLn . sort =<< knownScriptNames

knownScriptNames :: IO [String]
#ifdef WITH_LIBRARY
knownScriptNames = pure listPlutusScripts
#else
knownScriptNames = do
  assumedToExist <- getDataFileName $ "scripts-fallback" </> "Loop.plutus"
  let fallbackDir = takeDirectory assumedToExist
  contents <- listDirectory fallbackDir
  pure [dropExtension f | f <- contents, takeExtension f == ".plutus"]
#endif

-- TODO: support this for scaling at all?
_checkPlutusBuiltin :: FilePath -> IO ()
#ifndef WITH_LIBRARY
_checkPlutusBuiltin _protoParamFile
  = putStrLn "* checkPlutusBuiltin: skipped - no library available"
#else
_checkPlutusBuiltin protoParamFile
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
  -> TxGenPlutusParams
  -> IO ()
checkPlutusLoop protoParamFile _plutusDef@PlutusOn{..}
  = do
    script <- either (error . show) pure =<< readPlutusScript plutusScript
    putStrLn $ "--> read Plutus script: " ++ scriptNameExt
    protocolParameters <- readProtocolParametersOrDie protoParamFile

    let redeemerDef = Right _plutusDef

    redeemer :: ScriptData <-
      resolveRedeemer redeemerDef >>= either
        (error . show)
        (pure . getScriptData)
    printScriptData redeemer

    let budget = case protocolParamMaxTxExUnits protocolParameters of
                    Just x -> x
                    Nothing -> error "Cannot find protocolParamMaxTxExUnits"
        autoBudget = PlutusAutoBudget
          { autoBudgetUnits = budget
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
        case plutusAutoScaleBlockfit (pparamsScaleBlockBudget d) (scriptName ++ idPath) script b (TargetTxsPerBlock 8) 1 of
          Right (summary, _, _) -> Right summary
          Left err              -> Left $ BSL.pack $ show err
        where
          idPath = "/blockbudget" ++ if d == 1.0 then "" else "/steps_x" ++ show d

      txMaxOut b =
        case plutusAutoScaleBlockfit protocolParameters (scriptName ++ "/txbudget") script b TargetTxExpenditure 1 of
          Right (summary, _, _) -> Right summary
          Left err              -> Left $ BSL.pack $ show err

    putStrLn "--> run -- strategy:txbudget scale(tx):1.0 scale(block):1.0"
    txBudg <- evaluate $ txMaxOut autoBudget

    blockBudgs <- forM [1.0, 1.5, 2.0] $ \factor -> do
      putStrLn $ "--> run -- strategy:txperblock_8 scale(tx):1.0 scale(block):" ++ show factor
      evaluate $ blockMaxOut autoBudget factor

    let
      summaries :: [PlutusBudgetSummary]
      summaries = rights (txBudg : blockBudgs)
      summaryName = "summaries_" ++ scriptName <.> "json"
      csvName     = "scaling_" ++ scriptName <.> "csv"
      newColumns  = map csvBuildColumn summaries
    putStrLn $ "--> writing summaries to " ++ summaryName
    BSL.writeFile summaryName (encodePrettySorted summaries)

    csv <- readCSV csvName
    writeCSV csvName $ csvNewColumns newColumns csv

  where
    mul :: Natural -> Double -> Natural
    mul n d = floor $ d * fromIntegral n

    scriptNameExt = either (++ " (known script)") (++ " (from file)") plutusScript
    scriptName    = takeWhile (not . isSpace) scriptNameExt

checkPlutusLoop _ _ = error "calibrate-script: implementation error"


--
-- helpers
--

-- resolve protocol parameters file from --param PARAM
-- 1. resolve from 'data/protocol-parameters-v10.json', if not specified (file assumed to exist)
-- 2. try to resolve from file path
-- 3. try to resolve from 'data/' directory
readProtocolParametersOrDie :: Maybe FilePath -> IO ProtocolParameters
readProtocolParametersOrDie mFile =
  resolver mFile >>= eitherDecodeFileStrict' >>= either error pure
  where
    resolver Nothing  = getDataFileName ("data" </> "protocol-parameters-v10.json") >>= resolver . Just
    resolver (Just f) = doesFileExist f >>= bool (getDataFileName $ "data" </> f) (pure f)

resolveRedeemer :: Either ScriptData TxGenPlutusParams -> IO (Either TxGenError HashableScriptData)
resolveRedeemer (Left hsd) = do
  putStrLn "--> a hard-coded redeemer has been provided"
  pure $ Right $ unsafeHashableScriptData hsd
resolveRedeemer (Right PlutusOn{..}) =
  case plutusScript of
    -- it's a file path: we rely on a redeemer file that's been passed explicitly
    Right{} -> loader plutusRedeemer

    -- it's a built-in, either from the library or from scripts-fallback/
    -- 1. an explicitly passed in redeemer file takes precedence
    -- 2. a fallback redeemer is resolved from data/ by adding .redeemer.json
    -- NB: while scripts-fallback/ content might be used in production, data/ should *NEVER* be - it's for tx-generator development and testing only
    Left n  -> do
       let fallbackName = "data" </> n <.> "redeemer" <.> "json"
       fileExists     <- maybe (pure False) doesFileExist plutusRedeemer
       fallbackFile   <- try (getDataFileName fallbackName) <&> either (\SomeException{} -> "") id
       loader $ if fileExists then plutusRedeemer else Just fallbackFile
  where
    loader = \case
      Just f@(_:_) -> do
        putStrLn $ "--> will read redeemer from: " ++ f
        readScriptData f
      _ -> pure $ Left $ TxGenError "resolveRedeemer: no redeemer file resolved"

resolveRedeemer _ = pure $ Left $ TxGenError "resolveRedeemer: no Plutus script defined"

printScriptData :: ScriptData -> IO ()
printScriptData =
    BSL.putStrLn
  . encodePretty
  . scriptDataToJson ScriptDataJsonDetailedSchema
  . unsafeHashableScriptData

encodePrettySorted :: ToJSON a => a -> BSL.ByteString
encodePrettySorted = encodePretty' defConfig { confCompare = compare, confTrailingNewline = True }


--
-- command line parsing
--

parseCommandLine :: IO CommandLine
parseCommandLine
  = Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parserCommandLine Opt.fullDesc

parserCommandLine :: Parser CommandLine
parserCommandLine = subparser $
  mconcat
    [ parserOp "list" "list available scripts / strategies" (pure CLIList)
    , parserOp "run"  "calibrate script"                    parserRun
    ]

parserRun :: Parser CommandLine
parserRun =
  CLICalibrate
    <$> parseScriptName
    <*> parserStrategy
    <*> optional parseParamPath
  where
    parseScriptName =
          strArgument (help "name of a known script"        <> metavar "NAME")
      <|> strArgument (help "custom serialized script file" <> metavar "FILE" <> completer (bashCompleter "file"))
    parseParamPath =
      strOption $ long "param" <> metavar "JSON" <> completer (bashCompleter "file")
        <> help "protocol parameter file; default: data/protocol-parameters-v10.json"

parserStrategy :: Parser PlutusBudgetFittingStrategy
parserStrategy = a1 <|> a2 <|> pure TargetTxExpenditure
  where
    a1 = parserArg "txbudget" "exhaust the tx budget (default strategy)" $ \s -> if s == "txbudget" then Just TargetTxExpenditure else Nothing
    a2 = argument (maybeReader $ \s -> if s == "bnus" then Just (TargetTxsPerBlock 8) else Nothing) (help "help_txnsperbl" <> metavar "txperblock_<d>")

parserOp :: String -> String -> Parser a -> Mod CommandFields a
parserOp c descr p = command c $ info (p <**> helper) $ progDesc descr

parserArg :: String -> String -> (String -> Maybe a) -> Parser a
parserArg name descr mReader = argument (maybeReader mReader) (help descr <> metavar name)


---
--- CSV helpers
---

type CSVCells = [[Text]]

csvRows     :: [Text]
csvFillCell :: [PlutusBudgetSummary -> Text]
(csvRows, csvFillCell) = unzip
  [ (""               , const "scaling run")
  , ("limiting factor", showT . loopLimitingFactors)
  , ("loops per tx"   , showT . loopCounter)
  , ("loops per block", showT . projectedLoopsPerBlock)
  ]

showT :: Show a => a -> Text
showT = T.pack . show

-- returns an empty document if file not found
readCSV :: FilePath -> IO CSVCells
readCSV f = do
  exists <- doesFileExist f
  if exists
    then do
      putStrLn $ "--> appending to CSV: " ++ f
      map (T.splitOn ",") . T.lines <$> T.readFile f
    else do
      putStrLn $ "--> creating CSV: " ++ f
      pure $ transpose [csvRows]

writeCSV :: FilePath -> CSVCells -> IO ()
writeCSV f = T.writeFile f . T.unlines . map (T.intercalate ",")

csvNewColumns :: [[Text]] -> CSVCells -> CSVCells
csvNewColumns cols = transpose . (++ cols) . transpose

csvBuildColumn :: PlutusBudgetSummary -> [Text]
csvBuildColumn s = map ($ s) csvFillCell
