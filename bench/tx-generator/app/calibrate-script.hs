{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.Api hiding (toLedgerPParams)
import           Cardano.Api.Internal.ProtocolParameters (ProtocolParameters (..), toLedgerPParams)

#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts (listPlutusScripts)
#endif
import           Cardano.TxGenerator.Calibrate.Utils
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.Setup.Plutus
import           Cardano.TxGenerator.Types

import           Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.Char
import           Data.Function (on)
import           Data.List (nub, sort, transpose)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Numeric
import           Numeric.Natural
import           Options.Applicative as Opt
import           System.Directory
import           System.FilePath
-- import           Text.Read (readMaybe)

import           Paths_tx_generator

-- import Debug.Trace

-- import qualified PlutusTx


data CommandLine
    = CLIList
    | CLICalibrate
      { cName      :: FilePath
      , cStrategy  :: PlutusBudgetFittingStrategy
      , cParams    :: Maybe FilePath
      , cScale     :: Maybe [Scale]
      }
    | CLITest
    deriving Show

data Scale = Scale
  { stxmem      :: !Double
  , stxstep     :: !Double
  , sblockmem   :: !Double
  , sblockstep  :: !Double
  }
  deriving (Eq, Show)

scaleFields :: Scale -> [Double]
scaleFields Scale{..} = [stxmem, stxstep, sblockmem, sblockstep]

_scaleFromFields :: [Double] -> Maybe Scale
_scaleFromFields [stxmem, stxstep, sblockmem, sblockstep] = Just Scale{..}
_scaleFromFields _ = Nothing

scaleBaseLine :: Scale
scaleBaseLine = Scale 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  cli <- parseCommandLine
  putStrLn $ "--> CLI parse: " ++ show cli
  case cli of
    CLIList           -> runCommandList
    CLICalibrate{..}  -> do
      scriptNames <- knownScriptNames
      let
        p = PlutusOn LimitSaturationLoop s Nothing Nothing Nothing Nothing
        s = if cName `elem` scriptNames then Left cName else Right cName
      runPlutus cParams p cStrategy (fromMaybe [] cScale)
    CLITest -> readProtocolParametersOrDie Nothing >>= approximateTxProperties

runCommandList :: IO ()
runCommandList = do
  putStrLn "--> known fitting strategies:"
  putStrLn $ unlines
    [ "txbudget         -- such that the transaction budget is exhausted"
    , "                      (default strategy)"
    , "txperblock_<d>   -- such that the block budget is exhausted,"
    , "                      AND there are <d> script transactions per block"
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

runPlutus ::
     Maybe FilePath
  -> TxGenPlutusParams
  -> PlutusBudgetFittingStrategy
  -> [Scale]
  -> IO ()
runPlutus protoParamFile _plutusDef@PlutusOn{..} strategy scales
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

    let
      autoBudget = PlutusAutoBudget
        { autoBudgetUnits = fromMaybe (error "autoBudget: cannot find protocolParamMaxTxExUnits") (protocolParamMaxTxExUnits protocolParameters)
        , autoBudgetDatum = ScriptDataNumber 0
        , autoBudgetRedeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) redeemer
        , autoBudgetUpperBoundHint = Nothing
        }

      baseline = summaryOrDie "baseline" $
        plutusAutoScaleBlockfit protocolParameters (scriptNameExt, show strategy) script autoBudget strategy 1
    summaries_ <- baseline `seq`
      runScaling script protocolParameters baseline autoBudget scales

    let
      summaries   = baseline : summaries_
      summaryName = "summaries_" ++ scriptName <.> "json"
      csvName     = "scaling_" ++ scriptName <.> "csv"
      newColumns  = map csvBuildColumn summaries
    putStrLn $ "--> writing summaries to " ++ summaryName
    BSL.writeFile summaryName (encodePrettySorted summaries)
    csv <- readCSV csvName
    writeCSV csvName $ csvNewColumns newColumns csv
  where
    scriptNameExt = either (++ " (known script)") (++ " (from file)") plutusScript
    scriptName    = takeWhile (not . isSpace) scriptNameExt

runPlutus _ _ _ _ = error "calibrate-script: implementation error"

sanitizeScales :: [Scale] -> [Scale]
sanitizeScales = filter (\s -> s /= scaleBaseLine && all (> 0.0) (scaleFields s))

bumpLimit :: PlutusBudgetFittingStrategy -> Double -> Scale -> Maybe Scale
bumpLimit strategy factor (scaleFields -> scale) = go precedence
  where
    go []     = Nothing
    go (i:ix) = if scale !! i == factor then go ix else _scaleFromFields $ replaceAt i factor scale

    replaceAt :: Int -> a -> [a] -> [a]
    replaceAt at new xs =
      let (pref, suff) = splitAt at xs
      in pref ++ new : drop 1 suff

    precedence = case strategy of
      TargetTxExpenditure       -> [0, 2, 3, 1]
      TargetTxsPerBlock{}       -> [3, 2]
      TargetBlockExpenditure{}  -> []

runScaling ::
     ScriptInAnyLang
  -> ProtocolParameters
  -> PlutusBudgetSummary
  -> PlutusAutoBudget
  -> [Scale]
  -> IO [PlutusBudgetSummary]
runScaling script basePParams baseline baseBudget (sanitizeScales -> scalesArg)
  | null scalesArg = go1 scaleBaseLine [1.5, 2.0]
  | otherwise      = mapM go0 scalesArg
  where
    go0 :: Scale -> IO PlutusBudgetSummary
    go0 scale =
      let
        fields@[txm, txs, bm, bs] = scaleFields scale
        pparams = applyScale scale basePParams
        scope   = unwords $ show strategy : mapMaybe whenScaled [("txm", txm), ("txs", txs), ("bm", bm), ("bs", bs)]
        budget  = withHint (maximum fields) baseBudget
      in do
        putStrLn $ "--> run: " ++ show scope
        evaluate $
          summaryOrDie scope $
            plutusAutoScaleBlockfit pparams (scriptNameExt, scope) script budget strategy 1

    go1 :: Scale -> [Double] -> IO [PlutusBudgetSummary]
    go1 _ [] = pure []
    go1 scale_ factors@(factor:fs) =
      case bumpLimit strategy factor scale_ of
        Nothing     -> go1 scaleBaseLine fs
        Just scale  -> do
          summary <- go0 scale
          (summary :) <$> 
            if all (\condition -> condition summary baseline) noBumpNecessary
              then go1 scaleBaseLine fs
              else go1 scale factors
    -- TODO make strategy dependent
    noBumpNecessary = [(==) `on` loopLimitingFactors, (==) `on` projectedTxPerBlock]

    strategy      = budgetStrategy baseline
    scriptNameExt = scriptId baseline

    whenScaled (name, val) = if val /= 1.0 then Just $ name ++ ":" ++ show val else Nothing

    withHint :: Double -> PlutusAutoBudget -> PlutusAutoBudget
    withHint d b =
      b {autoBudgetUpperBoundHint = Just $ ceiling $ 1.25 * d * fromIntegral (loopCounter baseline)}

applyScale :: Scale -> ProtocolParameters -> ProtocolParameters
applyScale Scale{..} p@ProtocolParameters
  { protocolParamMaxTxExUnits
  , protocolParamMaxBlockExUnits
  } = p
    { protocolParamMaxTxExUnits     = apply stxmem stxstep       <$> protocolParamMaxTxExUnits
    , protocolParamMaxBlockExUnits  = apply sblockmem sblockstep <$> protocolParamMaxBlockExUnits
    }
    where
      apply :: Double -> Double -> ExecutionUnits -> ExecutionUnits
      apply mem steps ExecutionUnits{..} = ExecutionUnits
        { executionMemory  = executionMemory `mul` mem
        , executionSteps   = executionSteps  `mul` steps
        }

      mul :: Natural -> Double -> Natural
      mul n 1.0 = n
      mul n d   = floor $ d * fromIntegral n

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
    [ parserOp "list" "list available scripts / strategies"  (pure CLIList)
    , parserOp "run"  "calibrate script"                     parserRun
    , parserOp "test" "run internal test (development only)" (pure CLITest)
    ]

parserRun :: Parser CommandLine
parserRun =
  CLICalibrate
    <$> parseScriptName
    <*> parserStrategy
    <*> optional parseParamPath
    -- <*> optional (some (option readScale (long "scale" <> short 's' <> help "4 explicit scaling factors: txmem txstep blockmem blockstep")))
    -- <*> optional (some (option readScale (long "scale" <> short 's' <> help "4 explicit scaling factors: txmem txstep blockmem blockstep")))
    <*> optional (some parserScale)
  where
    parseScriptName =
          strArgument (help "name of a known script"        <> metavar "NAME")
      <|> strArgument (help "custom serialized script file" <> metavar "FILE" <> completer (bashCompleter "file"))
    parseParamPath =
      strOption $ long "param" <> metavar "JSON" <> completer (bashCompleter "file")
        <> help "protocol parameter file; default: data/protocol-parameters-v10.json"

parserScale :: Parser Scale
parserScale = p0
  where p0 = Scale <$> p1 <*> p1 <*> p1 <*> p1
        p1 = argument auto (help "bla" <> metavar "foo")
{-}
readScale :: Opt.ReadM Scale
readScale = Opt.maybeReader $ \arg ->
  let args    = take 4 (split isSpace arg)
      fields  = mapMaybe readMaybe args
  in traceShow args $ scaleFromFields fields
-}
parserStrategy :: Parser PlutusBudgetFittingStrategy
parserStrategy = strategy <|> pure TargetTxExpenditure
  where
    strategy  = argument auto (help "fitting strategy; 'list' command to show choices" <> metavar "strategy")

parserOp :: String -> String -> Parser a -> Mod CommandFields a
parserOp c descr p = command c $ info (p <**> helper) $ progDesc descr


---
--- CSV helpers
---

type CSVCells = [[Text]]

type BudgetSelector = PlutusBudgetSummary -> ExecutionUnits

data BudgetType
  = Mem
  | Steps

select :: BudgetType -> ExecutionUnits -> Natural
select = \case
  Mem   -> executionMemory
  Steps -> executionSteps


csvRows     :: [Text]
csvFillCell :: [PlutusBudgetSummary -> Text]
(csvRows, csvFillCell) = unzip
  [ (""                   , T.pack . messageId)
  , emptyLine
  , subheader "Protocol Parameters"
  , subheader "block budget"
  , ("memory"             , showBudget Mem   . budgetPerBlock)
  , ("steps"              , showBudget Steps . budgetPerBlock)
  , subheader "tx budget"
  , ("memory"             , showBudget Mem   . budgetPerTx)
  , ("steps"              , showBudget Steps . budgetPerTx)
  , emptyLine
  , subheader "Unused Budget"
  , subheader "block budget"
  , ("- memory"           , showBudget Mem . projectedBudgetUnusedPerBlock)
  , ("- - %"              , showBudgetPerc Mem projectedBudgetUnusedPerBlock budgetPerBlock)
  , ("- steps"            , showBudget Steps . projectedBudgetUnusedPerBlock)
  , ("- - %"              , showBudgetPerc Steps projectedBudgetUnusedPerBlock budgetPerBlock)
  , subheader "tx budget"
  , ("- memory"           , showBudget Mem . projectedBudgetUnusedPerTx)
  , ("- - %"              , showBudgetPerc Mem projectedBudgetUnusedPerTx budgetPerTx)
  , ("- steps"            , showBudget Steps . projectedBudgetUnusedPerTx)
  , ("- - %"              , showBudgetPerc Steps projectedBudgetUnusedPerTx budgetPerTx)
  , emptyLine
  , ("limiting factor"    , showLimitFactor . loopLimitingFactors)
  , ("loops per tx"       , showT . loopCounter)
  , ("txns per block"     , showT . projectedTxPerBlock)
  , ("loops per block"    , showT . projectedLoopsPerBlock)
  ]
  where
    subheader h = (h, const "")
    emptyLine   = subheader ""

showT :: Show a => a -> Text
showT = T.pack . show

showLimitFactor :: [PlutusAutoLimitingFactor] -> Text
showLimitFactor = T.intercalate "+" . map go
  where
    go ExceededMemoryLimit = "memory"
    go ExceededStepLimit   = "steps"

showBudget :: BudgetType -> ExecutionUnits -> Text
showBudget s = showT . select s

showBudgetPerc :: BudgetType -> BudgetSelector -> BudgetSelector -> PlutusBudgetSummary -> Text
showBudgetPerc s numerator denominator summary =
  T.pack (showFFloatAlt (Just 2) perc "") <> "%"
  where
    perc  :: Double
    perc  = (100 * fromIntegral num) / fromIntegral denom
    num   = select s $ numerator summary
    denom = select s $ denominator summary

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

-- add new columns, removing duplicates
csvNewColumns :: [[Text]] -> CSVCells -> CSVCells
csvNewColumns cols = transpose . nub . (++ cols) . transpose

csvBuildColumn :: PlutusBudgetSummary -> [Text]
csvBuildColumn s = map ($ s) csvFillCell

---
--- Tx helpers
---

approximateTxProperties :: ProtocolParameters -> IO ()
approximateTxProperties protocolParameters =
  case toLedgerPParams era protocolParameters of
    Left{}               -> putStrLn "approximateTxProperties: could not convert to Conway ledgerPParams"
    Right ledgerPParams  -> print ledgerPParams
  where
    era = ShelleyBasedEraConway

{-
    txBodyContent :: _ -> TxBody ConwayEra
    txBodyContent ledgerPParams = defaultTxBodyContent era
      & setTxIns (map (\f -> (getFundTxIn f, BuildTxWith $ getFundWitness f)) inFunds)
      & setTxInsCollateral collateral
      & setTxOuts outputs
      & setTxFee fee
      & setTxValidityLowerBound TxValidityNoLowerBound
      & setTxValidityUpperBound (defaultTxValidityUpperBound era)
      & setTxMetadata metadata
      & setTxProtocolParams (BuildTxWith (Just ledgerPParams))
  do
  case sourceTransactionPreview txGenerator fundPreview inToOut (mangle $ repeat toUTxO) of
    Left err -> traceDebug $ "Error creating Tx preview: " ++ show err
    Right tx -> do
      let
        txSize = txSizeInBytes tx
        txFeeEstimate = case toLedgerPParams defaultEra protocolParameters of
          Left{}              -> Nothing
          Right ledgerPParams -> Just $
            evaluateTransactionFee shelleyBasedEra ledgerPParams (getTxBody tx) (fromIntegral $ inputs + 1) 0 0    -- 1 key witness per tx input + 1 collateral
      putStrLn $ "Projected Tx size in bytes: " ++ show txSize
      putStrLn $ "Projected Tx fee in Coin: " ++ show txFeeEstimate
-}