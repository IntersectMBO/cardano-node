{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import           Cardano.Api hiding (eraProtVerHigh)
import           Cardano.Api.Experimental (AnyWitness (..), IsEra (useEra),
                   PlutusScriptDatum (..), PlutusScriptPurpose (..), SignedTx (..),
                   eraProtVerHigh, makeKeyWitness, makeUnsignedTx,
                   obtainCommonConstraints, signTx, toPlutusSLanguage)
import           Cardano.Api.Experimental.AnyScriptWitness (AnyPlutusScriptWitness (..),
                   createPlutusSpendingScriptWitness)
import qualified Cardano.Api.Experimental as Exp (PlutusScriptInEra (..),
                   PlutusScriptOrReferenceInput (..), PlutusScriptWitness (..),
                   evaluateTransactionFee)
import           Cardano.Api.Experimental.Plutus (AnyPlutusScript (..),
                   plutusScriptInEraSLanguage)
import qualified Cardano.Api.Experimental.Tx as Exp

import           Cardano.Ledger.Core (mkCoinTxOut)
import qualified Cardano.Ledger.Plutus.Language as L (PlutusLanguage, Plutus (..), PlutusBinary (..),
                   SLanguage (..), decodePlutusRunnable)

import           Cardano.Benchmarking.Compiler (keyBenchmarkInputs)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts (listPlutusScripts)
#endif
import           Cardano.TxGenerator.Calibrate.Utils
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters (..),
                   convertToLedgerProtocolParameters)
import           Cardano.TxGenerator.Setup.Plutus
import           Cardano.TxGenerator.Tx (txSizeInBytes)
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils (keyAddress, mkTxIn)

import           Control.Exception
import           Data.Typeable (Typeable)
import           Data.Aeson (decodeFileStrict')
import qualified Data.ByteString.Lazy.Char8 as BSL (writeFile)

import           Data.Char
import           Data.Function (on, (&))
import           Data.List (nub, sort, transpose)
import           Data.List.Extra (split)
import           Data.Map.Strict as Map (Map, empty, fromList, union)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Numeric
import           Numeric.Natural
import           Options.Applicative as Opt
import           System.Directory
import           System.FilePath
import           Text.Read (readMaybe)

import           Paths_tx_generator hiding (version)


data CommandLine
    = CLIList
    | CLICalibrate
      { cName       :: FilePath                     -- ^ path to an external file, or name of an internally known script
      , cStrategy   :: PlutusBudgetFittingStrategy  -- ^ strategy for calibration
      , cParams     :: Maybe FilePath               -- ^ path to a ProtocolParameters JSON file, external or internal (data/)
      , cBudgetHint :: Maybe BudgetType             -- ^ hint as to which budget the script exhausts; default is based on the strategy
      , cScale      :: Maybe [Scale]                -- ^ explicit scalings to run (default: autos-cale)
      }
    deriving Show

data BudgetType
  = Mem
  | Steps
  deriving (Eq, Show, Read)

-- This mapping is a sensible default in the context of exising benchmarking workloads:
-- memory intensive loops calibrate for spending the tx budget, all (CPU-intensive) others for a stable txns per block
exhaustsBudget :: PlutusBudgetFittingStrategy -> BudgetType
exhaustsBudget = \case
  TargetTxExpenditure -> Mem
  _                   -> Steps

-- scaling factor used for each field of tx and block budget
data Scale = Scale
  { stxmem      :: !Double
  , stxstep     :: !Double
  , sblockmem   :: !Double
  , sblockstep  :: !Double
  }
  deriving (Eq, Show)

scaleFields :: Scale -> [Double]
scaleFields Scale{..} = [stxmem, stxstep, sblockmem, sblockstep]

scaleFromFields :: [Double] -> Maybe Scale
scaleFromFields [stxmem, stxstep, sblockmem, sblockstep] = Just Scale{..}
scaleFromFields _ = Nothing

scaleBaseLine :: Scale
scaleBaseLine = Scale 1.0 1.0 1.0 1.0

-- when no explicit scaling is requested on the CLI, we auto-calibrate to those factors
scaleAutoFactors :: [Double]
scaleAutoFactors = [1.5, 2.0]

-- input sanitizing for explicit scaling from the CLI
scaleSanitize :: [Scale] -> [Scale]
scaleSanitize = filter (\s -> s /= scaleBaseLine && all (> 0.0) (scaleFields s))


---
--- main
---

main :: IO ()
main = do
  cli <- parseCommandLine
  putStrLn $ "--> CLI parse: " ++ show cli ++ "\n"
  case cli of
    CLIList           -> runCommandList
    CLICalibrate{..}  -> do
      scriptNames <- knownScriptNames
      let
        s = if cName `elem` scriptNames then Left cName else Right cName
        p = PlutusOn LimitSaturationLoop s Nothing Nothing Nothing Nothing
        b = fromMaybe (exhaustsBudget cStrategy) cBudgetHint
      runPlutus cStrategy b cParams p (fromMaybe [] cScale)

runCommandList :: IO ()
runCommandList = do
  putStrLn "--> known fitting strategies:"
  putStrLn $ unlines
    [ "txbudget         -- such that the transaction budget is exhausted"
    , "                      (default strategy)"
    , "txperblock_<d>   -- such that the block budget is exhausted,"
    , "                      AND there are <d> script transactions per block"
    , "blockbudget_<r>  -- such that the block budget is exhausted (unsupported)"
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
  pure [name | (name, ext) <- splitExtension <$> contents, ext == ".plutus"]
#endif

-- | Initialize (or die), perform scaling run(s), write to CSV and JSON
runPlutus ::
     PlutusBudgetFittingStrategy
  -> BudgetType
  -> Maybe FilePath
  -> TxGenPlutusParams
  -> [Scale]
  -> IO ()
runPlutus s@TargetBlockExpenditure{} _ _ _ _
    -- NEXT RELEASE: This strategy is currently unused in benchmarks.
  = putStrLn $ "--> fitting strategy currently unsupported: " ++ show s
runPlutus strategy budgetType protoParamFile plutusDef@PlutusOn{..} scales
  = do
    protocolParameters <- readProtocolParametersOrDie protoParamFile
    (script, resolvedTo) <- either (error . show) pure =<< readPlutusScript plutusScript
    let era = useEra @ConwayEra
    anyPlutusScript <- obtainCommonConstraints era $
      case script of
        ScriptInAnyLang _lang (PlutusScript version (PlutusScriptSerialised sbs)) -> do
          let slang = toPlutusSLanguage version
              decode :: forall l. (L.PlutusLanguage l, Typeable l) => L.SLanguage l -> IO (AnyPlutusScript (ShelleyLedgerEra ConwayEra))
              decode _ = case L.decodePlutusRunnable @l (eraProtVerHigh era) (L.Plutus (L.PlutusBinary sbs)) of
                Left err -> throwIO $ userError $ "script decode failed: " ++ show err
                Right runnable -> pure $ AnyPlutusScript (Exp.PlutusScriptInEra runnable)
          obtainLangConstraints slang $ decode slang
        _ -> throwIO $ userError "expected a Plutus script"

    let
      redeemerDef   = Right plutusDef
      scriptNameExt = show resolvedTo
      scriptName    = takeWhile (/= '.') . drop 1 . dropWhile (not . isSpace) $ scriptNameExt
    putStrLn $ "--> got script " ++ scriptNameExt

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

      baseline@(baseSummary, _) =
        summaryAndRedeermerOrDie "baseline"
          $ plutusAutoScaleBlockfit protocolParameters (scriptNameExt, show strategy) script autoBudget strategy 1
    summaries_ <- baseline `seq`
      runScaling script budgetType protocolParameters baseSummary autoBudget scales

    let
      summaries = baseline : summaries_
      jsonName  = "summaries_" ++ scriptName <.> "json"
      csvName   = "scaling_" ++ scriptName <.> "csv"

    ledgerProtocolParameters <-
          either (error . docToString . prettyError) pure
            $ convertToLedgerProtocolParameters shelleyBasedEra protocolParameters
    summariesWithApprox <- mapM (approximateTxProperties anyPlutusScript ledgerProtocolParameters) summaries
    writeResultsJSON jsonName summariesWithApprox
    writeResultsCSV csvName summariesWithApprox

runPlutus _ _ _ _ _ = error "calibrate-script: implementation error"

-- | Perform scaling run(s). When no explicit Scale values are provided, do auto-scaling.
runScaling ::
     ScriptInAnyLang
  -> BudgetType
  -> ProtocolParameters
  -> PlutusBudgetSummary
  -> PlutusAutoBudget
  -> [Scale]
  -> IO [(PlutusBudgetSummary, ScriptRedeemer)]
runScaling script budgetType basePParams baseline baseBudget (scaleSanitize -> scalesArg)
  | null scalesArg = go1 scaleBaseLine scaleAutoFactors
  | otherwise      = mapM go0 scalesArg
  where
    -- specific scaling requested
    go0 :: Scale -> IO (PlutusBudgetSummary, ScriptRedeemer)
    go0 scale =
      let
        fields@[txm, txs, bm, bs] = scaleFields scale
        pparams = applyScale scale basePParams
        scope   = unwords $ show strategy : mapMaybe whenScaled [("txm", txm), ("txs", txs), ("bm", bm), ("bs", bs)]
        budget  = withHint (maximum fields) baseBudget
      in do
        putStrLn $ "--> run: " ++ show scope
        evaluate $
          summaryAndRedeermerOrDie scope $
            plutusAutoScaleBlockfit pparams (scriptNameExt, scope) script budget strategy 1

    -- auto-scale until conditions are met (last list entry), but include intermediate results
    go1 :: Scale -> [Double] -> IO [(PlutusBudgetSummary, ScriptRedeemer)]
    go1 _ [] = pure []
    go1 scale_ factors@(factor:fs) =
      case bumpLimit strategy budgetType factor scale_ of
        Nothing     -> go1 scaleBaseLine fs     -- no further bump possible: give up and auto-scale for next factor
        Just scale  -> do
          run@(summary, _) <- go0 scale
          (run :) <$>
            if all (\cond -> cond summary baseline) happilyCalibrated
              then go1 scaleBaseLine fs         -- auto-scale next factor
              else go1 scale factors            -- apply next bump to current scaling, re-run

    -- conditions for a succesful calibration: same limiting factors, same txn count per block
    -- NEXT RELEASE: does that need to be strategy dependent with TargetBlockExpenditure?
    happilyCalibrated = [(==) `on` loopLimitingFactors, (==) `on` projectedTxPerBlock]

    strategy      = budgetStrategy baseline
    scriptNameExt = scriptId baseline

    whenScaled (name, val) = if val /= 1.0 then Just $ name ++ ":" ++ show val else Nothing

    -- heuristic hint at the maximum expected loop counter; helps speed up calibtation of scripts with low counters
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

-- For each fitting strategy and budget type targeted by some script,
-- there's an order to which budget limit is bumped until a suitable calibration is found.
-- If no further bump is possible given a Scale, the result will be Nothing.
bumpLimit :: PlutusBudgetFittingStrategy -> BudgetType -> Double -> Scale -> Maybe Scale
bumpLimit strategy budgetType factor (scaleFields -> scale) = go precedence
  where
    go []     = Nothing
    go (i:ix) = if scale !! i == factor then go ix else scaleFromFields $ replaceAt i factor scale

    precedence = case (strategy, budgetType) of
      (TargetTxExpenditure, Mem)    -> [0, 2, 3, 1]     -- bump order: txmem blockmem blocksteps txsteps
      (TargetTxExpenditure, Steps)  -> [1, 3, 2, 0]     -- bump order: txsteps blocksteps blockmem txmem
      (TargetTxsPerBlock{}, Mem)    -> [2, 3]           -- bump order: blockmem blocksteps
      (TargetTxsPerBlock{}, Steps)  -> [3, 2]           -- bump order: blocksteps blockmem
      (TargetBlockExpenditure{}, _) -> []               -- NEXT RELEASE: is not used in benchmarks currently, so no viable precendence defined.


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
    ]

parserRun :: Parser CommandLine
parserRun =
  CLICalibrate
    <$> parseScriptName
    <*> parserStrategy
    <*> optional parseParamPath
    <*> optional parseBudgetHint
    <*> optional (some parseScale)
  where
    parseScriptName =
          strArgument (help "name of a known script"        <> metavar "NAME")
      <|> strArgument (help "custom serialized script file" <> metavar "FILE" <> completer (bashCompleter "file"))
    parseParamPath =
      strOption $ long "param" <> metavar "JSON" <> completer (bashCompleter "file")
        <> help "protocol parameter file; default: data/protocol-parameters-v10.json"
    parseBudgetHint =
      option auto $ long "hint" <> metavar "BUDGET"
        <> help "Which budget does the script target? <Mem|Steps>"
    parseScale =
      option readScale $ long "scale" <> short 's' <> metavar "scaling"
        <> help "4 explicit scaling factors, quoted: \"txmem txsteps blockmem blocksteps\""

readScale :: Opt.ReadM Scale
readScale = Opt.maybeReader
  (scaleFromFields . mapMaybe readMaybe . split isSpace)

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
  , ("~ tx fee"           , showTMaybe . (unCoin `fmap`) . projectedTxFee)
  , ("~ tx size"          , showTMaybe . projectedTxSize)
  , ("~ block size"       , showTMaybe . projectedBlockSize)
  ]
  where
    subheader h = (h, const "")
    emptyLine   = subheader ""

showT :: Show a => a -> Text
showT = T.pack . show

showTMaybe :: Show a => Maybe a -> Text
showTMaybe = maybe "" showT

projectedBlockSize :: PlutusBudgetSummary -> Maybe Int
projectedBlockSize s = calc `fmap` projectedTxSize s
  where
    calc txSize = 864 + projectedTxPerBlock s * txSize    -- a constant average header size

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

-- Ensures appending to an existing CSV file.
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

-- Ensures removal of duplicate result columns.
writeResultsCSV :: FilePath -> [PlutusBudgetSummary] -> IO ()
writeResultsCSV csvName summaries = do
  csv <- readCSV csvName
  writeCSV csvName $ csvNewColumns newColumns csv
  where
    newColumns = map csvBuildColumn summaries

csvNewColumns :: [[Text]] -> CSVCells -> CSVCells
csvNewColumns cols = transpose . nub . (++ cols) . transpose

csvBuildColumn :: PlutusBudgetSummary -> [Text]
csvBuildColumn s = map ($ s) csvFillCell


---
--- JSON helpers
---

type SummariesJSON = Map String PlutusBudgetSummary

-- Ensures extending an existing JSON file; new entries will overwrite exsiting ones with the same key.
writeResultsJSON :: FilePath -> [PlutusBudgetSummary] -> IO ()
writeResultsJSON jsonName summaries = do
  existing <- readJSON
  BSL.writeFile jsonName $ encodePrettySorted $ newEntries `Map.union` existing
  where
    newEntries = Map.fromList [ (messageId s, s) | s <- summaries ]
    createJSON = do
      putStrLn $ "--> creating JSON: " ++ jsonName
      pure Map.empty
    extJSON j  = do
      putStrLn $ "--> extending JSON: " ++ jsonName
      pure j
    readJSON :: IO SummariesJSON
    readJSON = do
      exists <- doesFileExist jsonName
      if not exists
        then createJSON
        else decodeFileStrict' jsonName >>= maybe createJSON extJSON


---
--- Tx helpers
---

-- | Builds a dummy transaction that resembles the ones submitted during some Plutus benchmark and
--   uses it to augment the budget summary with txn size and fee.
--   * If anything fails to evaluate, the summary is returned unchanged.
--   * This function is currently monomorphic in the ledger era and will resolve era parameters to Conway.
approximateTxProperties :: AnyPlutusScript (ShelleyLedgerEra ConwayEra) -> LedgerProtocolParameters ConwayEra -> (PlutusBudgetSummary, ScriptRedeemer) -> IO PlutusBudgetSummary
approximateTxProperties (AnyPlutusScript ps) ledgerProtocolParameters (summary, redeemer) = do
  putStrLn $ "--> approximating txn size and fee for: " ++ messageId summary
  let
    era = useEra @ConwayEra
    pparams = unLedgerProtocolParameters ledgerProtocolParameters
    slang = plutusScriptInEraSLanguage ps
    dummyDatum = unsafeHashableScriptData $ ScriptDataNumber 0
    datum = mkSpendingScriptDatum slang dummyDatum
    anyWitness =
      let witness = Exp.PlutusScriptWitness slang (Exp.PScript ps) datum redeemer (budgetUsedPerTxInput summary)
      in AnyPlutusScriptWitness (AnyPlutusSpendingScriptWitness (createPlutusSpendingScriptWitness slang witness))
  obtainCommonConstraints era (do
    -- Corresponds to the metadata inserted in benchmarking workloads, which is why it's needed for the estimate.
    -- default value taken from: `add_tx_size` in nix/nixos/tx-generator-service.nix
    dummyMetadata <- either (\e -> throwIO $ userError $ "approximateTxProperties: mkMetadata: " ++ e) pure
                       $ mkMetadata @ConwayEra 100
    let
      expMetadata = case dummyMetadata of
        TxMetadataNone      -> mempty
        TxMetadataInEra _ m -> m

      -- just placeholders
      dummyTxIn ix = mkTxIn $ "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162#" <> textShow @Int ix
      dummyTxOut :: Exp.TxOut (ShelleyLedgerEra ConwayEra)
      dummyTxOut = Exp.TxOut $ mkCoinTxOut (toShelleyAddr $ keyAddress @ConwayEra (Testnet (NetworkMagic 42)) keyBenchmarkInputs) (Coin 1_000_000)

      -- build a dummy tx akin to what we'd get in the tx-generator's benchmarking workload;
      -- it just needs to be sufficient to get our approximations.
      txBodyContent =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(dummyTxIn 0, anyWitness)]
          & Exp.setTxInsCollateral [dummyTxIn 1]
          & Exp.setTxOuts [dummyTxOut]
          & Exp.setTxFee (Coin 1_000_000)
          & Exp.setTxMetadata expMetadata
          & Exp.setTxProtocolParams pparams

    unsignedTx <- either (\err -> throwIO $ userError $ "approximateTxProperties: " ++ show err) pure
                    $ makeUnsignedTx era txBodyContent

    let
      witVKey = makeKeyWitness era unsignedTx (WitnessPaymentKey keyBenchmarkInputs)
      dummyTx :: SignedTx ConwayEra
      dummyTx = signTx era [] [witVKey] unsignedTx

    pure summary
      { projectedTxSize = Just $ txSizeInBytes dummyTx
      , projectedTxFee  = Just $ Exp.evaluateTransactionFee pparams unsignedTx 2 0 0  -- 1 (script witness) + 1 (collateral) = 2
      }
    ) `catch` \(SomeException e) -> do
      putStrLn $ "approximation failed: " ++ show e
              ++ "\n--> using unmodified summary"
      pure summary

-- TODO: remove these vendored functions and import from
-- Cardano.Api.Experimental when the published cardano-api version includes them.

obtainLangConstraints :: L.SLanguage lang -> ((L.PlutusLanguage lang, Typeable lang) => a) -> a
obtainLangConstraints L.SPlutusV1 f = f
obtainLangConstraints L.SPlutusV2 f = f
obtainLangConstraints L.SPlutusV3 f = f
obtainLangConstraints L.SPlutusV4 f = f

mkSpendingScriptDatum :: L.SLanguage lang -> HashableScriptData -> PlutusScriptDatum lang 'SpendingScript
mkSpendingScriptDatum = \case
  L.SPlutusV1 -> SpendingScriptDatum
  L.SPlutusV2 -> SpendingScriptDatum
  L.SPlutusV3 -> SpendingScriptDatum . Just
  L.SPlutusV4 -> SpendingScriptDatum . Just

