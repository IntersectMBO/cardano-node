{-# LANGUAGE DeriveGeneric #-}

module Cardano.Unlog.PlutusBudgetSummary
       ( PlutusBudgetSummary
       , readBudgetSummaryMaybe
       , writeResultsCSV
       ) where

import           Cardano.Ledger.Coin (Coin (..))

import           Prelude

import           Data.Aeson as Aeson (FromJSON, decodeFileStrict, parseJSON, withObject, (.:))
import           Data.List (transpose)
import           Data.Text as T (Text, intercalate, pack, unlines)
import           Data.Text.IO as T (writeFile)
import           GHC.Generics (Generic)
import           GHC.Natural (Natural)
import           Numeric (showFFloatAlt)
import           System.Directory (doesFileExist)


readBudgetSummaryMaybe :: FilePath -> IO (Maybe PlutusBudgetSummary)
readBudgetSummaryMaybe fp = do
  exists <- doesFileExist fp
  if exists
    then Aeson.decodeFileStrict fp
    else pure Nothing



--
-- These are duplicate definitions from bench/tx-generator/src/Cardano/TxGenerator/PlutusContext.hs
-- *MINUS* the fields that are irrelevant for the workload scaling CSV - commented out but left in for reference.
--
-- locli should not depend on the tx-generator package.
--

data PlutusBudgetSummary =
  PlutusBudgetSummary
  { budgetPerBlock          :: !ExecutionUnits
  , budgetPerTx             :: !ExecutionUnits
  , budgetPerTxInput        :: !ExecutionUnits
  -- , budgetStrategy          :: !PlutusBudgetFittingStrategy
  -- , budgetTarget            :: !ExecutionUnits
  -- , scriptId                :: !FilePath
  -- , scriptArgDatum          :: !ScriptData
  -- , scriptArgRedeemer       :: !ScriptData
  , loopCounter             :: !Int
  , loopLimitingFactors     :: ![PlutusAutoLimitingFactor]
  , budgetUsedPerTxInput    :: !ExecutionUnits
  , projectedBudgetUnusedPerBlock :: !ExecutionUnits
  , projectedBudgetUnusedPerTx    :: !ExecutionUnits
  , projectedTxPerBlock     :: !Int
  , projectedLoopsPerBlock  :: !Int
  , projectedTxSize         :: !(Maybe Int)
  , projectedTxFee          :: !(Maybe Coin)
  -- , messageStrategy         :: !(Maybe String)
  -- , messageId               :: !String
  }
  deriving Generic

instance FromJSON PlutusBudgetSummary



--
-- This is a duplicate definition from cardano-api:cardano-api/src/Cardano/Api/Plutus/Internal/Script.hs
--
-- locli should not depend on the full cardano-api package for this type alone.
--

data ExecutionUnits
  = ExecutionUnits
  { executionSteps :: Natural
  , executionMemory :: Natural
  }

instance FromJSON ExecutionUnits where
  parseJSON =
    Aeson.withObject "ExecutionUnits" $ \o ->
      ExecutionUnits
        <$> o .: "steps"
        <*> o .: "memory"



--
-- These are duplicate definitions from bench/tx-generator/app/calibrate-script.hs
--
-- locli should not depend on the tx-generator package.
--

data BudgetType
  = Mem
  | Steps

data PlutusAutoLimitingFactor
  = ExceededMemoryLimit
  | ExceededStepLimit
  deriving Generic

instance FromJSON PlutusAutoLimitingFactor

type CSVCells = [[Text]]

type BudgetSelector = PlutusBudgetSummary -> ExecutionUnits

select :: BudgetType -> ExecutionUnits -> Natural
select = \case
  Mem   -> executionMemory
  Steps -> executionSteps

csvRows     :: [Text]
csvFillCell :: [PlutusBudgetSummary -> Text]
(csvRows, csvFillCell) = unzip
  [ subheader "Protocol Parameters"
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
  ]
  where
    subheader h = (h, const "")
    emptyLine   = subheader ""

showT :: Show a => a -> Text
showT = T.pack . show

showTMaybe :: Show a => Maybe a -> Text
showTMaybe = maybe "" showT

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

writeCSV :: FilePath -> CSVCells -> IO ()
writeCSV f = T.writeFile f . T.unlines . map (T.intercalate ",")

writeResultsCSV :: FilePath -> [PlutusBudgetSummary] -> IO ()
writeResultsCSV csvName summaries = do
  let csv = transpose [csvRows]
  writeCSV csvName $ csvNewColumns newColumns csv
  where
    newColumns = map csvBuildColumn summaries

csvNewColumns :: [[Text]] -> CSVCells -> CSVCells
csvNewColumns cols = transpose . (++ cols) . transpose

csvBuildColumn :: PlutusBudgetSummary -> [Text]
csvBuildColumn s = map ($ s) csvFillCell
