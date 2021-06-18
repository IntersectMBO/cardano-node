
import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Coin as Shelley
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Plutus.V1.Ledger.Api as Plutus
import           PlutusTx (IsData)

import           Cardano.PlutusExample.Typed.DatumRedeemerGuess
import           Cardano.PlutusExample.Untyped.AlwaysSucceeds (alwaysSucceedsScript,
                   alwaysSucceedsScriptShortBs)
import           Cardano.PlutusExample.Untyped.DatumRedeemerGuess (datumRedeemerGuessScript,
                   datumRedeemerGuessScriptShortBs)


main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
              -- Typed datum and redeemer
              typedDatum = MyTypedDatum 42
              typedRedeemer = MyTypedRedeemer 42
              -- Budget results
              alwaysSucceedsUntyped = calculateBudgetUntyped pData pData alwaysSucceedsScriptShortBs m
              datumGuessTyped = calculateBudgetTyped typedDatum typedRedeemer typeddatumRedeemerGuessScriptAsShortBs m
              datumGuess = calculateBudgetUntyped pData pData datumRedeemerGuessScriptShortBs m

          in do printBudgetResult alwaysSucceedsUntyped "Untyped always succeeds script"
                printBudgetResult datumGuessTyped "Typed Datum guess"
                printBudgetResult datumGuess "Untyped Datum guess script"

        Nothing -> error "defaultCostModelParams failed"

  untypedAlwaysSucceedsResult <- writeFileTextEnvelope "untyped-always-succeeds-txin.plutus" Nothing alwaysSucceedsScript
  alwaysSucceedsTypedResult <- writeFileTextEnvelope "typed-redeemer-42-datum-42.plutus" Nothing typeddatumRedeemerGuessScript
  datumGuessResult <- writeFileTextEnvelope "untyped-redeemer-42-datum-42.plutus" Nothing datumRedeemerGuessScript
  mapM_ print [untypedAlwaysSucceedsResult, alwaysSucceedsTypedResult, datumGuessResult]


printBudgetResult
  :: (Plutus.LogOutput, Either Plutus.EvaluationError Plutus.ExBudget, ScriptData, ScriptData)
  -> String
  -> IO ()
printBudgetResult (logoutput, eEvalErrBudget, datum, redeemer) scriptFp = do
  print $ "Predicted budget for script: " <> scriptFp
  print $ "Log output: " <> show logoutput
  print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema datum)
  LBS.writeFile (scriptFp <> ".datum") (encode (scriptDataToJson ScriptDataJsonDetailedSchema datum))
  print $ "Datum hash: " <> serialiseToRawBytesHex (hashScriptData datum)
  print $ "Redeemer value: " <> encode (scriptDataToJson ScriptDataJsonNoSchema redeemer)
  print $ "Redeemer hash: " <>  serialiseToRawBytesHex (hashScriptData redeemer)
  case eEvalErrBudget of
    Left evalErr -> print $ "Eval Error: " <> show evalErr
    Right exbudget -> do
      --let scriptFeeInLoveLace = Alonzo.scriptfee (Alonzo.Prices (Shelley.Coin 1) (Shelley.Coin 1))
      --                            (Alonzo.ExUnits
      --                               { Alonzo.exUnitsMem = 1180
      --                               , Alonzo.exUnitsSteps = 5602000
      --                               }
      --                             )
      print $ "Ex Budget: " <> show exbudget
    --  print $ "Lovelace requires: " <> show scriptFeeInLoveLace
      print ""

calculateBudgetTyped
  :: (IsData a, IsData b)
  => a
  -- ^ Typed datum
  -> b
  -- ^ Typed redeemer
  -> Plutus.SerializedScript
  -> Plutus.CostModelParams
  -> (Plutus.LogOutput, Either Plutus.EvaluationError Plutus.ExBudget, ScriptData, ScriptData)
calculateBudgetTyped typedDatum typedRedeemer serScript costModelParams =
  let datum = Plutus.toData typedDatum
      redeemer = Plutus.toData typedRedeemer
      (lOutput, eBudget) = Plutus.evaluateScriptCounting Plutus.Verbose costModelParams serScript [datum, redeemer]
  in (lOutput, eBudget, fromAlonzoData $ Alonzo.Data datum, fromAlonzoData $ Alonzo.Data redeemer)

calculateBudgetUntyped
  :: Plutus.Data
  -- ^ Untyped datum
  -> Plutus.Data
  -- ^ Untyped redeemer
  -> Plutus.SerializedScript
  -> Plutus.CostModelParams
  -> (Plutus.LogOutput, Either Plutus.EvaluationError Plutus.ExBudget, ScriptData, ScriptData)
calculateBudgetUntyped datum redeemer serScript costModelParams =
  let (lOutput, eBudget) = Plutus.evaluateScriptCounting Plutus.Verbose costModelParams serScript [datum, redeemer]
  in (lOutput, eBudget, fromAlonzoData $ Alonzo.Data datum, fromAlonzoData $ Alonzo.Data redeemer)
