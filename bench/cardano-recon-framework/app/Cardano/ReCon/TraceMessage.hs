{- HLINT ignore "Use newtype instead of data" -}
module Cardano.ReCon.TraceMessage where

import           Cardano.Logging
import qualified Cardano.Logging.Types.TraceMessage as Envelop
import           Cardano.ReCon.Common (extractProps)
import           Cardano.ReCon.LTL.Lang.Formula (Formula, PropValue (..), Relevance)
import qualified Cardano.ReCon.LTL.Lang.Formula.Prec as Prec
import           Cardano.ReCon.LTL.Pretty (prettyFormula)
import           Cardano.ReCon.LTL.Satisfy (SatisfactionResult (..))
import           Cardano.ReCon.Trace.Feed (TemporalEvent (..))

import           Data.Aeson (Value (..), (.=))
import           Data.Aeson.Encode.Pretty
import           Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)


data TraceMessage = FormulaStartCheck {formula :: Formula TemporalEvent Text}
                  | FormulaProgressDump {
                      eventsPerSecond :: Word,
                      catchupRatio    :: Double,
                      formula         :: Formula TemporalEvent Text
                    }
                  -- | Formula outcomes are split into (+) and (-) as we'd like to print them at distinct severity levels,
                  --   but severity level is indexed by namespace (= constructor name), hence
                  --   we can't store both outcomes in one constructor.
                  | FormulaPositiveOutcome { formula :: Formula TemporalEvent Text }
                  | FormulaNegativeOutcome {
                      formula   :: Formula TemporalEvent Text,
                      relevance :: Relevance TemporalEvent Text
                  }

-- | Smart constructor.
formulaOutcome :: Formula TemporalEvent Text -> SatisfactionResult TemporalEvent Text -> TraceMessage
formulaOutcome formula Satisfied         = FormulaPositiveOutcome formula
formulaOutcome formula (Unsatisfied rel) = FormulaNegativeOutcome formula rel

green :: Text -> Text
green text = "\x001b[32m" <> text <> "\x001b[0m"

red :: Text -> Text
red text = "\x001b[31m" <> text <> "\x001b[0m"

prettyTraceMessage :: Envelop.TraceMessage -> Text
prettyTraceMessage Envelop.TraceMessage{..} =
  toStrict $ toLazyText $ encodePrettyToTextBuilder  $
    Map.insert "at" (TextValue (showT tmsgAt))       $
      Map.insert "namespace" (TextValue tmsgNS)      $
        Map.insert "host" (TextValue tmsgHost)       $
          Map.insert "thread" (TextValue tmsgThread) $
            extractProps tmsgData

-- MKREV: never leave undefined values in production code.
-- replace the fallback string which whatever provides most information
prettyTemporalEvent :: TemporalEvent -> Text -> Text
prettyTemporalEvent (TemporalEvent _ msgs) ns =
  maybe "<<undefined>>" prettyTraceMessage (find (\ x -> x.tmsgNS == ns) msgs)

prettySatisfactionResult :: Formula TemporalEvent Text -> SatisfactionResult TemporalEvent Text -> Text
prettySatisfactionResult initial Satisfied = prettyFormula initial Prec.Universe <> " " <> green "(✔)"
prettySatisfactionResult initial (Unsatisfied rel) =
  prettyFormula initial Prec.Universe <> red " (✗)" <> "\n"
    <> Text.intercalate
         "\n----------------------------------------------\n"
         (fmap (uncurry prettyTemporalEvent) (Set.toList rel))

instance LogFormatting TraceMessage where
  forMachine _ FormulaStartCheck{..} = mconcat
    [
      "formula" .= String (prettyFormula formula Prec.Universe)
    ]
  forMachine _ FormulaProgressDump{..} = mconcat
    [
      "events_per_second" .= Number (fromIntegral eventsPerSecond),
      "catch_up_ratio" .= Number (realToFrac catchupRatio)
    ]
  forMachine _ FormulaPositiveOutcome{..} = mconcat
    [
      "formula" .= String (prettyFormula formula Prec.Universe)
    ]
  forMachine _ FormulaNegativeOutcome{..} = mconcat
    [
      "formula" .= String (prettyFormula formula Prec.Universe)
    ,
      "relevance" .= String (showT relevance)
    ]

  forHuman FormulaStartCheck{..} =
    "Starting satisfiability check on: " <> prettyFormula formula Prec.Universe
  forHuman FormulaProgressDump{..} =
         "\nevent/s: " <> showT eventsPerSecond <> "\n"
      <> "catch-up ratio: "
            <> showT catchupRatio
            <> "  // values above 1.0 <=> realtime"
            <> "\n"
      <> "formula: " <> prettyFormula formula Prec.Universe
  forHuman FormulaPositiveOutcome{..} =
    prettySatisfactionResult formula Satisfied
  forHuman FormulaNegativeOutcome{..} =
    prettySatisfactionResult formula (Unsatisfied relevance)

  asMetrics FormulaStartCheck{} = []
  asMetrics (FormulaProgressDump {catchupRatio}) = [DoubleM "catchup_ratio" catchupRatio]
  asMetrics FormulaPositiveOutcome{} = []
  asMetrics FormulaNegativeOutcome{} = []


instance MetaTrace TraceMessage where
  allNamespaces =
    [
      Namespace [] ["ReCon", "FormulaStartCheck"]
    ,
      Namespace [] ["ReCon", "FormulaProgressDump"]
    ,
      Namespace [] ["ReCon", "FormulaOutcome"]
    ]

  namespaceFor FormulaStartCheck{} = Namespace [] ["ReCon", "FormulaStartCheck"]
  namespaceFor FormulaProgressDump{} = Namespace [] ["ReCon", "FormulaProgressDump"]
  namespaceFor FormulaPositiveOutcome{} = Namespace [] ["ReCon", "FormulaPositiveOutcome"]
  namespaceFor FormulaNegativeOutcome{} = Namespace [] ["ReCon", "FormulaNegativeOutcome"]

  severityFor (Namespace [] ["ReCon", "FormulaStartCheck"])       _ = Just Info
  severityFor (Namespace [] ["ReCon", "FormulaProgressDump"])     _ = Just Debug
  severityFor (Namespace [] ["ReCon", "FormulaPositiveOutcome"])  _ = Just Info
  severityFor (Namespace [] ["ReCon", "FormulaNegativeOutcome"])  _ = Just Notice
  severityFor _                                                           _ = Nothing

  detailsFor _ _ =  Just DNormal

  documentFor (Namespace [] ["ReCon", "FormulaStartCheck"]) =
    Just "Formula satisfiability check has started."
  documentFor (Namespace [] ["ReCon", "FormulaProgressDump"]) =
    Just "Dump of formula satisfiability check progress."
  documentFor (Namespace [] ["ReCon", "FormulaPositiveOutcome"]) =
    Just "Formula evaluates to `true` against the given timeline of events."
  documentFor (Namespace [] ["ReCon", "FormulaNegativeOutcome"]) =
    Just "Formula evaluates to `false` against the given timeline of events."
  documentFor _ = Nothing

  metricsDocFor (Namespace [] ["ReCon", "FormulaProgressDump"]) =
    [("catch_up_ratio", "The rate of event consumption by the verifier (s⁻¹).")]
  metricsDocFor _ = []

