{- HLINT ignore "Use newtype instead of data" -}
module TraceVerifier.TraceMessage where

import           Cardano.Logging
import qualified Cardano.Logging.Types.TraceMessage as Envelop
import           Cardano.LTL.Lang.Formula           (Formula, PropValue (..),
                                                     Relevance)
import qualified Cardano.LTL.Lang.Formula.Prec      as Prec
import           Cardano.LTL.Pretty                 (prettyFormula)
import           Cardano.LTL.Satisfy                (SatisfactionResult (..))
import           Cardano.Trace.Feed                 (TemporalEvent (..))
import           Data.Aeson                         (Value (..), (.=))
import           Data.Aeson.Encode.Pretty
import           Data.List                          (find)
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Lazy                     (toStrict)
import           Data.Text.Lazy.Builder             (toLazyText)
import           TraceVerifier.Common               (extractProps)


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

prettyTemporalEvent :: TemporalEvent -> Text -> Text
prettyTemporalEvent (TemporalEvent _ msgs) ns =
  maybe undefined prettyTraceMessage (find (\ x -> x.tmsgNS == ns) msgs)

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
      Namespace [] ["TraceVerifier", "FormulaStartCheck"]
    ,
      Namespace [] ["TraceVerifier", "FormulaProgressDump"]
    ,
      Namespace [] ["TraceVerifier", "FormulaOutcome"]
    ]

  namespaceFor FormulaStartCheck{} = Namespace [] ["TraceVerifier", "FormulaStartCheck"]
  namespaceFor FormulaProgressDump{} = Namespace [] ["TraceVerifier", "FormulaProgressDump"]
  namespaceFor FormulaPositiveOutcome{} = Namespace [] ["TraceVerifier", "FormulaPositiveOutcome"]
  namespaceFor FormulaNegativeOutcome{} = Namespace [] ["TraceVerifier", "FormulaNegativeOutcome"]

  severityFor (Namespace [] ["TraceVerifier", "FormulaStartCheck"])       _ = Just Info
  severityFor (Namespace [] ["TraceVerifier", "FormulaProgressDump"])     _ = Just Debug
  severityFor (Namespace [] ["TraceVerifier", "FormulaPositiveOutcome"])  _ = Just Info
  severityFor (Namespace [] ["TraceVerifier", "FormulaNegativeOutcome"])  _ = Just Notice
  severityFor _                                                           _ = Nothing

  detailsFor _ _ =  Just DNormal

  documentFor (Namespace [] ["TraceVerifier", "FormulaStartCheck"]) =
    Just "Formula satisfiability check has started."
  documentFor (Namespace [] ["TraceVerifier", "FormulaProgressDump"]) =
    Just "Dump of formula satisfiability check progress."
  documentFor (Namespace [] ["TraceVerifier", "FormulaPositiveOutcome"]) =
    Just "Formula evaluates to `true` against the given timeline of events."
  documentFor (Namespace [] ["TraceVerifier", "FormulaNegativeOutcome"]) =
    Just "Formula evaluates to `false` against the given timeline of events."
  documentFor _ = Nothing

  metricsDocFor (Namespace [] ["TraceVerifier", "FormulaProgressDump"]) =
    [("catch_up_ratio", "The rate of event consumption by the verifier (s⁻¹).")]
  metricsDocFor _ = []

