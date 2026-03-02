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


data TraceMessage = FormulaStartCheck {
                      formula :: Formula TemporalEvent Text
                      -- | For the info about this field, see the doc entry of `FormulaProgressDump`.
                    , index :: Word
                    }
                  | FormulaProgressDump {
                      eventsPerSecond :: Word,
                      catchupRatio    :: Double,
                      formula         :: Formula TemporalEvent Text,
                      -- | As we may have multiple formulas evolving over time concurrently,
                      --   we assign a unique index to each for disambiguation.
                      --   catchup_ratio_$INDEX is thus a set of metrics indexed over the finite set of indices.
                      index           :: Word
                    }
                  -- | Formula outcomes are split into (+) and (-) as we'd like to print them at distinct severity levels,
                  --   but severity level is indexed by namespace (= constructor name), hence
                  --   we can't store both outcomes in one constructor.
                  | FormulaPositiveOutcome { formula :: Formula TemporalEvent Text, index :: Word }
                  | FormulaNegativeOutcome {
                      formula   :: Formula TemporalEvent Text,
                      relevance :: Relevance TemporalEvent Text,
                      index :: Word
                  }

-- | Smart constructor.
formulaOutcome :: Formula TemporalEvent Text -> SatisfactionResult TemporalEvent Text -> Word -> TraceMessage
formulaOutcome formula Satisfied         idx = FormulaPositiveOutcome formula idx
formulaOutcome formula (Unsatisfied rel) idx = FormulaNegativeOutcome formula rel idx

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
  maybe ("<<Unexpected namespace " <> ns <> ">>") prettyTraceMessage (find (\ x -> x.tmsgNS == ns) msgs)

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
      "formula" .= String (prettyFormula formula Prec.Universe),
      "index" .= index
    ]
  forMachine _ FormulaProgressDump{..} = mconcat
    [
      "events_per_second" .= Number (fromIntegral eventsPerSecond),
      "catch_up_ratio" .= Number (realToFrac catchupRatio),
      "index" .= index
    ]
  forMachine _ FormulaPositiveOutcome{..} = mconcat
    [
      "formula" .= String (prettyFormula formula Prec.Universe),
      "index" .= index
    ]
  forMachine _ FormulaNegativeOutcome{..} = mconcat
    [
      "formula" .= String (prettyFormula formula Prec.Universe)
    ,
      "relevance" .= String (showT relevance)
    ,
      "index" .= index
    ]

  forHuman FormulaStartCheck{..} =
    "Starting satisfiability check on formula #" <> showT index <> ": " <> prettyFormula formula Prec.Universe
  forHuman FormulaProgressDump{..} =
         "\nevent/s: " <> showT eventsPerSecond <> "\n"
      <> "catch-up ratio: "
            <> showT catchupRatio
            <> "  // values above 1.0 <=> realtime"
            <> "\n"
      <> "formula: " <> prettyFormula formula Prec.Universe <> "\n"
      <> "index: " <> showT index
  forHuman FormulaPositiveOutcome{..} =
    prettySatisfactionResult formula Satisfied
  forHuman FormulaNegativeOutcome{..} =
    prettySatisfactionResult formula (Unsatisfied relevance)

  asMetrics FormulaStartCheck{} = []
  asMetrics (FormulaProgressDump {catchupRatio, index}) = [DoubleM ("catchup_ratio_" <> showT index) catchupRatio]
  asMetrics FormulaPositiveOutcome{} = []
  asMetrics FormulaNegativeOutcome{} = []


instance MetaTrace TraceMessage where
  allNamespaces =
    [
      Namespace [] ["FormulaStartCheck"]
    ,
      Namespace [] ["FormulaProgressDump"]
    ,
      Namespace [] ["FormulaOutcome"]
    ]

  namespaceFor FormulaStartCheck{}      = Namespace [] ["FormulaStartCheck"]
  namespaceFor FormulaProgressDump{}    = Namespace [] ["FormulaProgressDump"]
  namespaceFor FormulaPositiveOutcome{} = Namespace [] ["FormulaPositiveOutcome"]
  namespaceFor FormulaNegativeOutcome{} = Namespace [] ["FormulaNegativeOutcome"]

  severityFor (Namespace [] ["FormulaStartCheck"])       _ = Just Info
  severityFor (Namespace [] ["FormulaProgressDump"])     _ = Just Debug
  severityFor (Namespace [] ["FormulaPositiveOutcome"])  _ = Just Info
  severityFor (Namespace [] ["FormulaNegativeOutcome"])  _ = Just Notice
  severityFor _                                          _ = Nothing

  detailsFor _ _ =  Just DNormal

  documentFor (Namespace [] ["FormulaStartCheck"]) =
    Just "Formula satisfiability check has started."
  documentFor (Namespace [] ["FormulaProgressDump"]) =
    Just "Dump of formula satisfiability check progress."
  documentFor (Namespace [] ["FormulaPositiveOutcome"]) =
    Just "Formula evaluates to `true` against the given timeline of events."
  documentFor (Namespace [] ["FormulaNegativeOutcome"]) =
    Just "Formula evaluates to `false` against the given timeline of events."
  documentFor _ = Nothing

  metricsDocFor (Namespace [] ["FormulaProgressDump"]) =
    [("catch_up_ratio", "The rate of event consumption by the verifier (s⁻¹).")]
  metricsDocFor _ = []

