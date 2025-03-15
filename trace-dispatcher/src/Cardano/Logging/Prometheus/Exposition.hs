module Cardano.Logging.Prometheus.Exposition
  ( MetricName
  , renderExpositionFromSample
  , renderExpositionFromSampleWith
  ) where

import           Data.Char
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HM
import           Data.List (find)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import           System.Metrics (Sample, Value (..))


type MetricName = Text


renderExpositionFromSample :: Bool -> Sample -> TL.Text
renderExpositionFromSample = renderExpositionFromSampleWith []

renderExpositionFromSampleWith
  :: [(MetricName, Builder)]
  -> Bool
  -> Sample
  -> TL.Text
renderExpositionFromSampleWith helpTextDict noSuffixes =
  TB.toLazyText . (<> buildEOF) . HM.foldlWithKey' buildMetric mempty
  where
    buildHelpText :: MetricName -> (Builder -> Builder)
    buildHelpText name = maybe
      (const mempty)
      (buildHelp . snd)
      (find ((`T.isInfixOf` name) . fst) helpTextDict)

    -- implements the metricsNoSuffix config option
    -- must strip all suffixes as per: trace-dispatcher/src/Cardano/Logging/Tracer/EKG.hs > ekgTracer > setIt
    stripSuffix :: MetricName -> MetricName
    stripSuffix
      | noSuffixes = \name -> fromMaybe name $ asum $ map (`T.stripSuffix` name) ["_int", "_counter", "_real"]
      | otherwise  = id

    prepareName :: MetricName -> MetricName
    prepareName =
        T.filter (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_')
      . T.replace " " "_"
      . T.replace "-" "_"
      . T.replace "." "_"

    -- the help annotation line
    buildHelp :: Builder -> Builder -> Builder
    buildHelp h n =
      TB.fromText "# HELP " <> n <> space <> h <> newline

    buildMetric :: TB.Builder -> MetricName -> Value -> TB.Builder
    buildMetric acc mName mValue =
      acc <> case mValue of
        Counter c -> annotate buildCounter <> buildVal space  (TB.decimal c)
        Gauge g   -> annotate buildGauge   <> buildVal space  (TB.decimal g)
        Label l
          | Just ('{', _) <- T.uncons l
                  -> annotate buildInfo    <> buildVal mempty (TB.fromText l)
          | otherwise
                  -> helpAnnotation        <> buildVal space  (TB.fromText l)
        _         -> mempty
      where
        helpAnnotation =
          buildHelpText mName buildName

        -- annotates a metric in the order TYPE, UNIT, HELP
        -- TODO: UNIT annotation
        annotate annType =
          buildTypeAnn annType <> helpAnnotation

        -- the metric name for exposition
        buildName =
          TB.fromText $ prepareName $ stripSuffix mName

        -- the type annotation line
        buildTypeAnn t =
          TB.fromText "# TYPE " <> buildName <> t <> newline

        -- the actual metric line, optional spacing after name, because of labels: 'metric_name{label_value="foo"} 1'
        buildVal spacing v =
          buildName <> spacing <> v <> newline

buildGauge, buildCounter, buildInfo, buildEOF, newline, space :: Builder
buildGauge    = TB.fromText " gauge"
buildCounter  = TB.fromText " counter"
buildInfo     = TB.fromText " info"
buildEOF      = TB.fromText "# EOF\n"
newline       = TB.singleton '\n'
space         = TB.singleton ' '
