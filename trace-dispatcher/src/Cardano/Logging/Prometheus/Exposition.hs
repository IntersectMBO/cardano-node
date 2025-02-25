module Cardano.Logging.Prometheus.Exposition
  ( MetricName
  , renderExpositionFromSample
  , renderExpositionFromSampleWith
  ) where

import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.List (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import           System.Metrics (Sample, Value (..))


type MetricName = Text


renderExpositionFromSample :: Sample -> TL.Text
renderExpositionFromSample = renderExpositionFromSampleWith Nothing []

renderExpositionFromSampleWith
  :: Maybe (Map MetricName MetricName)
  -> [(MetricName, Builder)]
  -> Sample
  -> TL.Text
renderExpositionFromSampleWith renameMap helpTextDict =
  TB.toLazyText . (`mappend` buildEOF) . HM.foldlWithKey' buildMetric mempty
  where
    buildHelpText :: MetricName -> (Builder -> Builder)
    buildHelpText name = maybe
      (const mempty)
      (buildHelp . snd)
      (find ((`T.isInfixOf` name) . fst) helpTextDict)

    -- implements the metricsComp config option
    replaceName :: MetricName -> MetricName
    replaceName =
      case renameMap of
        Nothing   -> Prelude.id
        Just mmap -> \name -> M.findWithDefault name name mmap

    prepareName :: MetricName -> MetricName
    prepareName =
        T.filter (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_')
      . T.replace " " "_"
      . T.replace "-" "_"
      . T.replace "." "_"

    -- the help annotation line
    buildHelp :: Builder -> Builder -> Builder
    buildHelp h n =
      TB.fromText "# HELP " `mappend` (n `mappend` (space `mappend` (h `mappend` newline)))

    buildMetric :: TB.Builder -> MetricName -> Value -> TB.Builder
    buildMetric acc mName mValue =
      acc `mappend` case mValue of
        Counter c -> annotate buildCounter `mappend` buildVal space  (TB.decimal c)
        Gauge g   -> annotate buildGauge   `mappend` buildVal space  (TB.decimal g)
        Label l
          | Just ('{', _) <- T.uncons l
                  -> annotate buildInfo    `mappend` buildVal mempty (TB.fromText l)
          | otherwise
                  -> helpAnnotation        `mappend` buildVal space  (TB.fromText l)
        _         ->                                 mempty
      where
        helpAnnotation = buildHelpText mName buildName

        -- annotates a metric in the order TYPE, UNIT, HELP
        -- TODO: UNIT annotation
        annotate annType =
          buildTypeAnn annType `mappend` helpAnnotation

        -- the name for exposition
        buildName = TB.fromText $ prepareName $ replaceName mName

        -- the type annotation line
        buildTypeAnn t =
          TB.fromText "# TYPE " `mappend` (buildName `mappend` (t `mappend` newline))

        -- the actual metric line, optional spacing after name, because of labels: 'metric_name{label_value="foo"} 1'
        buildVal spacing v =
          buildName `mappend` (spacing `mappend` (v `mappend` newline))

buildGauge, buildCounter, buildInfo, buildEOF, newline, space :: Builder
buildGauge    = TB.fromText " gauge"
buildCounter  = TB.fromText " counter"
buildInfo     = TB.fromText " info"
buildEOF      = TB.fromText "# EOF\n"
newline       = TB.singleton '\n'
space         = TB.singleton ' '
