module Cardano.TxSubmit.Tracing.Message (MetricAction(..), Message(..)) where

import           Cardano.Logging.Types

import           Data.Text (Text)

data MetricAction = MetricActionNone

-- TODO: (@russoul) Evaluate the Message structure.
data Message = Message SeverityS Text MetricAction

-- TODO: (@russoul) Implement this.
instance LogFormatting Message where
  forMachine _lvl _x = _
  forHuman _x = _

-- TODO: @russoul
instance MetaTrace Message where
  namespaceFor x = Namespace [] [] -- TODO @russoul
  severityFor x Nothing = _ -- TODO @russoul why severityFor doesn't always provide a message?
  severityFor x (Just (Message s _ _)) = Just s
  privacyFor = _
  detailsFor = _
  documentFor = _
  metricsDocFor = _
  allNamespaces = _

