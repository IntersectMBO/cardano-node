{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Statically configured alarm dispatch targets. Phase 1 (this sketch)
-- implements only 'ConsumerLog', matching the design doc's own staging
-- ("Add a log consumer to exercise filtering and dispatch"). Adding
-- @webhook@\/@email@ consumers later is one new constructor here and one new
-- case in 'dispatch' -- @email@ in particular can reuse the SMTP-sending
-- primitives in "Cardano.Tracer.Handlers.Notifications.Email" -- not
-- speculative machinery added now.
module Cardano.Tracer.Handlers.Alarms.Consumers
  ( AlarmConsumer (..)
  , consumerFromConfig
  , dispatch
  ) where

import           Cardano.Tracer.Configuration (AlarmsConsumerConfig (..))
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.MetaTrace (TracerTrace (..), Trace, traceWith)

import           Data.Text (Text)

data AlarmConsumer = ConsumerLog
  { clName    :: !Text
  , clEnabled :: !Bool
  , clFilter  :: !AlarmFilter
  }

consumerFromConfig :: AlarmsConsumerConfig -> AlarmConsumer
consumerFromConfig AlarmsConsumerLog{aclName, aclEnabled, aclFilter} = ConsumerLog
  { clName    = aclName
  , clEnabled = aclEnabled
  , clFilter  = maybe emptyAlarmFilter filterFromConfig aclFilter
  }

dispatch :: Trace IO TracerTrace -> AlarmConsumer -> AlarmEvent -> IO ()
dispatch tracer consumer@ConsumerLog{clEnabled, clFilter} ev
  | clEnabled && matchesFilter clFilter ev =
      traceWith tracer TracerAlarmDispatched
        { ttAlarmDispatchedConsumer = clName consumer
        , ttAlarmDispatchedSource   = unAlarmSource (source ev)
        , ttAlarmDispatchedRuleId   = unRuleId (ruleId ev)
        , ttAlarmDispatchedSeverity = severity ev
        , ttAlarmDispatchedSummary  = summary ev
        }
  | otherwise = pure ()
