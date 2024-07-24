module Cardano.Tracer.Handlers.Notifications.Check
  ( checkCommonErrors
  ) where

import           Cardano.Logging (SeverityS (..))
import           Cardano.Tracer.Handlers.Notifications.Types
import           Cardano.Tracer.Handlers.Notifications.Utils
import           Cardano.Tracer.Handlers.State.TraceObjects
import           Cardano.Tracer.Types

checkCommonErrors
  :: NodeId
  -> TraceObjectInfo
  -> EventsQueues
  -> IO ()
checkCommonErrors nodeId (msg, sev, ts) eventsQueues =
  case sev of
    Warning   -> addNewEventTo EventWarnings
    Error     -> addNewEventTo EventErrors
    Critical  -> addNewEventTo EventCriticals
    Alert     -> addNewEventTo EventAlerts
    Emergency -> addNewEventTo EventEmergencies
    _         -> return ()
 where
  addNewEventTo eventGroup =
    addNewEvent eventsQueues eventGroup $ Event nodeId ts sev msg

