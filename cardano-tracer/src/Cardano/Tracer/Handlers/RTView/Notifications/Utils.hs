
module Cardano.Tracer.Handlers.RTView.Notifications.Utils
  ( addNewEvent
  , getNewEvents
  , initEventsQueues
  , updateNotificationsEvents
  , updateNotificationsPeriods
  ) where

import           Cardano.Tracer.Handlers.RTView.Notifications.Send
import           Cardano.Tracer.Handlers.RTView.Notifications.Settings
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (flushTBQueue, isFullTBQueue, newTBQueueIO,
                   writeTBQueue)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Monad.Extra (unlessM, whenJust)
import qualified Data.Map.Strict as M

initEventsQueues
  :: Maybe FilePath
  -> ConnectedNodesNames
  -> DataPointRequestors
  -> Lock
  -> IO EventsQueues
initEventsQueues rtvSD nodesNames dpReqs curDPLock = do
  emailSettings <- readSavedEmailSettings rtvSD

  newTVarIO . M.fromList =<<
    if incompleteEmailSettings emailSettings
    then pure []
    else do
      lastTime <- newTVarIO nullTime
      let mkEventQueue ident (evsS, evsP) = do
            evsQ <- newTBQueueIO 2000
            evsT <- mkTimer
              (makeAndSendNotification emailSettings nodesNames dpReqs curDPLock lastTime evsQ) evsS evsP
            pure (ident, (evsQ, evsT))

      settings <- readSavedEventsSettings rtvSD
      mapM (uncurry mkEventQueue)
        [ (EventWarnings,         evsWarnings    settings)
        , (EventErrors,           evsErrors      settings)
        , (EventCriticals,        evsCriticals   settings)
        , (EventAlerts,           evsAlerts      settings)
        , (EventEmergencies,      evsEmergencies settings)
        , (EventNodeDisconnected, evsNodeDisconnected settings)
        ]

getNewEvents
  :: EventsQueues
  -> EventGroup
  -> IO [Event]
getNewEvents eventsQueues eventGroup = do
  queues <- readTVarIO eventsQueues
  case M.lookup eventGroup queues of
    Nothing -> return []
    Just (queue, _) -> atomically $ flushTBQueue queue

addNewEvent
  :: EventsQueues
  -> EventGroup
  -> Event
  -> IO ()
addNewEvent eventsQueues eventGroup event = do
  queues <- readTVarIO eventsQueues
  whenJust (M.lookup eventGroup queues) $ \(queue, _) -> atomically $
    unlessM (isFullTBQueue queue) $ writeTBQueue queue event

-- | ..
updateNotificationsEvents
  :: EventsQueues
  -> EventGroup
  -> Bool
  -> IO ()
updateNotificationsEvents queues group True  = changeTimerState startTimer queues group
updateNotificationsEvents queues group False = changeTimerState stopTimer  queues group

updateNotificationsPeriods
  :: EventsQueues
  -> EventGroup
  -> PeriodInSec
  -> IO ()
updateNotificationsPeriods queues group period =
  changeTimerState (`setCallPeriod` period) queues group

changeTimerState
  :: (Timer -> IO ())
  -> EventsQueues
  -> EventGroup
  -> IO ()
changeTimerState setter eventsQueues eventGroup = do
  queues <- readTVarIO eventsQueues
  whenJust (M.lookup eventGroup queues) $ setter . snd
