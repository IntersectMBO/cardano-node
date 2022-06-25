{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Utils
  ( addNewEvent
  , getNewEvents
  , initEventsQueues
  , updateNotificationsEvents
  , updateNotificationsPeriods
  ) where

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (flushTBQueue, isFullTBQueue, newTBQueueIO,
                   writeTBQueue)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Monad.Extra (unlessM, whenJust)
import qualified Data.Map.Strict as M

import           Cardano.Tracer.Handlers.RTView.Notifications.Send
import           Cardano.Tracer.Handlers.RTView.Notifications.Settings
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

initEventsQueues
  :: DataPointRequestors
  -> Lock
  -> IO EventsQueues
initEventsQueues dpRequestors currentDPLock = do
  lastTime <- newTVarIO nullTime

  warnQ <- initEventsQueue
  errsQ <- initEventsQueue
  critQ <- initEventsQueue
  alrtQ <- initEventsQueue
  emrgQ <- initEventsQueue
  nodeDisconQ <- initEventsQueue

  settings <- readSavedEventsSettings
  let (warnS, warnP) = evsWarnings settings
      (errsS, errsP) = evsErrors settings
      (critS, critP) = evsCriticals settings
      (alrtS, alrtP) = evsAlerts settings
      (emrgS, emrgP) = evsEmergencies settings
      (nodeDisconS, nodeDisconP) = evsNodeDisconnected settings

  warnT <- mkTimer (makeAndSendNotification dpRequestors currentDPLock lastTime warnQ) warnS warnP
  errsT <- mkTimer (makeAndSendNotification dpRequestors currentDPLock lastTime errsQ) errsS errsP
  critT <- mkTimer (makeAndSendNotification dpRequestors currentDPLock lastTime critQ) critS critP
  alrtT <- mkTimer (makeAndSendNotification dpRequestors currentDPLock lastTime alrtQ) alrtS alrtP
  emrgT <- mkTimer (makeAndSendNotification dpRequestors currentDPLock lastTime emrgQ) emrgS emrgP
  nodeDisconT <- mkTimer (makeAndSendNotification dpRequestors currentDPLock lastTime nodeDisconQ)
                         nodeDisconS nodeDisconP

  newTVarIO $ M.fromList
    [ (EventWarnings,         (warnQ, warnT))
    , (EventErrors,           (errsQ, errsT))
    , (EventCriticals,        (critQ, critT))
    , (EventAlerts,           (alrtQ, alrtT))
    , (EventEmergencies,      (emrgQ, emrgT))
    , (EventNodeDisconnected, (nodeDisconQ, nodeDisconT))
    ]
 where
  initEventsQueue = newTBQueueIO 2000

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
