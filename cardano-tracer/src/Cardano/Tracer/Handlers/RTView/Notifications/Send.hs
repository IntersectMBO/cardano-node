{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Send
  ( makeAndSendNotification
  ) where

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (flushTBQueue)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forM, unless, void)
import           Data.List (nub)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           Cardano.Node.Startup (NodeInfo (..))

import           Cardano.Tracer.Handlers.RTView.Notifications.Email
import           Cardano.Tracer.Handlers.RTView.Notifications.Settings
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

makeAndSendNotification
  :: DataPointRequestors
  -> Lock
  -> TVar UTCTime
  -> EventsQueue
  -> IO ()
makeAndSendNotification dpRequestors currentDPLock lastTime eventsQueue = do
  emailSettings <- readSavedEmailSettings
  unless (incompleteEmailSettings emailSettings) $ do
    events <- atomically $ nub <$> flushTBQueue eventsQueue
    let (nodeIds, tss) = unzip $ nub [(nodeId, ts) | Event nodeId ts _ _ <- events]
    unless (null nodeIds) $ do
      nodeNames <-
        forM nodeIds $ \nodeId@(NodeId anId) ->
          askDataPoint dpRequestors currentDPLock nodeId "NodeInfo" >>= \case
            Nothing -> return anId
            Just ni -> return $ niName ni
      lastEventTime <- readTVarIO lastTime
      let onlyNewEvents = filter (\(Event _ ts _ _) -> ts > lastEventTime) events
      sendNotification emailSettings onlyNewEvents $ zip nodeIds nodeNames
      updateLastTime $ maximum tss
 where
  updateLastTime = atomically . modifyTVar' lastTime . const

sendNotification
  :: EmailSettings
  -> [Event]
  -> [(NodeId, Text)]
  -> IO ()
sendNotification _ [] _ = return ()
sendNotification emailSettings newEvents nodeIdsWithNames =
  void $ createAndSendEmail emailSettings body
 where
  body = preface <> events

  preface = T.intercalate nl
    [ "This is a notification from Cardano RTView service."
    , ""
    , "The following " <> (if onlyOne then "event" else "events") <> " occurred:"
    , ""
    ]

  events = T.intercalate nl
    [ "[" <> formatTS ts <> "] [" <> getNodeName nodeId <> "] [" <> showT sev <> "] [" <> showT msg <> "]"
    | Event nodeId ts sev msg <- newEvents
    ]

  onlyOne = length newEvents == 1

  formatTS = T.pack . formatTime defaultTimeLocale "%F %T %Z"

  getNodeName nodeId@(NodeId anId) =
    fromMaybe anId $ lookup nodeId nodeIdsWithNames
