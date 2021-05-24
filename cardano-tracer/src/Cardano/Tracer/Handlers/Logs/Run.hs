{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.Run
  ( runLogsHandler
  ) where

import           Control.Exception.Safe (IOException, withException)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, forConcurrently_,
                                           uninterruptibleCancel)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, tryReadTBQueue)
import           Control.Monad (forM_, forever, void)
import           Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           Data.Maybe (fromMaybe)
import           System.IO (hPutStrLn, stderr)

import           Cardano.BM.Data.LogItem (LogObject)

import           Trace.Forward.Protocol.Type (NodeInfoStore)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems, LogObjects, Metrics,
                                       NodeId, NodeName, getNodeName)
import           Cardano.Tracer.Handlers.Logs.File (writeLogObjectsToFile)
import           Cardano.Tracer.Handlers.Logs.Journal (writeLogObjectsToJournal)
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)

runLogsHandler
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runLogsHandler config acceptedItems = do
  rotThr <- async $ runLogsRotator config
  void . forever $ do
    itemsFromAllNodes <- HM.toList <$> readIORef acceptedItems
    forConcurrently_ itemsFromAllNodes $ handleItemsFromNode config
    threadDelay 1000000 -- Take 'LogObject's from the queue every second.
  uninterruptibleCancel rotThr

handleItemsFromNode
  :: TracerConfig
  -> (NodeId, (NodeInfoStore, LogObjects, Metrics))
  -> IO ()
handleItemsFromNode config (nodeId, (niStore, loQueue, _)) = do
  nodeName <- fromMaybe "" <$> getNodeName niStore
  atomically (getAllLogObjects loQueue) >>= writeLogObjects config nodeId nodeName

getAllLogObjects :: TBQueue lo -> STM [lo]
getAllLogObjects loQueue =
  tryReadTBQueue loQueue >>= \case
    Just lo' -> (:) lo' <$> getAllLogObjects loQueue
    Nothing  -> return []

writeLogObjects
  :: ToJSON a
  => TracerConfig
  -> NodeId
  -> NodeName
  -> [LogObject a]
  -> IO ()
writeLogObjects _ _ _ [] = return ()
writeLogObjects config nodeId nodeName logObjects =
  withException (writeLogObjects' config nodeId nodeName logObjects) $ \(e :: IOException) ->
    hPutStrLn stderr $ "Cannot write log objects to log files: " <> show e

writeLogObjects'
  :: ToJSON a
  => TracerConfig
  -> NodeId
  -> NodeName
  -> [LogObject a]
  -> IO ()
writeLogObjects' config nodeId nodeName logObjects =
  forM_ (logging config) $ \LoggingParams{..} ->
    case logMode of
      FileMode    -> writeLogObjectsToFile nodeId nodeName logRoot logFormat logObjects
      JournalMode -> writeLogObjectsToJournal nodeId nodeName logObjects
