{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Queue
  ( readItems
  , writeLogObjectsToQueue
  , logObjectsFromReply
  ) where

import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue,
                                                 tryReadTBQueue, writeTBQueue)
import           Control.Monad (forM_, unless)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import           Trace.Forward.Protocol.Type

readItems
  :: ForwarderConfiguration lo -- ^ The forwarder configuration.
  -> TBQueue lo                -- ^ The queue we will read 'LogObject's from.
  -> Forwarder.TraceForwarder lo IO ()
readItems config@ForwarderConfiguration{..} loQueue =
  Forwarder.TraceForwarder
    { Forwarder.recvMsgNodeInfoRequest = do
        reply <- nodeBasicInfo            
        return (reply, readItems config loQueue)
    , Forwarder.recvMsgRequest = \blocking request@(GetLogObjects n) -> do
        actionOnRequest request
        replyList <-
          case blocking of
            TokBlocking -> do
              logObjects <- atomically $
                getLogObject n loQueue >>= \case
                  []     -> retry -- No 'LogObject's, just wait...
                  (x:xs) -> return $ x NE.:| xs
              return $ BlockingReply logObjects
            TokNonBlocking -> do
              logObjects <- atomically $ getLogObject n loQueue
              -- 'logObjects' may be empty, it's a normal case for non-blocking request.
              return $ NonBlockingReply logObjects  
        return (replyList, readItems config loQueue)
    , Forwarder.recvMsgDone = return ()
    }

getLogObject
  :: Word16
  -> TBQueue lo
  -> STM [lo]
getLogObject 0 _ = return []
getLogObject n loQueue =
  tryReadTBQueue loQueue >>= \case
    Just lo' ->
      (:) lo' <$> getLogObject (n - 1) loQueue
    Nothing ->
      -- It means that we want to read a new 'LogObject' from the queue,
      -- but it is already empty. The simplest case is just return all
      -- 'LogObject's we already read.
      getLogObject 0 loQueue

writeLogObjectsToQueue
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'LogObject's.
  -> TBQueue lo                    -- ^ The queue we want to write in.
  -> IO ()
writeLogObjectsToQueue reply loQueue =
  writeListToQueue $ logObjectsFromReply reply
 where
  writeListToQueue [] = return ()
  writeListToQueue l = atomically $
    forM_ l $ \lo' -> do
      itIsFull <- isFullTBQueue loQueue
      unless itIsFull $
        writeTBQueue loQueue lo'

logObjectsFromReply :: BlockingReplyList blocking lo -> [lo]
logObjectsFromReply reply = 
  case reply of
    BlockingReply loNEList  -> NE.toList loNEList
    NonBlockingReply loList -> loList
