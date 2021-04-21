{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Queue
  ( readLogObjectsFromQueue
  , writeLogObjectsToQueue
  , logObjectsFromReply
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue,
                                                 tryReadTBQueue, writeTBQueue)
import           Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import           Trace.Forward.Protocol.Type

readLogObjectsFromQueue
  :: ForwarderConfiguration lo -- ^ The forwarder configuration.
  -> TBQueue lo                -- ^ The queue we will read 'LogObject's from.
  -> Forwarder.TraceForwarder lo IO ()
readLogObjectsFromQueue config@ForwarderConfiguration {..} loQueue =
  Forwarder.TraceForwarder
    { Forwarder.recvMsgRequest = \blocking request@(GetLogObjects n) -> do
        actionOnRequest request
        logObjects <- atomically (getLogObject n loQueue)
        let replyList =
              case blocking of
                TokBlocking    -> BlockingReply $ NE.fromList logObjects -- TODO: logObjects can be empty!
                TokNonBlocking -> NonBlockingReply logObjects
        return (replyList, readLogObjectsFromQueue config loQueue)
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
      -- TODO: Discuss what we should do in this case!
      getLogObject 0 loQueue

writeLogObjectsToQueue
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'LogObject's.
  -> TBQueue lo                    -- ^ The queue we want to write in.
  -> IO ()
writeLogObjectsToQueue reply loQueue =
  writeListToQueue $ logObjectsFromReply reply
 where
  writeListToQueue [] = return ()
  writeListToQueue l =
    atomically $
      forM_ l $ \lo' -> do
        itIsFull <- isFullTBQueue loQueue
        if itIsFull
          then return ()
          else writeTBQueue loQueue lo'

logObjectsFromReply :: BlockingReplyList blocking lo -> [lo]
logObjectsFromReply reply = 
  case reply of
    BlockingReply loNEList  -> NE.toList loNEList
    NonBlockingReply loList -> loList
