{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Queue
  ( readLogObjectsFromQueue
  , writeLogObjectsToQueue
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue,
                                                 tryReadTBQueue, writeTBQueue)
import           Control.Monad (forM_)
import           Data.Word (Word16)

import           Cardano.BM.Data.LogItem (LogObject)

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import           Trace.Forward.ReqResp (Request (..), Response (..))
import qualified Trace.Forward.Protocol.Forwarder as Forwarder

readLogObjectsFromQueue
  :: ForwarderConfiguration a -- ^ The forwarder configuration.
  -> TBQueue (LogObject a)    -- ^ The queue we will read 'LogObject's from.
  -> Forwarder.TraceForwarder Request (Response a) IO ()
readLogObjectsFromQueue config@ForwarderConfiguration {..} loQueue =
  Forwarder.TraceForwarder
    { Forwarder.recvMsgReq = \request@(GetLogObjects n) -> do
        actionOnRequest request
        logObjects <- atomically $ getLogObject n loQueue
        return ( ResponseLogObjects logObjects
               , readLogObjectsFromQueue config loQueue
               )
    , Forwarder.recvMsgDone = return ()
    }

getLogObject
  :: Word16
  -> TBQueue (LogObject a)
  -> STM [LogObject a]
getLogObject 0 _ = return []
getLogObject n loQueue =
  tryReadTBQueue loQueue >>= \case
    Just lo ->
      (:) lo <$> getLogObject (n - 1) loQueue
    Nothing ->
      -- It means that we want to read a new 'LogObject' from the queue,
      -- but it is already empty. The simplest case is just return all
      -- 'LogObject's we already read.
      -- TODO: Discuss what we should do in this case!
      getLogObject 0 loQueue

writeLogObjectsToQueue
  :: Response a             -- ^ The response with list of 'LogObject's.
  -> TBQueue (LogObject a)  -- ^ The queue we want to write in.
  -> IO ()
writeLogObjectsToQueue (ResponseLogObjects []) _ = return ()
writeLogObjectsToQueue (ResponseLogObjects logObjects) loQueue =
  atomically $
    forM_ logObjects $ \lo -> do
      itIsFull <- isFullTBQueue loQueue
      if itIsFull
        then return ()
        else writeTBQueue loQueue lo
