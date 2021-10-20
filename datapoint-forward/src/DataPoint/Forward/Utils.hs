{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module DataPoint.Forward.Utils
  ( DataPoint (..)
  , DataPointStore
  , DataPointAsker (..)
  , initDataPointStore
  , initDataPointAsker
  , writeToStore
  , readFromStore
  , askForDataPoints
  , runActionInLoop
  ) where

import           Control.Concurrent.STM (atomically, check)
import           Control.Concurrent.STM.TVar
import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import           Control.Tracer (showTracing, stdoutTracer, traceWith)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           System.Time.Extra (sleep)

import           DataPoint.Forward.Configuration
import           DataPoint.Forward.Protocol.Forwarder
import           DataPoint.Forward.Protocol.Type

-- | Type wrapper for some value of type 'v'. The only reason we need this
--   wrapper is an ability to store different values in the same 'DataPointStore'.
data DataPoint where
  DataPoint :: ToJSON v => v -> DataPoint

type DataPointStore = TVar (HM.HashMap DataPointName DataPoint)

initDataPointStore :: IO DataPointStore
initDataPointStore = newTVarIO HM.empty

-- | Write 'DataPoint' to the store.
writeToStore
  :: DataPointStore
  -> DataPointName
  -> DataPoint
  -> IO ()
writeToStore dpStore dpName dp = atomically $
  modifyTVar' dpStore $ \store ->
    if dpName `HM.member` store
      then HM.adjust (const dp) dpName store
      else HM.insert dpName dp store

-- | Read 'DataPoint's from the store. Please note that we don't care what's
--   inside of 'DataPoint', we just know it can be encoded to JSON.
readFromStore
  :: DataPointStore
  -> DataPointForwarder IO ()
readFromStore dpStore =
  DataPointForwarder
    { recvMsgDataPointsRequest = \dpNames -> do
        store <- readTVarIO dpStore
        let replyList = map (lookupDataPoint store) dpNames
        return (replyList, readFromStore dpStore)
    , recvMsgDone = return ()
    }
 where
  lookupDataPoint store dpName =
    ( dpName
    , (\(DataPoint v) -> Just $ encode v) =<< HM.lookup dpName store
    )

-- | Since 'DataPointForward' protocol does not assume the stream of requests/replies,
--   we use the 'TVar's to provide to acceptor's side an ability to ask 'DataPoint's
--   explicitly.
data DataPointAsker = DataPointAsker
  { -- | The "ask flag": we use it to notify that we want 'DataPoint's.
    askDataPoints     :: !(TVar Bool)
    -- | The names of 'DataPoint's we need.
  , dataPointsNames   :: !(TVar [DataPointName])
    -- | The "ready flag": we use it to notify that 'DataPoint's was received.
  , dataPointsAreHere :: !(TVar Bool)
    -- | The list of received 'DataPoint's' values.
  , dataPointsReply   :: !(TVar DataPointValues)
  }

initDataPointAsker :: IO DataPointAsker
initDataPointAsker = DataPointAsker
  <$> newTVarIO False
  <*> newTVarIO []
  <*> newTVarIO False
  <*> newTVarIO []

askForDataPoints
  :: DataPointAsker
  -> [DataPointName]
  -> IO DataPointValues
askForDataPoints _ [] = return []
askForDataPoints DataPointAsker{askDataPoints, dataPointsNames, dataPointsAreHere, dataPointsReply} dpNames = do
  atomically $ do
    modifyTVar' dataPointsNames $ const dpNames -- Fill the names of 'DataPoint's we need.
    modifyTVar' askDataPoints $ const True      -- Ask them! The flag for acceptor's part of the protocol.
    modifyTVar' dataPointsAreHere $ const False -- There is no answer yet.
  -- Check the "ready flag" until it's True.
  atomically $ readTVar dataPointsAreHere >>= check
  -- Return the list of 'DataPoint's' values.
  readTVarIO dataPointsReply

-- | Run monadic action in a loop. If there's an exception, it will re-run
--   the action again, after pause that grows.
runActionInLoop
  :: IO ()
  -> HowToConnect
  -> Word
  -> IO ()
runActionInLoop action endpoint prevDelay =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      logTrace $ "datapoint-forward, connection with " <> show endpoint <> " failed: " <> show e
      sleep $ fromIntegral currentDelay
      runActionInLoop action endpoint currentDelay
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e

  logTrace = traceWith $ showTracing stdoutTracer

  currentDelay =
    if prevDelay < 60
      then prevDelay * 2
      else 60 -- After we reached 60+ secs delay, repeat an attempt every minute.
