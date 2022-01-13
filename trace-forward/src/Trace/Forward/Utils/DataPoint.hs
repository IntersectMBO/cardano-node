{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Utils.DataPoint
  ( DataPoint (..)
  , DataPointStore
  , DataPointRequestor (..)
  , initDataPointStore
  , initDataPointRequestor
  , writeToStore
  , readFromStore
  , askForDataPoints
  ) where

import           Control.Concurrent.STM (atomically, check, orElse)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Data.Aeson
import qualified Data.Map.Strict as M

import           Trace.Forward.Protocol.DataPoint.Forwarder
import           Trace.Forward.Protocol.DataPoint.Type

-- | Type wrapper for some value of type 'v'. The only reason we need this
--   wrapper is an ability to store different values in the same 'DataPointStore'.
--
--   Please note that when the acceptor application will read the value of type 'v'
--   from the store, this value is just as unstructured JSON, but not Haskell
--   value of type 'v'. That's why 'FromJSON' instance for type 'v' should be
--   available for the acceptor application, to decode unstructured JSON.
--
data DataPoint where
  DataPoint :: ToJSON v => v -> DataPoint

type DataPointStore = TVar (M.Map DataPointName DataPoint)

initDataPointStore :: IO DataPointStore
initDataPointStore = newTVarIO M.empty

-- | Write 'DataPoint' to the store.
writeToStore
  :: DataPointStore
  -> DataPointName
  -> DataPoint
  -> IO ()
writeToStore dpStore dpName dp = atomically $
  modifyTVar' dpStore $ \store ->
    if dpName `M.member` store
      then M.adjust (const dp) dpName store
      else M.insert dpName dp store

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
    , (\(DataPoint v) -> Just $ encode v) =<< M.lookup dpName store
    )

-- | Since 'DataPointForward' protocol does not assume the stream of requests/replies,
--   we use the 'TVar's to provide to acceptor's side an ability to ask 'DataPoint's
--   explicitly.
data DataPointRequestor = DataPointRequestor
  { -- | The "ask flag": we use it to notify that we want 'DataPoint's.
    askDataPoints   :: !(TVar Bool)
    -- | The names of 'DataPoint's we need.
  , dataPointsNames :: !(TVar [DataPointName])
    -- | The list of received 'DataPoint's' values.
    --   By default it's empty, but when 'DataPoint's
    --   are received they will be stored here.
  , dataPointsReply :: !(TMVar DataPointValues)
  }

initDataPointRequestor :: IO DataPointRequestor
initDataPointRequestor = DataPointRequestor
  <$> newTVarIO False
  <*> newTVarIO []
  <*> newEmptyTMVarIO

askForDataPoints
  :: DataPointRequestor
  -> [DataPointName]
  -> IO DataPointValues
askForDataPoints _ [] = return []
askForDataPoints DataPointRequestor{askDataPoints, dataPointsNames, dataPointsReply} dpNames = do
  atomically $ do
    modifyTVar' dataPointsNames $ const dpNames -- Fill the names of 'DataPoint's we need.
    modifyTVar' askDataPoints $ const True      -- Ask them! The flag for acceptor's part
                                                -- of the protocol, it's initiate the request.
  -- Since the acceptor's part of the protocol already sent the request,
  -- we are waiting for reply: currently 'dataPointsReply' is empty,
  -- so we are stuck on 'retry'.
  --
  -- Unfortunately, it's possible that 'dataPointsReply' won't be filled by 'DataPointValues',
  -- because of some error (for example, network problems). To prevent an infinite 'retry',
  -- we start a max timer in parallel.
  maxTimer <- registerDelay tenSeconds
  atomically $
    takeTMVar dataPointsReply -- If everything is OK, we'll have an answer earlier than 10 seconds.
    `orElse`
    (readTVar maxTimer >>= check >> return []) -- No later than after 10 seconds we return [].

-- | If 'retry' takes more than 10 seconds - it's definitely a problem.
tenSeconds :: Int
tenSeconds = 10 * 1000000
