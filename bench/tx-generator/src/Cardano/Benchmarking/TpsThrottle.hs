{-# LANGUAGE LambdaCase #-}
module Cardano.Benchmarking.TpsThrottle
where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM as STM
import           Control.Monad
import           Prelude

import qualified Data.Time.Clock as Clock

import           Cardano.Benchmarking.Types
import           Cardano.TxGenerator.Types (TPSRate)

data Step = Next | Stop
  deriving (Eq, Show)

data TpsThrottle = TpsThrottle {
    startSending :: IO ()
  , sendStop :: STM ()
  , receiveBlocking :: STM Step
  , receiveNonBlocking :: STM (Maybe Step)
  }

-- TVar state ::
-- empty ->  Block submission
-- Just 0 -> illegal state
-- Just n -> allow n transmissions ( n must be >0 )
-- Nothing -> teminate transmission

newTpsThrottle :: Int -> Int -> TPSRate -> IO TpsThrottle
newTpsThrottle buffersize count tpsRate = do
  var <- newEmptyTMVarIO
  return $ TpsThrottle {
      startSending = sendNTicks tpsRate buffersize count var
    , sendStop = putTMVar var Nothing
    , receiveBlocking = takeTMVar var >>= receiveAction var
    , receiveNonBlocking =
        (Just <$> (takeTMVar var >>= receiveAction var )) `orElse` return Nothing
  }

receiveAction :: TMVar (Maybe Int) -> Maybe Int -> STM Step
receiveAction var state = case state of
  Nothing -> do
    putTMVar var Nothing
    return Stop
  Just 1 -> return Next  -- leave var empty, i.e. block submission until sendNTicks unblocks
  Just n -> do
     -- decrease counter and let other threads transmit
    putTMVar var $ Just $ pred n
    return Next

sendNTicks :: TPSRate -> Int -> Int -> TMVar (Maybe Int) -> IO ()
sendNTicks rate buffersize count var = do
  now <- Clock.getCurrentTime
  worker count now 0
  where
    worker 0 _ _ = return ()
    worker n lastPreDelay lastDelay = do
      increaseWatermark
      now <- Clock.getCurrentTime
      let targetDelay = realToFrac $ 1.0 / rate
          loopCost = (now `Clock.diffUTCTime` lastPreDelay) - lastDelay
          delay = targetDelay - loopCost
      threadDelay . ceiling $ (realToFrac delay * 1000000.0 :: Double)
      worker (pred n) now delay
    -- increaseWatermark can retry/block if there are already buffersize ticks in the "queue"
    increaseWatermark = atomically $ do
      s <- tryTakeTMVar var
      case s of
        Nothing -> putTMVar var $ Just 1
        Just Nothing -> putTMVar var Nothing -- error "startTicks  unreachable state : Just Nothing"
        Just (Just n) -> if n == buffersize
          then retry -- block if buffer is full
          else putTMVar var $ Just $ succ n

consumeTxsBlocking :: TpsThrottle -> Req -> IO (Step, Int)
consumeTxsBlocking tpsThrottle req = go req 0
 where
  go :: Req -> Int -> IO (Step, Int)
  go 0 count = pure (Next, count)
  go n count = STM.atomically (receiveBlocking tpsThrottle) >>= \case
        Stop -> pure (Stop, count)
        Next -> go (n - 1) (succ count)

consumeTxsNonBlocking :: TpsThrottle -> Req -> IO (Step, Int)
consumeTxsNonBlocking tpsThrottle req
 = if req==0
      then pure (Next, 0)
      else do
        STM.atomically (receiveNonBlocking tpsThrottle) >>= \case
          Nothing -> pure (Next, 0)
          Just Stop -> pure (Stop, 0)
          Just Next -> pure (Next, 1)

test :: IO ()
test = do
  t <- newTpsThrottle 10 50 2
  _threadId <- startThrottle t
  threadDelay 5000000
  forM_ [1 .. 5] $ \i -> forkIO $ consumer t i
  forM_ [6 .. 7] $ \i -> forkIO $ consumer2 t i
  putStrLn "done"
 where
  startThrottle t = forkIO $ do
    startSending t
    putStrLn "startThrottle done"
    atomically $ sendStop t

  consumer :: TpsThrottle -> Int -> IO ()
  consumer t n = do
    s <- atomically $ receiveBlocking t
    print (n, s)
    if s == Next then consumer t n else putStrLn $ "Done " ++ show n

  consumer2 :: TpsThrottle -> Int -> IO ()
  consumer2 t n = do
    r <- atomically $ receiveNonBlocking t
    case r of
      Just s -> do
        print (n, s)
        if s == Next then consumer2 t n else putStrLn $ "Done " ++ show n
      Nothing -> do
        putStrLn $ "wait " ++ show n
        threadDelay 100000
        consumer2 t n
