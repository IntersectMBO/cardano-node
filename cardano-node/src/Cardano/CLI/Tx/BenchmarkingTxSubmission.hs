{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Tx.BenchmarkingTxSubmission
  ( ROEnv(..)
  , RPCTxSubmission(..)
  , TraceBenchTxSubmit(..)
  , bulkSubmission
  ) where

import           Prelude
import           Cardano.Prelude (MonadIO, Word16, Seq, StateT,
                                  evalStateT, gets, isJust, lift, modify,
                                  toList, unless, when)

import           Control.Exception (assert)
import           Control.Monad.Class.MonadSTM (MonadSTM, LazyTMVar, LazyTVar,
                                               atomically, putTMVar, readTVar,
                                               retry, takeTMVar, tryTakeTMVar)
import           Control.Monad.Class.MonadTime (MonadTime(..), addTime, diffTime)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Time.Clock (DiffTime)

import           Control.Tracer (Tracer, traceWith)
import qualified Ouroboros.Consensus.Mempool as Mempool
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TxSubmit

-- | Bulk submisson of transactions.
--
--   This is intended to be used as a seperate process that interfaces
--   between some form of transaction generation mechanism
--   (e.g. benchmarking) and the local Ouroboros protocol peer. It has
--   configurable latency hiding (number of transactions to try to
--   keep ahead of actual submission) and has tracing that both
--   reports observables related to transaction submission and whether
--   the tx generation is able to keep up the pace.
--
--   For the details of the protocol description see
--   `TxSubmit.TxSubmission` and its associated instances
--   (e.g. `Message`).
bulkSubmission
  :: forall m blk txid tx .
     ( MonadIO m, MonadSTM m, MonadTime m, MonadTimer m
     , Ord txid
     , Mempool.ApplyTx blk
     , tx ~ Mempool.GenTx blk, txid ~ Mempool.GenTxId blk)
  => (ROEnv txid tx -> ROEnv txid tx)
  -- changes to default settings
  -> Tracer m (TraceBenchTxSubmit txid)
  -> LazyTVar m Bool
  -- Set to True to indicate subsystem should terminate
  -> LazyTMVar m [tx]
  -- non-empty list of transactions to be forwarded,
  -- empty list indicates terminating
  -> LazyTMVar m (RPCTxSubmission m txid tx)
  -- the RPC variable shared with
  -- `TxSubmit.TxSubmission` local peer
  -> m ()
bulkSubmission updEnv tr termVar txIn rpcIn = do
--    liftIO $ putStrLn "bulkSubmission__0"
  defaultRWEnv >>= evalStateT (go $ updEnv defaultROEnv)
 where
  go :: ROEnv txid tx -> StateT (RWEnv m txid tx) m ()
  go env = do
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go, env: " ++ show env
    shutdown <- gets (\RWEnv{terminating, inFlight, notYetSent} ->
                       terminating &&
                       Seq.null inFlight &&
                       Seq.null notYetSent)
    if shutdown
    then do -- terminating normally, inform local peer
      -- check if remote peer is waiting on us
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "TERMINATING normally"
      (opP, op) <- maybe (False, error "internal error")
                         (\x -> (True,snd x)) <$> gets availableOp
--        liftIO $ putStrLn $ "TERMINATING normally, opP: " ++ show opP
      lift . atomically $
        if opP
        then putTMVar op Nothing
        else do
          reqTxIds <- takeTMVar rpcIn -- no other message should occur as nothing is in flight
          case reqTxIds of
            RPCRequestTxIds _ resp -> putTMVar resp Nothing
            _ -> return ()
    else do -- process next interaction
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "go, process next interaction"
      go1 env

  go1 :: ROEnv txid tx -> StateT (RWEnv m txid tx) m ()
  go1 env = do
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, env: " ++ show env
    terminating' <- gets terminating
    notYetSent'  <- gets notYetSent
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, terminating': " ++ show terminating'

    -- perform the blocking inquiry
    (nowTerminating, newTxs, rpc) <- lift . atomically $ do
      -- just looking for the transition to terminating here
      term' <- if terminating'
               then pure False
               else readTVar termVar
      -- try get more txs if our target backlog permits
      txs' <- if Seq.length notYetSent' < targetBacklog env &&
                 not terminating'
              then tryTakeTMVar txIn
              else pure Nothing
      -- always willing to interact with local peer
      rpc' <- tryTakeTMVar rpcIn
      -- check there was some change to process
      unless (term' || isJust txs' || isJust rpc') retry
      -- treat empty input transaction list as equiv to
      -- terminating
      let (term,txs) =
            case txs' of
              Just x | null x
                       -> (True, Nothing)
              _        -> (term', txs')
      pure (term || terminating', txs, rpc')

    -- Update terminating, if needed 
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, update terminating, if needed"
    when (nowTerminating && not terminating') $ do
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, update terminating: True"
      modify (\x -> x {terminating = True})

    -- incorporate new transactions from the generator into
    -- NotYetSent
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, incorporate new transactions from the generator into NotYetSent"
    flip (maybe (pure ())) newTxs $ \xs -> do
      let newTxs' = map (\x -> (getTxId x, x, getTxSize x)) xs
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, newTxs' size: " ++ show (length newTxs')
      lift . traceWith tr . TraceBenchTxSubRecv $ map (\(a,_,_) -> a) newTxs'
      modify (\x -> x {notYetSent = notYetSent x Seq.>< Seq.fromList newTxs'})

    -- if remote peer is waiting on us and there is something to be
    -- sent: reply to the outstanding operation.
    opP <- isJust <$> gets availableOp
    haveStuffP <- (not . Seq.null) <$> gets notYetSent
    when (opP && haveStuffP) $ processOp env

    -- process any interaction with the local peer
    flip (maybe (pure ())) rpc $ processRPC env

    -- and recurse
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go, recurse"
    go env

  getTxId :: tx -> txid
  getTxId = Mempool.txId

  getTxSize :: tx -> TxSubmit.TxSizeInBytes
  getTxSize = Mempool.txSize

  processOp :: ROEnv txid tx -> StateT (RWEnv m txid tx) m ()
  processOp env = do
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, env: " ++ show env
    (window, op) <- (maybe (error "internal error")) id <$> gets availableOp
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, window: " ++ show window
    (send,store) <- Seq.splitAt window <$> gets notYetSent
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, send size: " ++ show (length send)
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, store size: " ++ show (length store)
    let send' = toList send
    checkRateLimiter
    lift . traceWith tr . TraceBenchTxSubStart $ [a | (a,_,_) <- send']
    lift . atomically $ putTMVar op (Just $ [(a,c) | (a,_,c) <-  send'])
    setRateLimiter env [ c | (_,_,c) <- send']
    modify (\x -> x { availableOp = Nothing
                    , inFlight    = inFlight x Seq.>< send
                    , notYetSent  = store })
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, done."

  processRPC :: ROEnv txid tx -> RPCTxSubmission m txid tx -> StateT (RWEnv m txid tx) m ()
  processRPC env =
    \case
      RPCRequestTxs  txids resp -> do
        -- lift . traceWith tr . TraceBenchTxSubDebug $ "processRPC, RPCRequestTxs"
        lift . traceWith tr $ TraceBenchTxSubServReq txids
        let txset = Set.fromList txids
        result <- Seq.filter (\(x,_,_) -> Set.member x txset) <$> gets inFlight
        lift . atomically $ putTMVar resp [b |(_,b,_) <- toList result]

      -- if blocking (deferred response) then the window has to be
      -- greater than zero, otherwise you can never respond (as the
      -- list has to be non empty)
      RPCRequestTxIds (acked, window) resp -> assert (window > 0) $ do
      -- prompt reply NOT expected (blocking)
        -- lift . traceWith tr . TraceBenchTxSubDebug $ "processRPC, RPCRequestTxIds"
        haveWork <- (not . Seq.null) <$> gets notYetSent
        if haveWork
        then do
          checkRateLimiter
          (send,store) <- Seq.splitAt (fromIntegral window) <$> gets notYetSent
          let send' = toList send
          lift . traceWith tr . TraceBenchTxSubStart $ [a | (a,_,_) <- send']
          lift . atomically $ putTMVar resp (Just $ [(a,c) | (a,_,c) <-  send'])
          setRateLimiter env [ c | (_,_,c) <- send']
          modify (\x -> x { inFlight    = inFlight x Seq.>< send
                            , notYetSent  = store })
          noteBusy
        else do
          noteIdle
          modify (\x -> x { availableOp = Just (fromIntegral window, resp)})

        -- deal with acked txs
        processAcks acked

      RPCRequestTxIdsPromptly (acked, window) resp -> do
      -- prompt reply expected 
        haveWork <- (not . Seq.null) <$> gets notYetSent
        if haveWork
        then do
          checkRateLimiter
          (send,store) <- Seq.splitAt (fromIntegral window) <$> gets notYetSent
          let send' = toList send
          lift . traceWith tr . TraceBenchTxSubStart $ [a | (a,_,_) <- send']
          lift . atomically $ putTMVar resp [(a,c) | (a,_,c) <-  send']
          setRateLimiter env [ c | (_,_,c) <- send']
          modify (\x -> x { inFlight    = inFlight x Seq.>< send
                          , notYetSent  = store })
          noteBusy
        else do
          noteIdle
          lift . atomically $ putTMVar resp []
          modify (\x -> x { availableOp = Nothing })

        -- deal with acked txs
        processAcks acked

  checkRateLimiter :: StateT (RWEnv m txid tx) m ()
  checkRateLimiter = do
    waitUntil <- gets proceedAfter
    sleepFor <- (\x -> waitUntil `diffTime` x) <$> lift getMonotonicTime
    when (sleepFor > 0) . lift $ do
      traceWith tr $ TraceBenchTxSubRateLimit sleepFor
      threadDelay sleepFor

  setRateLimiter
    :: ROEnv txid tx
    -> [TxSubmit.TxSizeInBytes]
    -> StateT (RWEnv m txid tx) m ()
  setRateLimiter env tls = do
    let txLimit   = (* fromIntegral (length tls)) <$> txNumServiceTime  env
        sizeLimit = (* fromIntegral (sum    tls)) <$> txSizeServiceTime env
        limit     = max txLimit sizeLimit
    flip (maybe (pure ())) limit $ \d -> do
      waitUntil <- (addTime d) <$> lift getMonotonicTime
      modify (\x -> x { proceedAfter = waitUntil })

  processAcks :: Word16 -> StateT (RWEnv m txid tx) m ()
  processAcks acked  =  when (acked > 0) $ do
    (done, left) <- (Seq.splitAt (fromIntegral acked)) <$> gets inFlight
    lift . traceWith tr $ TraceBenchTxSubServAck [a | (a,_,_) <- toList done]
    modify (\x -> x {inFlight = left})

  noteIdle :: StateT (RWEnv m txid tx) m ()
  noteIdle = do
--      liftIO $ putStrLn $ "noteIdle"
    wasBusy <- (== Busy) <$> gets activityState
    when wasBusy $ do
      lift . traceWith tr $ TraceBenchTxSubIdle
--        liftIO $ putStrLn $ "noteIdle, Idle!"
      modify (\x -> x { activityState = Idle})

  noteBusy :: StateT (RWEnv m txid tx) m ()
  noteBusy = do -- just can't get those memory write cycles out of my head
--      liftIO $ putStrLn $ "noteBusy"
    wasIdle <- (== Idle) <$> gets activityState
    when wasIdle $ do
--        liftIO $ putStrLn $ "noteBusy, Busy!"
      modify (\x -> x { activityState = Busy})

data ActivityState = Idle | Busy deriving (Eq)

-- | The readonly environment
data ROEnv txid tx = ROEnv
  { targetBacklog     :: Int -- ^ how many to try to keep in back pocket, >0
  , txNumServiceTime  :: Maybe DiffTime -- ^ seconds per tx
  , txSizeServiceTime :: Maybe DiffTime -- ^ seconds per tx octet
  } deriving Show

defaultROEnv :: ROEnv txid tx
defaultROEnv = ROEnv
  { targetBacklog     = 1
  , txNumServiceTime  = Nothing
  , txSizeServiceTime = Nothing
  }

data RWEnv m txid tx = RWEnv
  { terminating     :: Bool
  , activityState   :: ActivityState
  , proceedAfter    :: Time m
  , availableOp     :: Maybe (Int, LazyTMVar m (Maybe [(txid, TxSubmit.TxSizeInBytes)]))
  -- ^ the window and the response action
  , inFlight
  , notYetSent      :: Seq (txid, tx,  TxSubmit.TxSizeInBytes)
  }

defaultRWEnv :: MonadTime m => m (RWEnv m txid tx)
defaultRWEnv = do
  now <- getMonotonicTime
  pure $ RWEnv
    { terminating     = False
    , activityState   = Idle
    , proceedAfter    = now
    , availableOp     = Nothing
    , inFlight        = mempty
    , notYetSent      = mempty
    }

-- | RPC interaction with `TxSubmit.TxSubmission`
data RPCTxSubmission m txid tx
  = RPCRequestTxIdsPromptly (Word16, Word16) (LazyTMVar m [(txid, TxSubmit.TxSizeInBytes)])
  -- ^ Request contains the acknowledged number and the size of the
  --   open window. Response contains the list of transactions (that
  --   can be empty - see the `TxSubmit.TxSubmission` description of
  --   `TxSubmit.StBlockingStyle` for more details). A prompt response
  --   is expected.
  |  RPCRequestTxIds (Word16, Word16) (LazyTMVar m (Maybe [(txid, TxSubmit.TxSizeInBytes)]))
  -- ^ Request contains the acknowledged number and the size of the
  --   open window. Response contains the list of transactions (that
  --   can not be empty - see the `TxSubmit.TxSubmission` description
  --   of `TxSubmit.StBlockingStyle` for more details); `Nothing`
  --   indicates no more transaction submissions and a clean
  --   shutdown. A prompt response is not expected.
  | RPCRequestTxs [txid] (LazyTMVar m [tx])
  -- ^ Request contains the list of transaction identifiers which are
  --   returned in the response.

-- | Tracer
data TraceBenchTxSubmit txid
  = TraceBenchTxSubRecv [txid]
  -- ^ Received from generator.
  | TraceBenchTxSubStart [txid]
  -- ^ The @txid@ has been submitted to `TxSubmit.TxSubmission`
  --   protocol peer.
  | TraceBenchTxSubServReq [txid]
  -- ^ Request for @tx@ recieved from `TxSubmit.TxSubmission` protocol
  --   peer.
  | TraceBenchTxSubServAck [txid]
  -- ^ An ack (window moved over) received for these transactions.
  | TraceBenchTxSubIdle
  -- ^ Remote peer requested new transasctions but none were
  --   available, generator not keeping up?
  | TraceBenchTxSubRateLimit DiffTime
  -- ^ Rate limiter bit, this much delay inserted to keep within
  --   configured rate.
  | TraceBenchTxSubDebug String
  deriving (Show)
