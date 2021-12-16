{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Submission
  ( SubmissionParams(..)
  , SubmissionThreadReport
  , TxSendQueue
  , TxSource
  , ReportRef
  , legacyTxSource
  , walletTxSource
  , mkSubmissionSummary
  , submitThreadReport
  , submitSubmissionThreadStats
  , simpleTxFeeder
  , tpsLimitedTxFeeder
  , tpsLimitedTxFeederShutdown
  ) where

import           Prelude (String, error)
import           Cardano.Prelude hiding (ByteString, atomically, retry, state, threadDelay)

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)

import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Clock

import           Control.Tracer (Tracer, traceWith)

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Ouroboros.Network.Protocol.TxSubmission.Type (TokBlockingStyle (..))

import           Cardano.Api

import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.Types

import           Cardano.Benchmarking.GeneratorTx.SubmissionClient
import           Cardano.Benchmarking.Wallet

{-------------------------------------------------------------------------------
  Parametrisation & state
-------------------------------------------------------------------------------}

data SubmissionParams
  = SubmissionParams
      { spTps           :: !TPSRate
      , spTargets       :: !Natural
      }

type ReportRef = STM.TMVar (Either String SubmissionThreadReport)
type TxSendQueue era = TBQueue (Maybe (Tx era))


data SubmissionThreadReport
  = SubmissionThreadReport
      { strStats         :: !SubmissionThreadStats
      , strEndOfProtocol :: !UTCTime
      }

submitThreadReport
  :: MonadIO m
  => ReportRef
  -> Either String SubmissionThreadReport
  -> m ()
submitThreadReport ref report
 = liftIO $ STM.atomically $ STM.putTMVar ref report

submitSubmissionThreadStats
  :: MonadIO m
  => ReportRef
  -> SubmissionThreadStats
  -> m ()
submitSubmissionThreadStats reportRef strStats = do
  strEndOfProtocol <- liftIO Clock.getCurrentTime
  let report = SubmissionThreadReport{..}
  submitThreadReport reportRef (Right report)
  return ()

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

mkSubmissionSummary ::
     String
  -> UTCTime
  -> [ReportRef]
  -> IO SubmissionSummary
mkSubmissionSummary ssThreadName startTime reportsRefs
 = do
  results <- sequence (STM.atomically . STM.readTMVar <$> reportsRefs)
  let (failures, reports) = partitionEithers results
  now <- Clock.getCurrentTime
  let ssElapsed = Clock.diffUTCTime now startTime
      ssTxSent@(Sent sent) = sum $ stsSent . strStats <$> reports
      ssTxUnavailable = sum $ stsUnavailable . strStats <$> reports
      ssEffectiveTps = txDiffTimeTPS sent ssElapsed
      ssThreadwiseTps = threadReportTps <$> reports
      ssFailures = failures
  pure SubmissionSummary{..}
 where
  txDiffTimeTPS :: Int -> NominalDiffTime -> TPSRate
  txDiffTimeTPS n delta =
    TPSRate $ realToFrac $ fromIntegral n / delta

  threadReportTps :: SubmissionThreadReport -> TPSRate
  threadReportTps
    SubmissionThreadReport
      { strStats=SubmissionThreadStats{stsAcked=Ack ack}, strEndOfProtocol } =
        txDiffTimeTPS ack (Clock.diffUTCTime strEndOfProtocol startTime)

{-------------------------------------------------------------------------------
  Submission queue:  feeding and consumption
-------------------------------------------------------------------------------}
simpleTxFeeder :: forall m era .
     MonadIO m
  => Tracer m (TraceBenchTxSubmit TxId)
  -> Natural
  -> TxSendQueue era
  -> [Tx era]
  -> m ()
simpleTxFeeder tracer threads txSendQueue txs = do
  forM_ (zip txs [0..]) feedTx
  -- Issue the termination notifications.
  replicateM_ (fromIntegral threads) $
    liftIO $ STM.atomically $ STM.writeTBQueue txSendQueue Nothing
 where
  feedTx :: (Tx era, Int) -> m ()
  feedTx (tx, ix) = do
    liftIO $ STM.atomically $ STM.writeTBQueue txSendQueue (Just tx)
    traceWith tracer $ TraceBenchTxSubServFed [getTxId $ getTxBody tx] ix

tpsLimitedTxFeederShutdown ::
     Natural
  -> TxSendQueue era
  -> IO ()
tpsLimitedTxFeederShutdown threads txSendQueue
   = replicateM_ (fromIntegral threads)
       $ STM.atomically $ STM.writeTBQueue txSendQueue Nothing

tpsLimitedTxFeeder :: forall m era .
     MonadIO m
  => Tracer m (TraceBenchTxSubmit TxId)
  -> Natural
  -> TxSendQueue era
  -> TPSRate
  -> [Tx era] -> m ()
tpsLimitedTxFeeder tracer threads txSendQueue (TPSRate rate) txs = do
  -- It would be nice to catch an AsyncException here and do a clean shutdown.
  -- However this would require extra machineries because we are in MonadIO m not in IO ().
  -- TODO: Move everything to IO () and avoid problems from over-polymorphism.
  now <- liftIO Clock.getCurrentTime
  foldM_ feedTx (now, 0) (zip txs [0..])
  traceWith tracer $ TraceBenchTxSubDebug "tpsLimitedFeeder : transmitting done"
  liftIO $ tpsLimitedTxFeederShutdown threads txSendQueue
  traceWith tracer $ TraceBenchTxSubDebug "tpsLimitedFeeder : shutdown done"
 where

  feedTx :: (UTCTime, NominalDiffTime)
         -> (Tx era, Int)
         -> m (UTCTime, NominalDiffTime)
  feedTx (lastPreDelay, lastDelay) (tx, ix) = do
    liftIO . STM.atomically $ STM.writeTBQueue txSendQueue (Just tx)
    traceWith tracer $ TraceBenchTxSubServFed [] ix
    now <- liftIO Clock.getCurrentTime
    let targetDelay = realToFrac $ 1.0 / rate
        loopCost = (now `Clock.diffUTCTime` lastPreDelay) - lastDelay
        delay = targetDelay - loopCost
    liftIO . threadDelay . ceiling $ (realToFrac delay * 1000000.0 :: Double)
    pure (now, delay)

-- This is used in the two phase/ non wallet based tx-generator.
legacyTxSource :: forall era. TxSendQueue era -> TxSource era
legacyTxSource txSendQueue = Active worker
 where
  worker :: forall m blocking . MonadIO m => TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era])
  worker blocking req = do
    (done, txList) <- case blocking of
       TokBlocking -> consumeTxsBlocking req
       TokNonBlocking -> consumeTxsNonBlocking req
    if done
       then return (Exhausted, txList)
       else return (Active worker, txList)

  consumeTxsBlocking ::
       MonadIO m
    => Req -> m (Bool, [Tx era])
  consumeTxsBlocking req
    = liftIO . STM.atomically $ go req []
   where
    go :: Req -> [Tx era] -> STM (Bool, [Tx era])
    go 0 acc = pure (False, acc)
    go n acc = STM.readTBQueue txSendQueue >>=
      \case
        Nothing -> pure (True, acc)
        Just tx -> go (n - 1) (tx:acc)

  consumeTxsNonBlocking ::
       MonadIO m
    => Req -> m (Bool, [Tx era])
  consumeTxsNonBlocking req
    = liftIO . STM.atomically $
        if req==0 then pure (False, [])
          else do
            STM.tryReadTBQueue txSendQueue >>= \case
              Nothing -> pure (False, [])
              Just Nothing -> pure (True, [])
              Just (Just tx) -> pure (False, [tx])

walletTxSource :: forall era. WalletScript era -> TxSendQueue era -> TxSource era
walletTxSource walletScript txSendQueue = Active $ worker walletScript
 where
  worker :: forall m blocking . MonadIO m => WalletScript era -> TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era])
  worker script blocking req = do
    (done, dummyList :: [Tx era]) <- case blocking of
       TokBlocking -> consumeTxsBlocking req
       TokNonBlocking -> consumeTxsNonBlocking req
    (txList, newScript) <- liftIO $ unFold script $ length dummyList
    if done
       then return (Exhausted, txList)
       else return (Active $ worker newScript, txList)

  consumeTxsBlocking ::
       MonadIO m
    => Req -> m (Bool, [Tx era])
  consumeTxsBlocking req = go req []
   where
    go :: MonadIO m => Req -> [Tx era] -> m (Bool, [Tx era])
    go 0 acc = pure (False, acc)
    go n acc = liftIO (STM.atomically $ STM.readTBQueue txSendQueue) >>=
      \case
        Nothing -> pure (True, acc)
        Just tx -> go (n - 1) (tx : acc)

  consumeTxsNonBlocking ::
       MonadIO m
    => Req -> m (Bool, [Tx era])
  consumeTxsNonBlocking req
    = if req==0
         then pure (False, [])
         else do
            liftIO (STM.atomically $ STM.tryReadTBQueue txSendQueue) >>= \case
              Nothing -> pure (False, [])
              Just Nothing -> pure (True, [])
              Just (Just tx) -> pure (False, [tx])

  unFold :: WalletScript era -> Int -> IO ([Tx era], WalletScript era)
  unFold script 0 = return ([], script)
  unFold script n = do
    next <- runWalletScript script
    case next of
      Done -> error "unexpected WalletScript Done" --return ([], script)
      NextTx s tx -> do
        (l, out) <- unFold s $ pred n
        return (tx:l, out)
      Error err -> error err
