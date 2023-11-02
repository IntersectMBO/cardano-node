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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Submission
  ( StreamState (..)
  , SubmissionParams(..)
  , SubmissionThreadReport
  , TxSource
  , ReportRef
  , mkSubmissionSummary
  , submitThreadReport
  , submitSubmissionThreadStats
  , txStreamSource
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, retry, state, threadDelay)
import           Prelude (String, error)

import qualified Control.Concurrent.STM as STM

import qualified Streaming.Prelude as Streaming

import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Clock

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Ouroboros.Network.Protocol.TxSubmission2.Type (TokBlockingStyle (..))

import           Cardano.Api hiding (Active)
import           Cardano.TxGenerator.Types (TPSRate, TxGenError)

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.TpsThrottle
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

mkSubmissionSummary ::
     String
  -> UTCTime
  -> [ReportRef]
  -> IO SubmissionSummary
mkSubmissionSummary ssThreadName startTime reportsRefs
 = do
  results <- mapM (STM.atomically . STM.readTMVar) reportsRefs
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
    realToFrac $ fromIntegral n / delta

  threadReportTps :: SubmissionThreadReport -> TPSRate
  threadReportTps
    SubmissionThreadReport
      { strStats=SubmissionThreadStats{stsAcked=Ack ack}, strEndOfProtocol } =
        txDiffTimeTPS ack (Clock.diffUTCTime strEndOfProtocol startTime)

txStreamSource :: forall era. MVar (StreamState (TxStream IO era)) -> TpsThrottle -> TxSource era
txStreamSource streamRef tpsThrottle = Active worker
 where
  worker :: forall m blocking . MonadIO m => TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era])
  worker blocking req = do
    (done, txCount) <- case blocking of
       TokBlocking -> liftIO $ consumeTxsBlocking tpsThrottle req
       TokNonBlocking -> liftIO $ consumeTxsNonBlocking tpsThrottle req
    txList <- liftIO $ unFold txCount
    case done of
      Stop -> return (Exhausted, txList)
      Next -> return (Active worker, txList)

  unFold :: Int -> IO [Tx era]
  unFold 0 = return []
  unFold n = nextOnMVar streamRef >>= \case
    -- Node2node clients buffer a number x of TXs internally (x is determined by the node.)
    -- Therefore it is possible that the submission client requests TXs from an empty TxStream.
    -- In other words, it is not an error to request more TXs than there are in the TxStream.
    StreamEmpty -> return []
    StreamError err -> error $ show err
    StreamActive tx -> do
      l <- unFold $ pred n
      return $ tx:l

  nextOnMVar :: MVar (StreamState (TxStream IO era)) -> IO (StreamState (Tx era))
  nextOnMVar v = modifyMVar v $ \case
    StreamEmpty -> return (StreamEmpty, StreamEmpty)
    StreamError err -> return (StreamError err, StreamError err)
    StreamActive s -> update <$> Streaming.next s
   where
    update :: Either () (Either TxGenError (Tx era), TxStream IO era) -> (StreamState (TxStream IO era), StreamState (Tx era))
    update x = case x of
      Left () -> (StreamEmpty, StreamEmpty)
      Right (Right tx, t) -> (StreamActive t, StreamActive tx)
      Right (Left err, _) -> (StreamError err, StreamError err)

data StreamState x
  = StreamEmpty
  | StreamError TxGenError
  | StreamActive x
