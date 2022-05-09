{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
  , TxSource
  , ReportRef
  , walletTxSource
  , mkSubmissionSummary
  , submitThreadReport
  , submitSubmissionThreadStats
  ) where

import           Prelude (String, error)
import           Cardano.Prelude hiding (ByteString, atomically, retry, state, threadDelay)

import qualified Control.Concurrent.STM as STM

import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Clock

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Ouroboros.Network.Protocol.TxSubmission2.Type (SingBlockingStyle (..))

import           Cardano.Api

import           Cardano.Benchmarking.TpsThrottle
import           Cardano.Benchmarking.LogTypes
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

walletTxSource :: forall era. WalletScript era -> TpsThrottle -> TxSource era
walletTxSource walletScript tpsThrottle = Active $ worker walletScript
 where
  worker :: forall m blocking . MonadIO m => WalletScript era -> SingBlockingStyle blocking -> Req -> m (TxSource era, [Tx era])
  worker script blocking req = do
    (done, txCount) <- case blocking of
       SingBlocking -> liftIO $ consumeTxsBlocking tpsThrottle req
       SingNonBlocking -> liftIO $ consumeTxsNonBlocking tpsThrottle req
    (txList, newScript) <- liftIO $ unFold script txCount
    case done of
      Stop -> return (Exhausted, txList)
      Next -> return (Active $ worker newScript, txList)

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
