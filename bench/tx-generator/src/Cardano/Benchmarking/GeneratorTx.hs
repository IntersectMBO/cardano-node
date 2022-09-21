{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Benchmarking.GeneratorTx
  ( AsyncBenchmarkControl
  , walletBenchmark
  , readSigningKey
  , waitBenchmark
  ) where

import           Cardano.Prelude
import           Prelude (String, id)

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Trans.Except.Extra (newExceptT)
import           "contra-tracer" Control.Tracer (Tracer, traceWith)
import qualified Data.Time.Clock as Clock

import qualified Data.List.NonEmpty as NE
import           Data.Text (pack)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                   addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.Configuration.NodeAddress

import           Cardano.Api hiding (txFee)

import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.SubmissionClient
import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.TpsThrottle
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Wallet (TxStream)
import           Cardano.TxGenerator.Types (NumberOfTxs, TPSRate, TxGenError (..))

readSigningKey :: SigningKeyFile -> ExceptT TxGenError IO (SigningKey PaymentKey)
readSigningKey =
  withExceptT ApiError . newExceptT . readKey . unSigningKeyFile
 where
  readKey :: FilePath -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentKey))
  readKey f = flip readFileTextEnvelopeAnyOf f
    [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
    , FromSomeType (AsSigningKey AsPaymentKey) id
    ]

type AsyncBenchmarkControl = (Async (), [Async ()], IO SubmissionSummary, IO ())

waitBenchmark :: Tracer IO (TraceBenchTxSubmit TxId) -> AsyncBenchmarkControl -> ExceptT TxGenError IO ()
waitBenchmark traceSubmit (feeder, workers, mkSummary, _) = liftIO $ do
  mapM_ waitCatch (feeder : workers)
  traceWith traceSubmit =<< TraceBenchTxSubSummary <$> mkSummary

lookupNodeAddress ::
  NodeAddress' NodeHostIPv4Address -> IO AddrInfo
lookupNodeAddress node = do
  (remoteAddr:_) <- getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
  return remoteAddr
 where
  targetNodeHost = show . unNodeHostIPv4Address $ naHostAddress node
  targetNodePort = show $ naPort node
  hints :: AddrInfo
  hints = defaultHints
    { addrFlags      = [AI_PASSIVE]
    , addrFamily     = AF_INET
    , addrSocketType = Stream
    , addrCanonName  = Nothing
    }

handleTxSubmissionClientError ::
     Tracer IO (TraceBenchTxSubmit TxId)
  -> Network.Socket.AddrInfo
  -> ReportRef
  -> SubmissionErrorPolicy
  -> SomeException
  -> IO ()
handleTxSubmissionClientError
  traceSubmit
  remoteAddr
  reportRef
  errorPolicy
  (SomeException err) = do
    submitThreadReport reportRef (Left errDesc)
    case errorPolicy of
      FailOnError -> throwIO err
      LogErrors   -> traceWith traceSubmit $
        TraceBenchTxSubError (pack errDesc)
   where
    errDesc = mconcat
      [ "Exception while talking to peer "
      , " (", show (addrAddress remoteAddr), "): "
      , show err]

walletBenchmark :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> String
  -> NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> AsType era
-- this is used in newTpsThrottle to limit the tx-count !
-- This should not be needed, the stream should do it itself (but it does not!)
  -> NumberOfTxs
-- This is TxStream is used in a wrong way !
-- It is used multithreaded here ! And every thread gets its own copy of the stream !!
-- This is a BUG it only works by coincidence !
-- Todo: Use the stream behind an MVar !
  -> TxStream IO era
  -> ExceptT TxGenError IO AsyncBenchmarkControl
walletBenchmark
  traceSubmit
  traceN2N
  connectClient
  threadName
  targets
  tpsRate
  errorPolicy
  _era
  count
  txSource
  = liftIO $ do
  traceDebug "******* Tx generator, phase 2: pay to recipients *******"

  remoteAddresses <- forM targets lookupNodeAddress
  let numTargets :: Natural = fromIntegral $ NE.length targets

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  tpsThrottle <- newTpsThrottle 32 count tpsRate

  reportRefs <- STM.atomically $ replicateM (fromIntegral numTargets) STM.newEmptyTMVar

  txStreamRef <- newMVar $ StreamActive txSource
  allAsyncs <- forM (zip reportRefs $ NE.toList remoteAddresses) $
    \(reportRef, remoteAddr) -> do
      let errorHandler = handleTxSubmissionClientError traceSubmit remoteAddr reportRef errorPolicy
          client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (txStreamSource txStreamRef tpsThrottle)
                     (submitSubmissionThreadStats reportRef)
      async $ handle errorHandler (connectClient remoteAddr client)

  tpsThrottleThread <- async $ do
    startSending tpsThrottle
    traceWith traceSubmit $ TraceBenchTxSubDebug "tpsLimitedFeeder : transmitting done"
    atomically $ sendStop tpsThrottle
    traceWith traceSubmit $ TraceBenchTxSubDebug "tpsLimitedFeeder : shutdown done"

  let tpsFeederShutdown = do
        cancel tpsThrottleThread
        liftIO $ atomically $ sendStop tpsThrottle

  return (tpsThrottleThread, allAsyncs, mkSubmissionSummary threadName startTime reportRefs, tpsFeederShutdown)
 where
  traceDebug :: String -> IO ()
  traceDebug =   traceWith traceSubmit . TraceBenchTxSubDebug
