{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Redundant <$>" -}

module Cardano.Benchmarking.GeneratorTx
  ( AsyncBenchmarkControl
  , walletBenchmark
  -- , readSigningKey
  , waitBenchmark
  ) where

import           Cardano.Api hiding (txFee)

import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.SubmissionClient
import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.TpsThrottle
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Wallet (TxStream)
import           Cardano.Logging
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Prelude
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Types (NumberOfTxs, TPSRate, TxGenError (..))

import           Prelude (String)

import qualified Control.Concurrent.STM.TMVar as STM (newEmptyTMVar)
import qualified Control.Monad.STM as STM (atomically)
import qualified Data.List as List (unwords)
import qualified Data.List.NonEmpty as NE
import           Data.Text (pack)
import qualified Data.Time.Clock as Clock
import           Data.Tuple.Extra (secondM)
import           GHC.Conc as Conc (labelThread)

-- For some reason, stylish-haskell wants to delete this.
#if MIN_VERSION_base(4,18,0)
--- fromMaybe is imported via Cardano.Prelude
--- However, this configuration actually uses it.
--- import           Data.Maybe (fromMaybe)
import           GHC.Conc.Sync as Conc (threadLabel)
#endif
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                   addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)


waitBenchmark :: Trace IO (TraceBenchTxSubmit TxId) -> AsyncBenchmarkControl -> ExceptT TxGenError IO ()
waitBenchmark traceSubmit AsyncBenchmarkControl { .. } = liftIO $ do
  mapM_ waitCatch $ abcFeeder : abcWorkers
  traceWith traceSubmit . TraceBenchTxSubSummary =<< abcSummary

lookupNodeAddress :: NodeIPv4Address -> IO AddrInfo
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
     Trace IO (TraceBenchTxSubmit TxId)
  -> (String, Network.Socket.AddrInfo)
  -> ReportRef
  -> SubmissionErrorPolicy
  -> SomeException
  -> IO ()
handleTxSubmissionClientError
  traceSubmit
  (remoteName, remoteAddr)
  reportRef
  errorPolicy
  (SomeException err) = do
    tid   <- myThreadId
#if MIN_VERSION_base(4,18,0)
    label <- Conc.threadLabel tid
    let labelStr = fromMaybe "(unlabelled)" label
#else
    let labelStr = "(base version too low to examine thread labels)"
#endif
    let errDesc = List.unwords
                    [ "Thread"
                    , show tid
                    , labelStr
                    , "Exception while talking to peer "
                    , remoteName
                    , "(" ++ show (addrAddress remoteAddr) ++ "):"
                    , show err ]
    submitThreadReport reportRef (Left errDesc)
    case errorPolicy of
      FailOnError -> throwIO err
      LogErrors   -> traceWith traceSubmit $
        TraceBenchTxSubError (pack errDesc)

walletBenchmark :: forall era. IsShelleyBasedEra era
  => Trace IO (TraceBenchTxSubmit TxId)
  -> Trace IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> String
  -> NonEmpty NodeDescription
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

  let numTargets :: Natural = fromIntegral $ NE.length targets
      lookupTarget :: NodeDescription -> IO (String, AddrInfo)
      lookupTarget NodeDescription {..} = secondM lookupNodeAddress (ndName, ndAddr)
  remoteAddresses <- forM targets lookupTarget

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  tpsThrottle <- newTpsThrottle 32 count tpsRate

  txStreamRef <- newMVar $ StreamActive txSource

  reportRefs <- atomically do replicateM (fromIntegral numTargets) STM.newEmptyTMVar
  let asyncList = zip reportRefs $ NE.toList remoteAddresses
  abcWorkers <- forM asyncList \(reportRef, remoteInfo@(remoteName, remoteAddrInfo)) -> do
    let errorHandler = handleTxSubmissionClientError traceSubmit remoteInfo reportRef errorPolicy
        client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (txStreamSource txStreamRef tpsThrottle)
                     (submitSubmissionThreadStats reportRef)
        remoteAddrString = show $ addrAddress remoteAddrInfo
    asyncThread <- async do handle errorHandler $ connectClient remoteAddrInfo client
    let tid = asyncThreadId asyncThread
    Conc.labelThread tid $ "txSubmissionClient " ++ show tid ++
                            " servicing " ++ remoteName ++ " (" ++ remoteAddrString ++ ")"
    pure asyncThread

  abcFeeder <- async $ do
    startSending tpsThrottle
    traceWith traceSubmit $ TraceBenchTxSubDebug "tpsLimitedFeeder : transmitting done"
    STM.atomically $ sendStop tpsThrottle
    traceWith traceSubmit $ TraceBenchTxSubDebug "tpsLimitedFeeder : shutdown done"
  let tid = asyncThreadId abcFeeder
  labelThread tid $ "tpsThrottleThread " ++ show tid

  let abcShutdown = do
        cancel abcFeeder
        liftIO . STM.atomically $ sendStop tpsThrottle

  pure AsyncBenchmarkControl { abcSummary = mkSubmissionSummary threadName startTime reportRefs, .. }
 where
  traceDebug :: String -> IO ()
  traceDebug =   traceWith traceSubmit . TraceBenchTxSubDebug
