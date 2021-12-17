{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Benchmarking.GeneratorTx
  ( AsyncBenchmarkControl
  , TxGenError
  , walletBenchmark
  , readSigningKey
  , secureGenesisFund
  , waitBenchmark
  ) where

import           Cardano.Prelude
import           Prelude (String, error, id)

import qualified Control.Concurrent.STM as STM
import           Control.Monad (fail)
import           Control.Monad.Trans.Except.Extra (newExceptT)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.Time.Clock as Clock

import qualified Data.List.NonEmpty as NE
import           Data.Text (pack)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                   addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.Types

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)

import           Cardano.Api hiding (txFee)

import qualified Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.GeneratorTx.Error
import           Cardano.Benchmarking.GeneratorTx.Genesis
import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.SubmissionClient
import           Cardano.Benchmarking.GeneratorTx.Tx
import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Wallet (WalletScript)

import           Cardano.Ledger.Shelley.API (ShelleyGenesis)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

readSigningKey :: SigningKeyFile -> ExceptT TxGenError IO (SigningKey PaymentKey)
readSigningKey =
  withExceptT TxFileError . newExceptT . readKey . unSigningKeyFile
 where
  readKey :: FilePath -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentKey))
  readKey f = flip readFileTextEnvelopeAnyOf f
    [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
    , FromSomeType (AsSigningKey AsPaymentKey) id
    ]

secureGenesisFund :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> (TxInMode CardanoMode -> IO (SubmitResult (TxValidationErrorInMode CardanoMode)))
  -> NetworkId
  -> ShelleyGenesis StandardShelley
  -> Lovelace
  -> SlotNo
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ExceptT TxGenError IO Fund
secureGenesisFund submitTracer localSubmitTx networkId genesis txFee ttl key outAddr = do
  let (_inAddr, lovelace) = genesisFundForKey @ era networkId genesis key
      (tx, fund) =
         genesisExpenditure networkId key outAddr lovelace txFee ttl
  r <- liftIO $
    catches (localSubmitTx $ txInModeCardano tx)
      [ Handler $ \e@SomeException{} ->
          fail $ mconcat
            [ "Exception while moving genesis funds via local socket: "
            , show e
            ]]
  case r of
    SubmitSuccess ->
      liftIO . traceWith submitTracer . TraceBenchTxSubDebug
      $ mconcat
      [ "******* Funding secured ("
      , show $ fundTxIn fund, " -> ", show $ fundAdaValue fund
      , ")"]
    SubmitFail e -> fail $ show e
  return fund

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
  -> NumberOfTxs
  -> (FundSet.Target -> WalletScript era)
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
  walletScript
  = liftIO $ do
  traceDebug "******* Tx generator, phase 2: pay to recipients *******"

  remoteAddresses <- forM targets lookupNodeAddress
  let numTargets :: Natural = fromIntegral $ NE.length targets

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  txSendQueue <- STM.newTBQueueIO 32

  reportRefs <- STM.atomically $ replicateM (fromIntegral numTargets) STM.newEmptyTMVar

  allAsyncs <- forM (zip reportRefs $ NE.toList remoteAddresses) $
    \(reportRef, remoteAddr) -> do
      let errorHandler = handleTxSubmissionClientError traceSubmit remoteAddr reportRef errorPolicy
          client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (walletTxSource (walletScript (FundSet.Target $ show remoteAddr)) txSendQueue)
                     (submitSubmissionThreadStats reportRef)
      async $ handle errorHandler (connectClient remoteAddr client)

  tpsFeeder <- async $ tpsLimitedTxFeeder traceSubmit numTargets txSendQueue tpsRate
                        $ replicate (fromIntegral $ unNumberOfTxs count) (error "dummy transaction" :: Tx era)

  let tpsFeederShutdown = do
        cancel tpsFeeder
        liftIO $ tpsLimitedTxFeederShutdown numTargets txSendQueue

  return (tpsFeeder, allAsyncs, mkSubmissionSummary threadName startTime reportRefs, tpsFeederShutdown)
 where
  traceDebug :: String -> IO ()
  traceDebug =   traceWith traceSubmit . TraceBenchTxSubDebug
