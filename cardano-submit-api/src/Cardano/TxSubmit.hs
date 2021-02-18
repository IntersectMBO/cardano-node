{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit
  ( module X
  , runTxSubmitWebapi
  ) where

import           Cardano.BM.Trace (Trace, logInfo)
import           Cardano.Prelude
import           Cardano.TxSubmit.CLI.Parsers as X
import           Cardano.TxSubmit.CLI.Types as X
import           Cardano.TxSubmit.Config as X
import           Cardano.TxSubmit.Metrics (registerMetricsServer)
import           Cardano.TxSubmit.Tx as X
import           Cardano.TxSubmit.Types as X
import           Cardano.TxSubmit.Util as X
import           Cardano.TxSubmit.Web as X

import qualified Cardano.BM.Setup as Logging
import qualified Cardano.BM.Trace as Logging
import qualified Control.Concurrent.Async as Async

runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
    tsnc <- readTxSubmitNodeConfig (unConfigFile $ tspConfigFile tsnp)
    trce <- mkTracer tsnc
    (metrics, metricsServer) <- registerMetricsServer
    txSubmitServer <- Async.async $
      runTxSubmitServer trce metrics tspWebserverConfig tspProtocol tspNetworkId tspSocketPath
    void $ Async.waitAnyCancel
      [ txSubmitServer
      , metricsServer
      ]
    logInfo trce "runTxSubmitWebapi: Async.waitAnyCancel returned"
  where
    TxSubmitNodeParams
      { tspProtocol
      , tspNetworkId
      , tspSocketPath
      , tspWebserverConfig
      } = tsnp

mkTracer :: TxSubmitNodeConfig -> IO (Trace IO Text)
mkTracer enc =
  if not (tscEnableLogging enc)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ tscLoggingConfig enc) "cardano-tx-submit"
