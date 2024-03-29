{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit
  ( runTxSubmitWebapi
  , opts
  ) where

import qualified Cardano.BM.Setup as Logging
import           Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.BM.Trace as Logging
import           Cardano.TxSubmit.CLI.Parsers (opts)
import           Cardano.TxSubmit.CLI.Types (ConfigFile (unConfigFile), TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Config (GenTxSubmitNodeConfig (..), ToggleLogging (..),
                   TxSubmitNodeConfig, readTxSubmitNodeConfig)
import           Cardano.TxSubmit.Metrics (registerMetricsServer)
import           Cardano.TxSubmit.Web (runTxSubmitServer)

import qualified Control.Concurrent.Async as Async
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text (Text)

runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
    tsnc <- readTxSubmitNodeConfig (unConfigFile $ tspConfigFile tsnp)
    trce <- mkTracer tsnc
    (metrics, metricsServer) <- registerMetricsServer (tspMetricsPort tsnp)
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
mkTracer enc = case tscToggleLogging enc of
  LoggingOn -> liftIO $ Logging.setupTrace (Right $ tscLoggingConfig enc) "cardano-tx-submit"
  LoggingOff -> pure Logging.nullTracer
