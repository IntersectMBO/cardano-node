{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Prelude hiding (head)

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Bimap as BM
import           Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           System.Metrics (Sample, Value (..), sampleAll)
import           System.Time.Extra (sleep)
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title)

import           Snap.Blaze (blaze)
import           Snap.Core (Snap, getRequest, route, rqParams, writeText)
import           Snap.Http.Server (Config, ConfigLog (..), defaultConfig, setAccessLog, setBind,
                   setErrorLog, setPort, simpleHttpServe)

-- | Runs simple HTTP server that listens host and port and returns
--   the list of currently connected nodes in such a format:
--
--   * relay-1
--   * relay-2
--   * core-1
--
--  where 'relay-1', 'relay-2' and 'core-1' are nodes' names.
--
--  Each of list items is a href. By clicking on it, the user will be
--  redirected to the page with the list of metrics received from that node,
--  in such a format:
--
--  rts_gc_par_tot_bytes_copied 0
--  rts_gc_num_gcs 17
--  rts_gc_max_bytes_slop 15888
--  rts_gc_bytes_copied 165952
--  ekg_server_timestamp_ms 1639569439623
--
runPrometheusServer
  :: TracerEnv
  -> Endpoint
  -> IO ()
runPrometheusServer tracerEnv (Endpoint host port) = forever $ do
  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.1
  -- If everything is okay, the function 'simpleHttpServe' never returns.
  -- But if there is some problem, it never throws an exception, but just stops.
  -- So if it stopped - it will be re-started.
  simpleHttpServe config $
    route [ ("/",          renderListOfConnectedNodes)
          , ("/:nodename", renderMetricsFromNode)
          ]
  sleep 1.0
 where
  TracerEnv{teConnectedNodesNames, teAcceptedMetrics} = tracerEnv

  config :: Config Snap ()
  config =
      setPort (fromIntegral port)
    . setBind (encodeUtf8 . T.pack $ host)
    . setAccessLog ConfigNoLog
    . setErrorLog ConfigNoLog
    $ defaultConfig

  renderListOfConnectedNodes :: Snap ()
  renderListOfConnectedNodes = do
    nIdsWithNames <- liftIO $ readTVarIO teConnectedNodesNames
    if BM.null nIdsWithNames
      then writeText "There are no connected nodes yet."
      else blaze . mkPage . map mkHref $ BM.toList nIdsWithNames

  mkHref (_, nodeName) =
    a ! href (fromString $ "http://" <> host <> ":" <> show port <> "/" <> nodeName')
      $ toHtml nodeName'
   where
    nodeName' = T.unpack nodeName

  mkPage hrefs = html $ do
    head . title $ "Prometheus metrics"
    body . ul $ mapM_ li hrefs

  renderMetricsFromNode :: Snap ()
  renderMetricsFromNode = do
    reqParams <- rqParams <$> getRequest
    case M.lookup "nodename" reqParams of
      Just [nodeName] -> do
        liftIO (askNodeId tracerEnv $ decodeUtf8 nodeName) >>= \case
          Nothing -> writeText "No such a node!"
          Just anId -> writeText =<< liftIO (getMetricsFromNode tracerEnv anId teAcceptedMetrics)
      _ -> writeText "No such a node!"

type MetricName  = Text
type MetricValue = Text
type MetricsList = [(MetricName, MetricValue)]

getMetricsFromNode
  :: TracerEnv
  -> NodeId
  -> AcceptedMetrics
  -> IO Text
getMetricsFromNode tracerEnv nodeId acceptedMetrics =
  readTVarIO acceptedMetrics >>=
    (\case
        Nothing ->
          return "No such a node!"
        Just (ekgStore, _) ->
          sampleAll ekgStore <&> renderListOfMetrics . getListOfMetrics
    ) . M.lookup nodeId
 where
  getListOfMetrics :: Sample -> MetricsList
  getListOfMetrics =
      metricsCompatibility
      . filter (not . T.null . fst)
      . map metricsWeNeed
      . HM.toList

  metricsWeNeed (mName, mValue) =
    case mValue of
      Counter c -> (mName, T.pack $ show c)
      Gauge g   -> (mName, T.pack $ show g)
      Label l   -> (mName, l)
      _         -> ("",    "") -- 'ekg-forward' doesn't support 'Distribution' yet.

  renderListOfMetrics :: MetricsList -> Text
  renderListOfMetrics [] = "No metrics were received from this node."
  renderListOfMetrics mList = T.intercalate "\n" $
    map (\(mName, mValue) -> prepareName mName <> " " <> mValue) mList

  prepareName =
      T.filter (`elem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']))
    . T.replace " " "_"
    . T.replace "-" "_"
    . T.replace "." "_"

  metricsCompatibility :: MetricsList -> MetricsList
  metricsCompatibility metricsList =
      case metricsComp (teConfig tracerEnv) of
        Nothing -> metricsList
        Just mmap -> foldl  (\ accu p'@(mn,mv) -> case M.lookup mn mmap of
                                                Nothing -> p' : accu
                                                Just rep -> p' : (rep,mv) : accu)
                            []
                            metricsList

