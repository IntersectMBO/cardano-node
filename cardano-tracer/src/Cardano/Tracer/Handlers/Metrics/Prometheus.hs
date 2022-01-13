{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Prelude hiding (head)

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Snap.Blaze (blaze)
import           Snap.Core (Snap, getRequest, route, rqParams, writeText)
import           Snap.Http.Server (Config, ConfigLog (..), defaultConfig, setAccessLog,
                   setBind, setErrorLog, setPort, simpleHttpServe)
import           System.Metrics (Sample, Value (..), sampleAll)
import           System.Time.Extra (sleep)
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title)

import           Cardano.Tracer.Configuration (Endpoint (..))
import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes, NodeId (..))

-- | Runs simple HTTP server that listens host and port and returns
--   the list of currently connected nodes in such a format:
--
--   * tmp-forwarder.sock@0
--   * tmp-forwarder.sock@1
--   * tmp-forwarder.sock@2
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
  :: Endpoint
  -> ConnectedNodes
  -> AcceptedMetrics
  -> IO ()
runPrometheusServer (Endpoint host port) connectedNodes acceptedMetrics = forever $ do
  -- If everything is okay, the function 'simpleHttpServe' never returns.
  -- But if there is some problem, it never throws an exception, but just stops.
  -- So if it stopped - it will be re-started.
  simpleHttpServe config $
    route [ ("/",        renderListOfConnectedNodes)
          , ("/:nodeid", renderMetricsFromNode)
          ]
  sleep 1.0
 where
  config :: Config Snap ()
  config =
      setPort (fromIntegral port)
    . setBind (encodeUtf8 . T.pack $ host)
    . setAccessLog ConfigNoLog
    . setErrorLog ConfigNoLog
    $ defaultConfig

  renderListOfConnectedNodes :: Snap ()
  renderListOfConnectedNodes =
    liftIO (readTVarIO connectedNodes <&> S.toList) >>= \case
      []   -> writeText "There are no connected nodes yet."
      nIds -> blaze . mkPage . map mkHref $ nIds

  mkPage hrefs = html $ do
    head . title $ "Prometheus metrics"
    body . ul $ mapM_ li hrefs

  mkHref (NodeId anId) =
    a ! href (fromString $ "http://" <> host <> ":" <> show port <> "/" <> anId')
      $ toHtml anId'
   where
     anId' = T.unpack anId

  renderMetricsFromNode :: Snap ()
  renderMetricsFromNode = do
    reqParams <- rqParams <$> getRequest
    case M.lookup "nodeid" reqParams of
      Nothing   -> writeText "No such a node!"
      Just anId -> writeText =<< liftIO (getMetricsFromNode anId acceptedMetrics)

type MetricName  = Text
type MetricValue = Text
type MetricsList = [(MetricName, MetricValue)]

getMetricsFromNode
  :: [BS.ByteString]
  -> AcceptedMetrics
  -> IO Text
getMetricsFromNode [] _ = return "No such a node!"
getMetricsFromNode (anId':_) acceptedMetrics =
  readTVarIO acceptedMetrics <&> M.lookup nodeId >>= \case
    Nothing ->
      return "No such a node!"
    Just (ekgStore, _) ->
      sampleAll ekgStore <&> renderListOfMetrics . getListOfMetrics
 where
  nodeId = NodeId $ decodeUtf8 anId'

  getListOfMetrics :: Sample -> MetricsList
  getListOfMetrics = filter (not . T.null . fst) . map metricsWeNeed . HM.toList

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
