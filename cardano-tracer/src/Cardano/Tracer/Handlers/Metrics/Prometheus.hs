{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Prelude hiding (head)

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM, forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
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
  renderListOfConnectedNodes = do
    nodes <- liftIO $ readTVarIO connectedNodes
    case S.toList nodes of
      []   -> writeText "There are no connected nodes yet."
      nIds -> blaze =<< liftIO (mkListOfHrefs nIds)

  mkListOfHrefs :: [NodeId] -> IO Html
  mkListOfHrefs nIds = do
    nodeHrefs <- forM nIds $ \(NodeId anId) -> do
      let anId' = T.unpack anId
      return $ a ! href (mkURL anId') $ toHtml anId'
    return $ mkPage nodeHrefs

  mkURL :: String -> AttributeValue
  mkURL anId' = fromString $
    "http://" <> host <> ":" <> show port <> "/" <> anId'

  mkPage :: [Html] -> Html
  mkPage hrefs = html $ do
    head $ title "Prometheus metrics"
    body $ ul $ mapM_ li hrefs

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
getMetricsFromNode (anId':_) acceptedMetrics = do
  metrics <- readTVarIO acceptedMetrics
  case metrics M.!? nodeId of
    Nothing ->
      return "No such a node!"
    Just (ekgStore, _) -> do
      allMetrics <- sampleAll ekgStore
      return . renderListOfMetrics . getListOfMetrics $ allMetrics
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
