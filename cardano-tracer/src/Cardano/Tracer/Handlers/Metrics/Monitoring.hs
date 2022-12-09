{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Monitoring
  ( runMonitoringServer
  ) where

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar,
                   tryReadTMVar)
import           Control.Monad (forM, void)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, Element, liftIO, set, (#), (#+))
import           System.Remote.Monitoring (forkServerWith, serverThreadId)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.SSL.Certs
import           Cardano.Tracer.Types

-- | 'ekg' package allows to run only one EKG server, to display only one web page
--   for particular EKG.Store. Since 'cardano-tracer' can be connected to any number
--   of nodes, we display their list on the first web page (the first 'Endpoint')
--   as a list of hrefs. After clicking on the particular href, the user will be
--   redirected to the monitoring web page (the second 'Endpoint') built by 'ekg' package.
--   This page will display the metrics received from that node.
--
--   If the user returns to the first web page and clicks to another node's href,
--   the EKG server will be restarted and the monitoring page will display the metrics
--   received from that node.
runMonitoringServer
  :: TracerEnv
  -> (Endpoint, Endpoint) -- ^ (web page with list of connected nodes, EKG web page).
  -> IO ()
runMonitoringServer tracerEnv (Endpoint listHost listPort, monitorEP) = do
  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.2
  (certFile, keyFile) <- placeDefaultSSLFiles tracerEnv
  UI.startGUI (config certFile keyFile) $ \window -> do
    void $ return window # set UI.title "EKG Monitoring Nodes"
    void $ mkPageBody window tracerEnv monitorEP
 where
  config cert key =
    UI.defaultConfig
      { UI.jsLog    = const $ return ()
      , UI.jsUseSSL =
          Just $ UI.ConfigSSL
            { UI.jsSSLBind = encodeUtf8 $ T.pack listHost
            , UI.jsSSLPort = fromIntegral listPort
            , UI.jsSSLCert = cert
            , UI.jsSSLKey  = key
            , UI.jsSSLChainCert = False
            }
      }

-- | We have to keep an id of the node as well as thread id of currently launched EKG server.
type CurrentEKGServer = TMVar (NodeId, ThreadId)

-- | The first web page contains only the list of hrefs
--   corresponding to currently connected nodes.
mkPageBody
  :: UI.Window
  -> TracerEnv
  -> Endpoint
  -> UI Element
mkPageBody window tracerEnv mEP@(Endpoint monitorHost monitorPort) = do
  nodes <- liftIO $ S.toList <$> readTVarIO teConnectedNodes
  nodesHrefs <-
    if null nodes
      then UI.string "There are no connected nodes yet"
      else do
        currentServer :: CurrentEKGServer <- liftIO newEmptyTMVarIO
        nodesLinks <-
          forM nodes $ \nodeId@(NodeId anId) -> do
            nodeLink <-
              UI.li #+
                [ UI.anchor # set UI.href ("http://" <> monitorHost <> ":" <> show monitorPort)
                            # set UI.target "_blank"
                            # set UI.title__ "Open EKG monitor page for this node"
                            # set UI.text (T.unpack anId)
                ]
            void $ UI.on UI.click nodeLink $ const $
              restartEKGServer tracerEnv nodeId mEP currentServer
            return $ UI.element nodeLink
        UI.ul #+ nodesLinks
  UI.getBody window #+ [ UI.element nodesHrefs ]
 where
  TracerEnv{teConnectedNodes} = tracerEnv

-- | After clicking on the node's href, the user will be redirected to the monitoring page
--   which is rendered by 'ekg' package. But before, we have to check if EKG server is
--   already launched, and if so, restart the server if needed.
restartEKGServer
  :: TracerEnv
  -> NodeId
  -> Endpoint
  -> CurrentEKGServer
  -> UI ()
restartEKGServer TracerEnv{teAcceptedMetrics} newNodeId
                 (Endpoint monitorHost monitorPort) currentServer = liftIO $ do
  metrics <- readTVarIO teAcceptedMetrics
  whenJust (metrics M.!? newNodeId) $ \(storeForSelectedNode, _) ->
    atomically (tryReadTMVar currentServer) >>= \case
      Just (_curNodeId, _sThread) ->
        -- TODO: Currently we cannot restart EKG server,
        -- please see https://github.com/tibbe/ekg/issues/87
        return ()
        -- unless (newNodeId == curNodeId) $ do
        --   killThread sThread
        --   runEKGAndSave storeForSelectedNode
      Nothing ->
        -- Current server wasn't stored yet, it's a first click on the href.
        runEKGAndSave storeForSelectedNode
 where
  runEKGAndSave store = do
    ekgServer <- forkServerWith store
                   (encodeUtf8 . T.pack $ monitorHost)
                   (fromIntegral monitorPort)
    atomically $ putTMVar currentServer (newNodeId, serverThreadId ekgServer)
