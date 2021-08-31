{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.WebServer
  ( runWebServer
  ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO)
import           Control.Monad (forM_, void)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (liftIO, on, set, (#), (#+))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own (ownCSS)
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma (bulmaCSS)
--import           Cardano.Tracer.Handlers.RTView.UI.Elements
--import           Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody (mkPageBody)
--import           Cardano.Tracer.Handlers.RTView.UI.Updater (updateUI)
--import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

runWebServer
  :: Endpoint
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runWebServer (Endpoint host port) acceptedMetrics acceptedNodeInfo =
  UI.startGUI config mainPage
 where
  config = UI.defaultConfig
    { UI.jsPort = Just port
    , UI.jsAddr = Just $ BSC.pack host
    }

  mainPage window = do
    void $ return window # set UI.title ""-- pageTitle
    void $ UI.getHead window #+
      [ UI.link # set UI.rel "icon" # set UI.href "data:,"
      , UI.meta # set UI.name "viewport" # set UI.content "width=device-width, initial-scale=1"
      , UI.mkElement "style" # set UI.html bulmaCSS
      , UI.mkElement "style" # set UI.html ownCSS
      -- , UI.mkElement "script" # set UI.html chartJS
      ]

    --(pageBody, noNodesNotify, rootElemForNodePanels) <- mkPageBody window

    --pageElementsTVar :: TVar PageElements <- liftIO $ newTVarIO HM.empty

    -- Prepare and run the timer, which calls 'updateUI' function once per second.
    uiUpdateTimer <- UI.timer # set UI.interval 1000
    on UI.tick uiUpdateTimer $ const $
      return ()
      -- updateUI acceptedItems pageElementsTVar noNodesNotify rootElemForNodePanels
    UI.start uiUpdateTimer

    on UI.disconnect window $ const $ do
      -- The connection with the browser was dropped (user closed the tab),
      -- so previous timer should be stopped.
      UI.stop uiUpdateTimer




    return ()
    -- void $ UI.element pageBody
