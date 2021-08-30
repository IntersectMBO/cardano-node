module Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
  ( mkPageBody
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo (mkOwnInfo)
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody
  :: UI.Window
  -> UI (Element, Element, Element)
mkPageBody window = do
  noNodesNotify
    <- UI.div #. "container is-max-widescreen has-text-centered" #+
         [ image "rt-view-no-nodes-icon" noNodesSVG
         , UI.p #. "rt-view-no-nodes-message" #+
             [ string "There are no connected nodes. Yet." ]
         ]
  rootElemForNodePanels
    <- UI.div #. "container is-max-widescreen" #+ []
  body
    <- UI.getBody window #+
         [ topNavigation
         , UI.mkElement "section" #. "section" #+
             [ element noNodesNotify
             , element rootElemForNodePanels
             ]
         ]
  return (body, noNodesNotify, rootElemForNodePanels)

topNavigation :: UI Element
topNavigation = do
  closeInfo <- UI.button #. "modal-close is-large" #+ []
  info <- mkOwnInfo closeInfo
  infoIcon <- image "mr-4 rt-view-info-icon" rtViewInfoSVG # set UI.title__ "RTView info"
  registerClicksForModal info infoIcon closeInfo

  closeNotifications <- UI.button #. "modal-close is-large" #+ []
  notifications <- mkOwnInfo closeNotifications
  notifyIcon <- image "rt-view-notify-icon" rtViewNotifySVG # set UI.title__ "RTView notifications"
  registerClicksForModal notifications notifyIcon closeNotifications

  UI.div #. "navbar rt-view-top-bar" #+
    [ element info
    , element notifications
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoSVG
            , UI.span #. "rt-view-name" # set text "Node Real-time View"
            ]
        ]
    , UI.div #. "navbar-menu" #+
        [ UI.div #. "navbar-start" #+ []
        , UI.div #. "navbar-end" #+
            [ UI.div #. "navbar-item" #+ [ element notifyIcon ]
            , UI.div #. "navbar-item" #+ [ element infoIcon ]
            ]
        ]
    ]
 where
  registerClicksForModal modal iconToOpen iconToClose = do
    on UI.click iconToOpen  $ const $ element modal #. "modal is-active"
    on UI.click iconToClose $ const $ element modal #. "modal"
