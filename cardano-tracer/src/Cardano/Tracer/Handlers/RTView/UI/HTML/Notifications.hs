module Cardano.Tracer.Handlers.RTView.UI.HTML.Notifications
  ( mkNotificationsEvents
  , mkNotificationsSettings
  ) where

--import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

--import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
--import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkNotificationsEvents :: UI Element
mkNotificationsEvents = do
  closeIt <- UI.button #. "delete"
  notifications <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card" #+
          [ UI.header #. "modal-card-head rt-view-notifications-head" #+
              [ UI.p #. "modal-card-title rt-view-notifications-title" # set text "Notifications: events"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-notifications-body" #+
              [ UI.div #. "field" #+
                  [ UI.input ## "switchRoundedInfo"
                             #. "switch is-rounded is-info"
                             # set UI.type_ "checkbox"
                             # set UI.name "switchRoundedInfo"
                  , UI.label # set UI.for "switchRoundedInfo"
                             # set text "Switch info"
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element notifications #. "modal"
  return notifications

mkNotificationsSettings :: UI Element
mkNotificationsSettings = do
  closeIt <- UI.button #. "delete"
  notifications <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card" #+
          [ UI.header #. "modal-card-head rt-view-notifications-head" #+
              [ UI.p #. "modal-card-title rt-view-notifications-title" # set text "Notifications: settings"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-notifications-body" #+
              [ string "SETTINGS"
              ]
          ]
      ]
  on UI.click closeIt . const $ element notifications #. "modal"
  return notifications





