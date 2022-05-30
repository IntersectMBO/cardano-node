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
      , UI.div #. "modal-card rt-view-notifications-settings" #+
          [ UI.header #. "modal-card-head rt-view-notifications-head" #+
              [ UI.p #. "modal-card-title rt-view-notifications-title" # set text "Notifications: settings"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-notifications-body" #+
              [ UI.p #. "rt-view-email-only" #+
                  [ string "Currently, only email notifications are supported"
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "SMTP host *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set (attr "placeholder") "e.g. smtp.gmail.com"
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "SMTP port *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set UI.type_ "number"
                                         # set (attr "placeholder") "e.g. 587"
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "Username *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set (attr "placeholder") "e.g. your.name@gmail.com"
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "Password *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set UI.type_ "password"
                                         # set (attr "placeholder") "your password"
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "SSL"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.div #. "select" #+
                                  [ UI.select #+
                                      [ UI.option # set value "TLS"      # set text "TLS"
                                      , UI.option # set value "STARTTLS" # set text "STARTTLS"
                                      , UI.option # set value "NO_SSL"   # set text "No SSL"
                                      ]
                                  ]
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "From *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set UI.type_ "email"
                                         # set (attr "placeholder") "e.g. your.no.reply@gmail.com"
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "To *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set UI.type_ "email"
                                         # set (attr "placeholder") "e.g. your.name@gmail.com"
                              ]
                          ]
                      ]
                  ]
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "Subject"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field" #+
                          [ UI.p #. "control" #+
                              [ UI.input #. "input is-normal"
                                         # set (attr "placeholder") "e.g. Cardano RTView Notification"
                              ]
                          ]
                      ]
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-notification-settings-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field is-grouped" #+
                          [ UI.p #. "control" #+
                              [ UI.button #. "button is-primary"
                                          # set UI.enabled False
                                          # set text "Send test email"
                              ]
                          , UI.p #. "control" #+
                              [ string "" -- element searchMessages
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element notifications #. "modal"
  return notifications


{-
    , UI.div #. [W3RowPadding] #+
        [ UI.div #. [W3Half] #+
            [ UI.label # set UI.text "SMTP server host"
            , string "*" #. [RequiredInput]
            , UI.input ## show ServerHostInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "url"
                       # set UI.value (unpack emServerHost)
            ]
        , UI.div #. [W3Half] #+
            [ UI.label # set UI.text "SMTP server port"
            , string "*" #. [RequiredInput]
            , UI.input ## show ServerPortInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "number"
                       # set UI.value (show emServerPort)
            ]
        ]
    , UI.div #. [NotificationsVSpacer]
    , UI.div #. [W3RowPadding] #+
        [ UI.div #. [W3Third] #+
            [ UI.label # set UI.text "Username"
            , string "*" #. [RequiredInput]
            , UI.input ## show UsernameInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "text"
                       # set UI.value (unpack emUsername)
            ]
        , UI.div #. [W3Third] #+
            [ UI.label # set UI.text "Password"
            , string "*" #. [RequiredInput]
            , UI.input ## show PasswordInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "password"
                       # set UI.value (unpack emPassword)
            ]
        , UI.div #. [W3Third] #+
            [ UI.label # set UI.text "SSL"
            , string "*" #. [RequiredInput]
            , UI.select ## show SSLInput #. [W3Select] # set UI.name "option" #+
                [ element tlsOption
                , element sTLSOption
                , element noSSLOption
                ]
            ]
        ]
    , UI.div #. [NotificationsVSpacer]
    , UI.div #. [W3RowPadding] #+
        [ UI.div #. [W3Third] #+
            [ UI.label # set UI.text "Email From"
            , string "*" #. [RequiredInput]
            , UI.input ## show EmailFromInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "email"
                       # set UI.value (unpack emEmailFrom)
            ]
        , UI.div #. [W3Third] #+
            [ UI.label # set UI.text "Email To"
            , string "*" #. [RequiredInput]
            , UI.input ## show EmailToInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "email"
                       # set UI.value (unpack emEmailTo)
            ]
        , UI.div #. [W3Third] #+
            [ UI.label # set UI.text "Subject"
            , UI.input ## show SubjectInput
                       #. [W3Input, NotificationsInput]
                       # set UI.type_ "text"
                       # set UI.value (unpack emSubject)
            ]
        ]
    , UI.div #. [TestEmailContainer] #+
        [ UI.div #. [W3Row] #+
            [ UI.div #. [W3Col, TestEmailButtonArea] #+
                [element testEmailButton]
            , UI.div #. [W3Rest] #+
                [element testEmailResult]
            ]
        ]
    ]
-}

