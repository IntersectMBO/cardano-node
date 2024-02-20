{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Notifications
  ( mkNotificationsEvents
  , mkNotificationsSettings
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Notifications.Email
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Notifications.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Utils

import           Control.Monad (void)
import           Control.Monad.Extra (whenJustM)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

mkNotificationsEvents :: TracerEnv -> EventsQueues -> UI Element
mkNotificationsEvents tracerEnv eventsQueues = do
  closeIt <- UI.button #. "delete"

  (switchAll, switchAllW) <- mkSwitch "switch-all" "All events" ""

  (switchWarnings,    switchWarningsW)    <- mkSwitch "switch-warnings"    "Warnings"    "warning"
  (switchErrors,      switchErrorsW)      <- mkSwitch "switch-errors"      "Errors"      "danger"
  (switchCriticals,   switchCriticalsW)   <- mkSwitch "switch-criticals"   "Criticals"   "danger"
  (switchAlerts,      switchAlertsW)      <- mkSwitch "switch-alerts"      "Alerts"      "danger"
  (switchEmergencies, switchEmergenciesW) <- mkSwitch "switch-emergencies" "Emergencies" "danger"
  (switchNodeDiscon,  switchNodeDisconW)  <- mkSwitch "switch-node-disconnected" "Node disconnected" "warning"

  selectNotifyPeriodWarnings    <- mkSelect "select-period-warnings"
  selectNotifyPeriodErrors      <- mkSelect "select-period-errors"
  selectNotifyPeriodCriticals   <- mkSelect "select-period-criticals"
  selectNotifyPeriodAlerts      <- mkSelect "select-period-alerts"
  selectNotifyPeriodEmergencies <- mkSelect "select-period-emergencies"
  selectNotifyPeriodNodeDiscon  <- mkSelect "select-period-node-disconnected"

  notifications <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card" #+
          [ UI.header #. "modal-card-head rt-view-notifications-head" #+
              [ UI.p #. "modal-card-title rt-view-notifications-title" # set text "Notifications: events"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-notifications-body" #+
              [ mkDivider "Common Errors"
              , UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element switchWarningsW
                      , element switchErrorsW
                      , element switchCriticalsW
                      , element switchAlertsW
                      , element switchEmergenciesW
                      ]
                  , UI.div #. "column" #+
                      [ mkErrorsSelectWrapper selectNotifyPeriodWarnings
                      , mkErrorsSelectWrapper selectNotifyPeriodErrors
                      , mkErrorsSelectWrapper selectNotifyPeriodCriticals
                      , mkErrorsSelectWrapper selectNotifyPeriodAlerts
                      , mkErrorsSelectWrapper selectNotifyPeriodEmergencies
                      ]
                  ]
              , mkDivider "Network"
              , UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element switchNodeDisconW
                      ]
                  , UI.div #. "column" #+
                      [ mkErrorsSelectWrapper selectNotifyPeriodNodeDiscon
                      ]
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-notification-settings-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element switchAllW
                      ]
                  ]
              ]
          ]
      ]

  on UI.click closeIt . const $ do
    void $ element notifications #. "modal"
    saveEventsSettings tracerEnv

  on UI.checkedChange switchWarnings $ \state -> do
    setNotifyIconState
    saveEventsSettings tracerEnv
    liftIO $ updateNotificationsEvents eventsQueues EventWarnings state
  on UI.checkedChange switchErrors $ \state -> do
    setNotifyIconState
    saveEventsSettings tracerEnv
    liftIO $ updateNotificationsEvents eventsQueues EventErrors state
  on UI.checkedChange switchCriticals $ \state -> do
    setNotifyIconState
    saveEventsSettings tracerEnv
    liftIO $ updateNotificationsEvents eventsQueues EventCriticals state
  on UI.checkedChange switchAlerts $ \state -> do
    setNotifyIconState
    saveEventsSettings tracerEnv
    liftIO $ updateNotificationsEvents eventsQueues EventAlerts state
  on UI.checkedChange switchEmergencies $ \state -> do
    setNotifyIconState
    saveEventsSettings tracerEnv
    liftIO $ updateNotificationsEvents eventsQueues EventEmergencies state
  on UI.checkedChange switchNodeDiscon $ \state -> do
    setNotifyIconState
    saveEventsSettings tracerEnv
    liftIO $ updateNotificationsEvents eventsQueues EventNodeDisconnected state

  on UI.checkedChange switchAll $ \state -> do
    void $ element switchWarnings    # set UI.checked state
    void $ element switchErrors      # set UI.checked state
    void $ element switchCriticals   # set UI.checked state
    void $ element switchAlerts      # set UI.checked state
    void $ element switchEmergencies # set UI.checked state
    void $ element switchNodeDiscon  # set UI.checked state

    setNotifyIconState
    saveEventsSettings tracerEnv

    liftIO $ do
      updateNotificationsEvents eventsQueues EventWarnings state
      updateNotificationsEvents eventsQueues EventErrors state
      updateNotificationsEvents eventsQueues EventCriticals state
      updateNotificationsEvents eventsQueues EventAlerts state
      updateNotificationsEvents eventsQueues EventEmergencies state
      updateNotificationsEvents eventsQueues EventNodeDisconnected state

  handleSelectChange selectNotifyPeriodWarnings    EventWarnings
  handleSelectChange selectNotifyPeriodErrors      EventErrors
  handleSelectChange selectNotifyPeriodCriticals   EventCriticals
  handleSelectChange selectNotifyPeriodAlerts      EventAlerts
  handleSelectChange selectNotifyPeriodEmergencies EventEmergencies
  handleSelectChange selectNotifyPeriodNodeDiscon  EventNodeDisconnected

  return notifications
 where
  handleSelectChange selector eventGroup =
    on UI.selectionChange selector . const $
      whenJustM (readMaybe <$> get value selector) $ \(period :: PeriodInSec) ->
        liftIO $ updateNotificationsPeriods eventsQueues eventGroup period

mkDivider :: String -> UI Element
mkDivider dTitle =
  UI.div #. "divider rt-view-divider" #+
    [ UI.span #. "rt-view-events-title" # set text dTitle
    ]

mkErrorsSelectWrapper :: Element -> UI Element
mkErrorsSelectWrapper sel =
  UI.div #. "rt-view-notifications-errors-select-wrapper" #+
    [ UI.div #. "field is-grouped" #+
        [ UI.p #. "control" #+
            [ UI.div #. "select is-link is-small" #+
                [ element sel
                ]
            ]
        , UI.p #. "control" #+
            [ image "has-tooltip-multiline has-tooltip-left rt-view-period-what-icon" whatSVG
                    # set dataTooltip "How often notification will be sent"
            ]
        ]
    ]

mkSelect :: String -> UI Element
mkSelect elId =
  UI.select ## elId #+
    -- Values are periods in seconds.
    [ UI.option # set value "5"     # set text "Immediately"
    , UI.option # set value "300"   # set text "Every 5 minutes"
    , UI.option # set value "1800"  # set text "Every 30 minutes"
    , UI.option # set value "3600"  # set text "Every 1 hour"
    , UI.option # set value "10800" # set text "Every 3 hours"
    , UI.option # set value "21600" # set text "Every 6 hours"
    , UI.option # set value "43200" # set text "Every 12 hours"
    ]

mkSwitch
  :: String
  -> String
  -> String
  -> UI (Element, Element)
mkSwitch switchId switchName bulmaColorName = do
  let colorClass = case bulmaColorName of
                     "" -> ""
                     _  -> " is-" <> bulmaColorName
  aSwitch <-
    UI.input ## switchId
             #. ("switch is-rounded" <> colorClass)
             # set UI.type_ "checkbox"
             # set UI.name switchId

  switchWrapper <-
    UI.div #. "field" #+
      [ element aSwitch
      , UI.label # set UI.for switchId
                 # set text switchName
      ]

  return (aSwitch, switchWrapper)

-- | Settings for notifications (email, etc.).

mkNotificationsSettings :: TracerEnv -> UI Element
mkNotificationsSettings tracerEnv = do
  closeIt <- UI.button #. "delete"
  sendTestEmail <- UI.button ## "send-test-email"
                             #. "button is-primary"
                             # set text "Send test email"
  sendTestEmailStatus <- UI.span # set text ""
  showHidePasswordIcon <- image "rt-view-show-hide-pass-icon" hideSVG
  showHidePassword <- UI.button #. "button is-info"
                                # set dataState hiddenState
                                #+ [element showHidePasswordIcon]
  inputHost <- UI.input ## "es-smtp-host"
                        #. "input is-normal"
                        # set (attr "placeholder") "e.g. smtp.gmail.com"
                        # set (attr "required") "required"
  inputUser <- UI.input ## "es-username"
                        #. "input is-normal"
                        # set (attr "placeholder") "e.g. your.name@gmail.com"
                        # set (attr "required") "required"
  inputPassword <- UI.input ## "es-password"
                            #. "input is-normal"
                            # set UI.type_ "password"
                            # set (attr "placeholder") "your password"
                            # set (attr "required") "required"
  inputEmailFrom <- UI.input ## "es-email-from"
                             #. "input is-normal"
                             # set UI.type_ "email"
                             # set (attr "placeholder") "e.g. your.no.reply@gmail.com"
                             # set (attr "required") "required"
  inputEmailTo <- UI.input ## "es-email-to"
                           #. "input is-normal"
                           # set UI.type_ "email"
                           # set (attr "placeholder") "e.g. your.name@gmail.com"
                           # set (attr "required") "required"
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
              , mkControlPair "SMTP host *" $ element inputHost
              , mkControlPair "SMTP port" $
                  UI.div #. "select" #+
                    [ UI.select ## "es-smtp-port" #+
                        [ UI.option # set value "25"   # set text "25"
                        , UI.option # set value "465"  # set text "465"
                        , UI.option # set value "587"  # set text "587"
                        , UI.option # set value "2525" # set text "2525"
                        ]
                    ]
              , mkControlPair "Username *" $ element inputUser
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "Password *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field has-addons" #+
                          [ UI.p #. "control" #+
                              [ element inputPassword
                              ]
                          , UI.div #. "control" #+
                              [ element showHidePassword
                              ]
                          ]
                      ]
                  ]
              , mkControlPair "SSL" $
                  UI.div #. "select" #+
                    [ UI.select ## "es-ssl" #+
                        [ UI.option # set value (show TLS)      # set text "TLS"
                        , UI.option # set value (show STARTTLS) # set text "STARTTLS"
                        , UI.option # set value (show NoSSL)    # set text "No SSL"
                        ]
                    ]
              , mkControlPair "From *" $ element inputEmailFrom
              , mkControlPair "To *" $ element inputEmailTo
              , mkControlPair "Subject" $
                  UI.input ## "es-subject"
                           #. "input is-normal"
                           # set (attr "placeholder") "e.g. Cardano RTView Notification"
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-notification-settings-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field is-grouped" #+
                          [ UI.p #. "control" #+
                              [ element sendTestEmail
                              ]
                          , UI.p #. "control pt-2" #+
                              [ element sendTestEmailStatus
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]

  on UI.click closeIt . const $ do
    void $ element notifications #. "modal"
    void $ element sendTestEmailStatus # set text ""
    saveEmailSettings tracerEnv

  on UI.click sendTestEmail . const $ do
    void $ element sendTestEmailStatus # set text ""
    void $ element sendTestEmail #. "button is-primary is-loading"
                                 # set UI.enabled False
    statusMessage <- liftIO . createAndSendTestEmail =<< getCurrentEmailSettings
    let msgClass =
          if statusIsOK statusMessage
            then "rt-view-test-status-message-ok"
            else "rt-view-test-status-message-fail"
    void $ element sendTestEmailStatus # set text (T.unpack statusMessage)
                                       # set UI.class_ msgClass
    void $ element sendTestEmail #. "button is-primary"
                                 # set UI.enabled True

  on UI.click showHidePassword . const $ do
    state <- get dataState showHidePassword
    let haveToHide = state == shownState
    if haveToHide
      then do
        void $ element showHidePasswordIcon # set html hideSVG
        void $ element showHidePassword # set dataState hiddenState
        void $ element inputPassword # set UI.type_ "password"
      else do
        void $ element showHidePasswordIcon # set html showSVG
        void $ element showHidePassword # set dataState shownState
        void $ element inputPassword # set UI.type_ "text"

  on UI.valueChange inputHost      $ const setStatusTestEmailButton
  on UI.valueChange inputUser      $ const setStatusTestEmailButton
  on UI.valueChange inputPassword  $ const setStatusTestEmailButton
  on UI.valueChange inputEmailFrom $ const setStatusTestEmailButton
  on UI.valueChange inputEmailTo   $ const setStatusTestEmailButton

  return notifications

mkControlPair
  :: String
  -> UI Element
  -> UI Element
mkControlPair labelText control =
  UI.div #. "field is-horizontal" #+
    [ UI.div #. "field-label is-normal" #+
        [ UI.label #. "label rt-view-label" # set text labelText
        ]
    , UI.div #. "field-body" #+
        [ UI.div #. "field" #+
            [ UI.p #. "control" #+
                [ control
                ]
            ]
        ]
    ]
