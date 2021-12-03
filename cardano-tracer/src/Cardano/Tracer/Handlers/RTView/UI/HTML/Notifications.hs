{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.GUI.Markup.Notifications
    ( mkNotifications
    ) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forM_, void)
import           Control.Monad.STM (atomically)
import           Data.Text (isPrefixOf, pack, unpack)
import           Text.Read (readMaybe)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, get, set, string,
                                              (#), (#+))

import           Cardano.BM.Configuration (Configuration)

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.Config (saveNotificationsForNextSessions)
import           Cardano.RTView.GUI.Elements (HTMLClass (..), HTMLId (..), (#.), (##),
                                              hideIt, showIt, showInline)
import           Cardano.RTView.Notifications.Send (sendTestEmail)
import           Cardano.RTView.Notifications.Types

mkNotifications
  :: UI.Window
  -> Configuration
  -> RTViewParams
  -> TVar NotificationSettings
  -> Element
  -> UI Element
mkNotifications window _config _params notifyTVar notificationsButton = do
  NotificationSettings {..} <- liftIO $ readTVarIO notifyTVar
  let EventsToNotify {..} = nsEventsToNotify
      HowToNotify {..}    = nsHowToNotify

  closeButton <- UI.img #. [W3DisplayTopright, RTViewInfoClose]
                        # set UI.src "/static/images/times.svg"
                        # set UI.title__ "Save changes and close"

  (mainSwitch, mainSwitchSpan, mainSwitchRoot)
    <- mkSwitch nsEnabled "Click to enable/disable notifications"
  mainSwitchLabel <- string "Enabled"

  eventsTab <- UI.button #. [W3BarItem, W3Button, W3Mobile]
                         # makeItActive
                         # set UI.text "Events to notify"
  howToTab  <- UI.button #. [W3BarItem, W3Button, W3Mobile]
                         # set UI.text "How to notify"

  eventsTabContent <- mkEventsTabContent notifyTVar errorsEvents blockchainEvents
  howToTabContent  <- mkHowToTabContent window notifyTVar emailSettings # hideIt

  let tabs :: [((Element, Element), Int)]
      tabs = let allTabs = [ (eventsTab, eventsTabContent)
                           , (howToTab,  howToTabContent)
                           ]
             in zip allTabs [1..length allTabs]

  registerClicksOnTabs tabs

  settings
    <- UI.div #. [W3Container] #+
         [ UI.div #. [W3Bar, W3BorderBottom, NotificationsBar] #+
             [ element eventsTab
             , element howToTab
             ]
         , UI.div #+
             [ element eventsTabContent
             , element howToTabContent
             ]
         ]

  notifications <-
    UI.div #. [W3Modal] #+
      [ UI.div #. [W3ModalContent, W3AnimateTop, W3Card4] #+
          [ UI.div #. [W3Container, RTViewInfoTop] #+
              [ element closeButton
              , UI.h2 #+ [ string "Notifications" ]
              ]
          , UI.div #. [W3Container, RTViewInfoContainer] #+
              [ UI.div #. [W3Row, NotificationsMainSwitch] #+
                  [ UI.div #. [W3Col, SwitchContainer] #+
                      [ element mainSwitchRoot
                      ]
                  , UI.div #. [W3Rest] #+
                      [ element mainSwitchLabel ]
                  ]
              ]
          , element settings
          ]
      ]

  void $ UI.onEvent (UI.click closeButton) $ \_ -> do
    void $ element notifications # hideIt
    takeEmailSettingsInputs window notifyTVar
    notifySettings <- liftIO $ readTVarIO notifyTVar
    liftIO $ saveNotificationsForNextSessions notifySettings

  void $ UI.onEvent (UI.checkedChange mainSwitch) $ \isChecked -> do
    let (label, label', bell, iconClass, action) =
          if isChecked
            then ("disable", "Enabled",  "bell",       NotificationsIcon,      showIt)
            else ("enable",  "Disabled", "bell-slash", NotificationsIconSlash, hideIt)
    void $ element mainSwitchLabel # set UI.text label'
    void $ element mainSwitchSpan # set UI.title__ ("Click to " <> label <> " notifications")
    void $ element notificationsButton #. [iconClass]
                                       # set UI.src ("/static/images/" <> bell <> ".svg")
    void $ element settings # action
    -- Save it in the notifications settings.
    liftIO . atomically . modifyTVar' notifyTVar $ \notifySettings ->
      notifySettings { nsEnabled = isChecked }

  return notifications

registerClicksOnTabs
  :: [((Element, Element), Int)]
  -> UI ()
registerClicksOnTabs tabs =
  forM_ tabs $ \((tab, _), tabNum) ->
    void $ UI.onEvent (UI.click tab) $ \_ -> showTabAndMakeItActive tabNum
 where
  showTabAndMakeItActive num =
    forM_ tabs $ \((tab', tabContent), tabNum') ->
      if num == tabNum'
        then do
          void $ element tabContent # showIt
          void $ element tab' # makeItActive
        else do
          void $ element tabContent # hideIt
          void $ element tab' # makeItInactive

makeItActive, makeItInactive :: UI Element -> UI Element
makeItActive el   = el #. [W3BarItem, W3Button, W3Mobile, ActiveTab]
makeItInactive el = el #. [W3BarItem, W3Button, W3Mobile]

mkSwitch
  :: Bool
  -> String
  -> UI (Element, Element, Element)
mkSwitch isChecked aTitle = do
  switch
    <- UI.input # set UI.type_ "checkbox" # set UI.checked isChecked
  switchSpan
    <- UI.span #. [Slider, Round] # set UI.title__ aTitle
  switchRoot
    <- UI.label #. [Switch] #+
         [ element switch
         , element switchSpan
         ]
  return (switch, switchSpan, switchRoot)

-- | Here we describe events the user will be notified on.
mkEventsTabContent
  :: TVar NotificationSettings
  -> ErrorsEvents
  -> BlockchainEvents
  -> UI Element
mkEventsTabContent notifyTVar ErrorsEvents {..} BlockchainEvents {..} = do
  warnings    <- switchSection aboutWarnings    "Warnings"    "warnings"    $ handler setAboutWarnings
  errors      <- switchSection aboutErrors      "Errors"      "errors"      $ handler setAboutErrors
  criticals   <- switchSection aboutCriticals   "Criticals"   "criticals"   $ handler setAboutCriticals
  alerts      <- switchSection aboutAlerts      "Alerts"      "alerts"      $ handler setAboutAlerts
  emergencies <- switchSection aboutEmergencies "Emergencies" "emergencies" $ handler setAboutEmergencies

  missedSlots <- switchSection aboutMissedSlots "Missed slots"      "missed slots"
                   $ handler setAboutMissedSlots
  cannotLead  <- switchSection aboutCannotForge "Node cannot forge" "node that cannot forge"
                   $ handler setAboutCannotForge

  UI.div #. [NotificationsTabContainer] #+
    [ UI.div #. [NotificationsEventsHeader] #+
        [ string "Node errors" ]
    , UI.div #. [NotificationsSwitches] #+
        [ element warnings
        , element errors
        , element criticals
        , element alerts
        , element emergencies
        ]
    , UI.div #. [NodeMetricsVSpacer]
    , UI.div #. [NotificationsEventsHeader] #+
        [ string "Blockchain errors" ]
    , UI.div #. [NotificationsSwitches] #+
        [ element missedSlots
        , element cannotLead
        ]
    ]
 where
  switchSection checked aLabel titleSpec checkedHandler = do
    (switch, _, switchRoot)
      <- mkSwitch checked ("Click to enable/disable notifications on " <> titleSpec)

    void $ UI.onEvent (UI.checkedChange switch) $ \isChecked ->
      checkedHandler isChecked

    UI.div #. [W3Row, NotificationsSwitch] #+
      [ UI.div #. [W3Col, SwitchContainer] #+
          [ element switchRoot
          ]
      , UI.div #. [W3Rest] #+
          [ string aLabel ]
      ]

  handler :: (EventsToNotify -> Bool -> EventsToNotify) -> Bool -> UI ()
  handler setter isChecked =
    liftIO . atomically . modifyTVar' notifyTVar $ \notifySettings ->
      let currentEvents = nsEventsToNotify notifySettings
      in notifySettings { nsEventsToNotify = setter currentEvents isChecked }

  setAboutWarnings :: EventsToNotify -> Bool -> EventsToNotify
  setAboutWarnings evs isChecked =
    evs { errorsEvents = (errorsEvents evs) { aboutWarnings = isChecked } }

  setAboutErrors :: EventsToNotify -> Bool -> EventsToNotify
  setAboutErrors evs isChecked =
    evs { errorsEvents = (errorsEvents evs) { aboutErrors = isChecked } }

  setAboutCriticals :: EventsToNotify -> Bool -> EventsToNotify
  setAboutCriticals evs isChecked =
    evs { errorsEvents = (errorsEvents evs) { aboutCriticals = isChecked } }

  setAboutAlerts :: EventsToNotify -> Bool -> EventsToNotify
  setAboutAlerts evs isChecked =
    evs { errorsEvents = (errorsEvents evs) { aboutAlerts = isChecked } }

  setAboutEmergencies :: EventsToNotify -> Bool -> EventsToNotify
  setAboutEmergencies evs isChecked =
    evs { errorsEvents = (errorsEvents evs) { aboutEmergencies = isChecked } }

  setAboutMissedSlots :: EventsToNotify -> Bool -> EventsToNotify
  setAboutMissedSlots evs isChecked =
    evs { blockchainEvents = (blockchainEvents evs) { aboutMissedSlots = isChecked } }

  setAboutCannotForge :: EventsToNotify -> Bool -> EventsToNotify
  setAboutCannotForge evs isChecked =
    evs { blockchainEvents = (blockchainEvents evs) { aboutCannotForge = isChecked } }

-- | Here we describe how the user will be notified about events.
mkHowToTabContent
  :: UI.Window
  -> TVar NotificationSettings
  -> EmailSettings
  -> UI Element
mkHowToTabContent window notifyTVar EmailSettings {..} = do
  tlsOption   <- UI.option # set UI.value (show TLS)      # set UI.text "TLS"
  sTLSOption  <- UI.option # set UI.value (show StartTLS) # set UI.text "STARTTLS"
  noSSLOption <- UI.option # set UI.value (show NoSSL)    # set UI.text "No SSL"
  case emSSL of
    TLS      -> void $ element tlsOption   # set UI.selected True
    StartTLS -> void $ element sTLSOption  # set UI.selected True
    NoSSL    -> void $ element noSSLOption # set UI.selected True

  testEmailButton
    <- UI.button #. [W3Button, W3Round, TestEmailButton]
                 # set UI.text "Test email"
                 # set UI.title__ "Test email will be sent"
  testEmailResultMessage
    <- string "Result"
  dismiss
    <- UI.span #. [W3Right, TestEmailDismiss]
               # set UI.text "Dismiss"
               # set UI.title__ "Dismiss this message"
               # hideIt
  testEmailResult
    <- UI.div #. [TestEmailResult] # hideIt #+
         [ element testEmailResultMessage
         , element dismiss
         ]

  void $ UI.onEvent (UI.click testEmailButton) $ \_ -> do
    void $ element testEmailButton # set UI.enabled False
    void $ element testEmailResultMessage #. [] # set UI.text "Please wait..."
    void $ element testEmailResult # showIt
    void $ element dismiss # hideIt
    takeEmailSettingsInputs window notifyTVar
    notifySettings <- liftIO $ readTVarIO notifyTVar
    result <- liftIO $ sendTestEmail notifySettings
    void $ element testEmailResultMessage # set UI.text (unpack result)
    if "Yay!" `isPrefixOf` result
      then void $ element testEmailResultMessage #. [TestEmailResultSuccess]
      else void $ element testEmailResultMessage #. [TestEmailResultError]
    void $ element testEmailResult # showIt
    void $ element dismiss # showInline
    void $ element testEmailButton # set UI.enabled True

  void $ UI.onEvent (UI.click dismiss) $ \_ ->
    void $ element testEmailResult # hideIt

  UI.div #. [NotificationsTabContainer] #+
    [ UI.div #. [NotificationsEventsHeader] #+
        [ string "Email settings"
        , infoMark "Current release supports email notifications only"
        ]
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

takeEmailSettingsInputs
  :: UI.Window
  -> TVar NotificationSettings
  -> UI ()
takeEmailSettingsInputs window notifyTVar = do
  host      <- getValueOf ServerHostInput
  port      <- getValueOf ServerPortInput
  user      <- getValueOf UsernameInput
  pass      <- getValueOf PasswordInput
  ssl       <- getValueOf SSLInput
  emailFrom <- getValueOf EmailFromInput
  emailTo   <- getValueOf EmailToInput
  subject   <- getValueOf SubjectInput

  liftIO . atomically . modifyTVar' notifyTVar $ \notifySettings ->
    let currentSettings = nsHowToNotify notifySettings
        newEmailSettings = (emailSettings currentSettings)
          { emServerHost = host
          , emServerPort = maybe 0 id $ (readMaybe (unpack port) :: Maybe Int)
          , emUsername   = user
          , emPassword   = pass
          , emSSL        = maybe TLS id $ (readMaybe (unpack ssl) :: Maybe SSL)
          , emEmailFrom  = emailFrom
          , emEmailTo    = emailTo
          , emSubject    = subject
          }
    in notifySettings { nsHowToNotify = currentSettings { emailSettings = newEmailSettings } }
 where
   getValueOf anId = do
    s <- UI.getElementById window (show anId) >>= maybe (pure "") (get UI.value)
    return $ pack s

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. [InfoMark]
          #  set UI.title__ aTitle
          #+ [ UI.img #. [InfoMarkImg]
                      # set UI.src "/static/images/question.svg" #+ []
             ]
