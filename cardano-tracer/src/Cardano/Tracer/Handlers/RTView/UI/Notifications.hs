{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Notifications
  ( getCurrentEmailSettings
  , getCurrentEventsSettings
  , restoreEmailSettings
  , restoreEventsSettings
  , saveEmailSettings
  , saveEventsSettings
  , setNotifyIconState
  , setStatusTestEmailButton
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Notifications.Settings
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils

import           Control.Monad (unless, when)
import           Data.Maybe (fromMaybe)
import           Data.Text (pack, unpack)
import qualified Data.Text as T
import           Data.Text.Read (decimal)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

restoreEmailSettings :: TracerEnv -> UI ()
restoreEmailSettings TracerEnv{teRTViewStateDir} = do
  eSettings <- liftIO $ readSavedEmailSettings teRTViewStateDir
  setEmailSettings eSettings
  setStatusTestEmailButton
 where
  setEmailSettings settings = do
    window <- askWindow
    setValue window (unpack $ esSMTPHost settings)  "es-smtp-host"
    setValue window (show   $ esSMTPPort settings)  "es-smtp-port"
    setValue window (unpack $ esUsername settings)  "es-username"
    setValue window (unpack $ esPassword settings)  "es-password"
    setValue window (show   $ esSSL settings)       "es-ssl"
    setValue window (unpack $ esEmailFrom settings) "es-email-from"
    setValue window (unpack $ esEmailTo settings)   "es-email-to"
    setValue window (unpack $ esSubject settings)   "es-subject"

  setValue window elValue elId =
    unless (null elValue || elValue == "-1") $
      findAndSet (set value elValue) window elId

restoreEventsSettings :: TracerEnv -> UI ()
restoreEventsSettings TracerEnv{teRTViewStateDir} = do
  eSettings <- liftIO $ readSavedEventsSettings teRTViewStateDir
  setEventsSettings eSettings
  setNotifyIconState
  setSwitchAllState eSettings
 where
  setEventsSettings settings = do
    setState (fst $ evsWarnings settings)         "switch-warnings"
    setState (fst $ evsErrors settings)           "switch-errors"
    setState (fst $ evsCriticals settings)        "switch-criticals"
    setState (fst $ evsAlerts settings)           "switch-alerts"
    setState (fst $ evsEmergencies settings)      "switch-emergencies"
    setState (fst $ evsNodeDisconnected settings) "switch-node-disconnected"

    setPeriod (snd $ evsWarnings settings)         "select-period-warnings"
    setPeriod (snd $ evsErrors settings)           "select-period-errors"
    setPeriod (snd $ evsCriticals settings)        "select-period-criticals"
    setPeriod (snd $ evsAlerts settings)           "select-period-alerts"
    setPeriod (snd $ evsEmergencies settings)      "select-period-emergencies"
    setPeriod (snd $ evsNodeDisconnected settings) "select-period-node-disconnected"

  setState elState elId = do
    window <- askWindow
    findAndSet (set UI.checked elState) window elId

  setPeriod elPeriod elId =
    selectOption elId $ fromIntegral elPeriod

  setSwitchAllState settings = do
    let allChecked = and
          [ fst (evsWarnings settings)
          , fst (evsErrors settings)
          , fst (evsCriticals settings)
          , fst (evsAlerts settings)
          , fst (evsEmergencies settings)
          , fst (evsNodeDisconnected settings)
          ]
    setState allChecked "switch-all"

setNotifyIconState :: UI ()
setNotifyIconState = do
  window <- askWindow
  settings <- getCurrentEventsSettings
  let switches =
        [ fst (evsWarnings settings)
        , fst (evsErrors settings)
        , fst (evsCriticals settings)
        , fst (evsAlerts settings)
        , fst (evsEmergencies settings)
        ]
      anyChecked = or switches
      noChecked  = not anyChecked
      allChecked = and switches
  when anyChecked $ findAndSet (set html rtViewNotifySVG)   window "notifications-icon"
  when noChecked  $ findAndSet (set html rtViewNoNotifySVG) window "notifications-icon"
  findAndSet (set UI.checked allChecked) window "switch-all"

saveEmailSettings :: TracerEnv -> UI ()
saveEmailSettings tracerEnv =
  (liftIO . saveEmailSettingsOnDisk tracerEnv) =<< getCurrentEmailSettings

saveEventsSettings :: TracerEnv -> UI ()
saveEventsSettings tracerEnv =
  (liftIO . saveEventsSettingsOnDisk tracerEnv) =<< getCurrentEventsSettings

getCurrentEmailSettings :: UI EmailSettings
getCurrentEmailSettings = do
  window <- askWindow
  smtpHost  <- findAndGetValue window "es-smtp-host"
  smtpPort  <- findAndGetValue window "es-smtp-port"
  username  <- findAndGetValue window "es-username"
  password  <- findAndGetValue window "es-password"
  ssl       <- findAndGetValue window "es-ssl"
  emailFrom <- findAndGetValue window "es-email-from"
  emailTo   <- findAndGetValue window "es-email-to"
  subject   <- findAndGetValue window "es-subject"
  return $ EmailSettings
    { esSMTPHost  = pack smtpHost
    , esSMTPPort  = readInt (pack smtpPort) (-1)
    , esUsername  = pack username
    , esPassword  = pack password
    , esSSL       = read ssl
    , esEmailFrom = pack emailFrom
    , esEmailTo   = pack emailTo
    , esSubject   = pack subject
    }

getCurrentEventsSettings :: UI EventsSettings
getCurrentEventsSettings = do
  window <- askWindow

  warningsState    <- fromMaybe False <$> findAndGetCheckboxState window "switch-warnings"
  errorsState      <- fromMaybe False <$> findAndGetCheckboxState window "switch-errors"
  criticalsState   <- fromMaybe False <$> findAndGetCheckboxState window "switch-criticals"
  alertsState      <- fromMaybe False <$> findAndGetCheckboxState window "switch-alerts"
  emergenciesState <- fromMaybe False <$> findAndGetCheckboxState window "switch-emergencies"
  nodeDisconState  <- fromMaybe False <$> findAndGetCheckboxState window "switch-node-disconnected"

  warningsPeriod    <- readPeriod 1800 <$> findAndGetValue window "select-period-warnings"
  errorsPeriod      <- readPeriod 1800 <$> findAndGetValue window "select-period-errors"
  criticalsPeriod   <- readPeriod 1800 <$> findAndGetValue window "select-period-criticals"
  alertsPeriod      <- readPeriod 1800 <$> findAndGetValue window "select-period-alerts"
  emergenciesPeriod <- readPeriod 1800 <$> findAndGetValue window "select-period-emergencies"
  nodeDisconPeriod  <- readPeriod 1800 <$> findAndGetValue window "select-period-node-disconnected"

  return $ EventsSettings
    { evsWarnings         = (warningsState,    warningsPeriod)
    , evsErrors           = (errorsState,      errorsPeriod)
    , evsCriticals        = (criticalsState,   criticalsPeriod)
    , evsAlerts           = (alertsState,      alertsPeriod)
    , evsEmergencies      = (emergenciesState, emergenciesPeriod)
    , evsNodeDisconnected = (nodeDisconState,  nodeDisconPeriod)
    }
 where
  readPeriod :: PeriodInSec -> String -> PeriodInSec
  readPeriod defP s =
    case decimal (pack s) of
      Left _ -> defP
      Right (i :: Int, _) -> fromIntegral i

setStatusTestEmailButton :: UI ()
setStatusTestEmailButton = do
  EmailSettings host _ user pass _ eFrom eTo _ <- getCurrentEmailSettings
  let allRequiredIsHere =
           isHere host
        && isHere user
        && isHere pass
        && isHere eFrom
        && isHere eTo
  window <- askWindow
  findAndSet (set UI.enabled allRequiredIsHere) window "send-test-email"
 where
  isHere = not . T.null
