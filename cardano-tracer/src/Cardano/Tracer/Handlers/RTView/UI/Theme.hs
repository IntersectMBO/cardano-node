{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.Theme
  ( lightState
  , darkState
  , restoreTheme
  , switchTheme
  , isCurrentThemeDark
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

restoreTheme, switchTheme :: TracerEnv -> UI ()
restoreTheme tracerEnv = readSavedTheme tracerEnv >>= setThemeAndSave tracerEnv
switchTheme tracerEnv  = readSavedTheme tracerEnv >>= setThemeAndSave tracerEnv . switch
 where
  switch s = if s == darkState then lightState else darkState

isCurrentThemeDark :: TracerEnv -> UI Bool
isCurrentThemeDark tracerEnv = (== darkState) <$> readSavedTheme tracerEnv

setThemeAndSave
  :: TracerEnv
  -> String
  -> UI ()
setThemeAndSave tracerEnv themeToSet = do
  window <- askWindow
  let change elId what = findAndSet what window elId

  change "theme-icon" $
      set html        (if toBeLight then rtViewThemeToDarkSVG else rtViewThemeToLightSVG)
    . set dataState   (if toBeLight then lightState else darkState)
    . set dataTooltip ("Switch to " <> (if toBeLight then "dark" else "light") <> " theme")

  getElementsByTagName window "body" >>= \case
    [body] -> void $ element body # set UI.class_ (if toBeLight then lightState else darkState)
    _ -> return ()

  if toBeLight
    then changeChartsToLightTheme
    else changeChartsToDarkTheme

  saveTheme tracerEnv themeToSet
 where
  toBeLight = themeToSet == lightState

lightState, darkState :: String
lightState = "light"
darkState  = "dark"

-- | Every time when the user changed the theme, it should be saved on the file
--   for next sessions, both after web-page reload and 'cardano-tracer' restart.
saveTheme :: TracerEnv -> String -> UI ()
saveTheme tracerEnv state = liftIO . ignore $ do
  pathToThemeConfig <- getPathToThemeConfig tracerEnv
  TIO.writeFile pathToThemeConfig $ T.pack state

readSavedTheme :: TracerEnv -> UI String
readSavedTheme tracerEnv = liftIO $
  try_ (TIO.readFile =<< getPathToThemeConfig tracerEnv) >>= \case
    Right saved -> return $ T.unpack saved
    Left _      -> return darkState -- Use dark theme by default.
