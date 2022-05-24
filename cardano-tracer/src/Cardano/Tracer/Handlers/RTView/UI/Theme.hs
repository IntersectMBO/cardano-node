{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.Theme
  ( lightState
  , darkState
  , restoreTheme
  , switchTheme
  , isCurrentThemeDark
  ) where

import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

restoreTheme, switchTheme :: UI.Window -> UI ()
restoreTheme window = readSavedTheme >>= setThemeAndSave window
switchTheme window  = readSavedTheme >>= setThemeAndSave window . switch
 where
  switch s = if s == darkState then lightState else darkState

isCurrentThemeDark :: UI Bool
isCurrentThemeDark = (== darkState) <$> readSavedTheme

setThemeAndSave
  :: UI.Window
  -> String
  -> UI ()
setThemeAndSave window themeToSet = do
  changeThemeIcon
  changeBodyClass
  changeCharts
  saveTheme themeToSet
 where
  toBeLight = themeToSet == lightState

  changeThemeIcon = do
    change "theme-icon" $
        set html        (if toBeLight then rtViewThemeToDarkSVG else rtViewThemeToLightSVG)
      . set dataState   (if toBeLight then lightState else darkState)
      . set dataTooltip ("Switch to " <> (if toBeLight then "dark" else "light") <> " theme")

  changeBodyClass =
    getElementsByTagName window "body" >>= \case
      [body] -> void $ element body # set UI.class_ (if toBeLight then lightState else darkState)
      _ -> return ()

  change elId what = findAndSet what window elId

  changeCharts =
    if toBeLight
      then changeChartsToLightTheme
      else changeChartsToDarkTheme

lightState, darkState :: String
lightState = "light"
darkState  = "dark"

-- | Every time when the user changed the theme, it should be saved on the file
--   for next sessions, both after web-page reload and 'cardano-tracer' restart.
saveTheme :: String -> UI ()
saveTheme state = liftIO . ignore $ do
  pathToThemeConfig <- getPathToThemeConfig
  TIO.writeFile pathToThemeConfig $ T.pack state

readSavedTheme :: UI String
readSavedTheme = liftIO $
  try_ (TIO.readFile =<< getPathToThemeConfig) >>= \case
    Right saved -> return $ T.unpack saved
    Left _      -> return darkState -- Use dark theme by default.
