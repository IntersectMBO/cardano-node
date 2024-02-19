{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.Logs
  ( restoreLogsLiveViewFont
  , saveLogsLiveViewFont
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.Utils

import           Control.Exception.Extra (ignore, try_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Graphics.UI.Threepenny.Core

-- | When the user opened the logs live view window,
--   the previous font's size should be restored from the disk.
restoreLogsLiveViewFont :: TracerEnv -> UI ()
restoreLogsLiveViewFont tracerEnv = readSavedFontSize tracerEnv >>= setFontSize

readSavedFontSize :: TracerEnv -> UI String
readSavedFontSize tracerEnv = liftIO $
  try_ (TIO.readFile =<< getPathToLogsLiveViewFontConfig tracerEnv) >>= \case
    Right saved -> return $ T.unpack saved
    Left _      -> return "90%"

setFontSize :: String -> UI ()
setFontSize fontSizePct =
  case fontSizePct of
    "50%"  -> setSetterAndFont "1"
    "60%"  -> setSetterAndFont "2"
    "70%"  -> setSetterAndFont "3"
    "80%"  -> setSetterAndFont "4"
    "90%"  -> setSetterAndFont "5"
    "100%" -> setSetterAndFont "6"
    _      -> setSetterAndFont "5"
 where
  setSetterAndFont setterValue = do
    window <- askWindow
    findAndSet (set value setterValue) window "logs-live-view-font-setter"
    findAndSet (set style [("font-size", fontSizePct)]) window "node-logs-live-view-tbody"

-- | Every time when the user changed the font's size in logs live view window,
--   it should be saved on the file for next sessions, both after web-page reload
--   and 'cardano-tracer' restart.
saveLogsLiveViewFont :: TracerEnv -> String -> UI ()
saveLogsLiveViewFont tracerEnv fontSizePct = liftIO . ignore $ do
  pathToLLVFontConfig <- getPathToLogsLiveViewFontConfig tracerEnv
  TIO.writeFile pathToLLVFontConfig $ T.pack fontSizePct
