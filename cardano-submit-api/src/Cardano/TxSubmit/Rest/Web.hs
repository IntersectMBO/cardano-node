{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Rest.Web
  ( runSettings
  ) where

import           Cardano.BM.Trace (Trace, logInfo)

import           Control.Exception (bracket)
import           Data.Streaming.Network (bindPortTCP)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.Socket (close, getSocketName, withSocketsDo)
import           Network.Wai.Handler.Warp (Settings, getHost, getPort, runSettingsSocket)

import           Servant (Application)

-- | Like 'Network.Wai.Handler.Warp.runSettings', except with better logging.
runSettings :: Trace IO Text -> Settings -> Application -> IO ()
runSettings trace settings app = withSocketsDo $ bracket
  (bindPortTCP (getPort settings) (getHost settings))
  close
  (\socket -> do
    addr <- getSocketName socket
    logInfo trace $ "Running server on " <> T.pack (show addr)
    runSettingsSocket settings socket app)
