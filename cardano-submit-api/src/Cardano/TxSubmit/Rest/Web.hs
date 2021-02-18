{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Rest.Web
  ( runSettings
  ) where

import Control.Exception
    ( bracket )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Logger
    ( logInfoN, runStdoutLoggingT )
import Data.Streaming.Network
    ( bindPortTCP )
import Data.Text
    ( pack )
import Network.Socket
    ( close, getSocketName, withSocketsDo )
import Network.Wai.Handler.Warp
    ( Settings, getHost, getPort, runSettingsSocket )
import Servant
    ( Application )

-- | Like 'Network.Wai.Handler.Warp.runSettings', except with better logging.
runSettings :: Settings -> Application -> IO ()
runSettings settings app =
  withSocketsDo $ do
    bracket
      (bindPortTCP (getPort settings) (getHost settings))
      close
      (\socket ->
         runStdoutLoggingT $ do
           addr <- liftIO $ getSocketName socket
           logInfoN $ "Running server on " <> pack (show addr)
           liftIO $ runSettingsSocket settings socket app)
