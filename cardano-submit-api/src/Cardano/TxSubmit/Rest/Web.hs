{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Rest.Web
  ( runSettings
  )
where

import           Cardano.Api.Pretty (textShow)

import           Cardano.BM.Trace (Trace, logInfo)
import           Cardano.TxSubmit.Tracing.Message (Message(..), MetricAction (MetricActionNone))

import           Control.Exception (bracket)
import           Data.Streaming.Network (bindPortTCP)
import           Data.Text (Text)
import           Network.Socket (close, getSocketName, withSocketsDo)
import           Network.Wai.Handler.Warp (Settings, getHost, getPort, runSettingsSocket)

import           Servant (Application)
import qualified Cardano.Logging.Types as TraceD
import Cardano.Logging.Trace (traceWith)
import Cardano.Logging.Types (SeverityS(Info))

-- | Like 'Network.Wai.Handler.Warp.runSettings', except with better logging.
runSettings :: Trace IO Text -> TraceD.Trace IO Message -> Settings -> Application -> IO ()
runSettings trace trace' settings app =
  withSocketsDo $
    bracket
      (bindPortTCP (getPort settings) (getHost settings))
      close
      ( \socket -> do
          addr <- getSocketName socket
          logInfo trace $ "Web API listening on port " <> textShow addr
          traceWith trace' $ Message Info ("Web API listening on port " <> textShow addr) MetricActionNone
          runSettingsSocket settings socket app
      )
