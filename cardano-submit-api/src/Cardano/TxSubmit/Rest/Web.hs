{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Rest.Web
  ( runSettings
  )
where

import           Cardano.Api.Pretty (textShow)

import           Cardano.BM.Trace (Trace, logInfo)
import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi(..))

import           Control.Exception (bracket)
import           Data.Streaming.Network (bindPortTCP)
import           Data.Text (Text)
import           Network.Socket (close, getSocketName, withSocketsDo)
import           Network.Wai.Handler.Warp (Settings, getHost, getPort, runSettingsSocket)

import           Servant (Application)
import qualified Cardano.Logging.Types as TraceD
import Cardano.Logging.Trace (traceWith)

-- | Like 'Network.Wai.Handler.Warp.runSettings', except with better logging.
runSettings :: Trace IO Text -> TraceD.Trace IO TraceSubmitApi -> Settings -> Application -> IO ()
runSettings trace trace' settings app =
  withSocketsDo $
    bracket
      (bindPortTCP (getPort settings) (getHost settings))
      close
      ( \socket -> do
          addr <- getSocketName socket
          logInfo trace $ "Web API listening on port " <> textShow addr
          traceWith trace' $ EndpointListeningOnPort addr
          runSettingsSocket settings socket app
      )
