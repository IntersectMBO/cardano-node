module Cardano.TxSubmit.Rest.Web
  ( runSettings
  )
where


import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi (..))

import           Control.Exception (bracket)
import           Data.Streaming.Network (bindPortTCP)
import           Network.Socket (close, getSocketName, withSocketsDo)
import           Network.Wai.Handler.Warp (Settings, getHost, getPort, runSettingsSocket)

import           Hermod.Tracing (traceWith)
import qualified Hermod.Tracing as TraceD
import           Servant (Application)

-- | Like 'Network.Wai.Handler.Warp.runSettings', except with better logging.
runSettings :: TraceD.Trace IO TraceSubmitApi -> Settings -> Application -> IO ()
runSettings trace settings app =
  withSocketsDo $
    bracket
      (bindPortTCP (getPort settings) (getHost settings))
      close
      ( \socket -> do
          addr <- getSocketName socket
          traceWith trace $ EndpointListeningOnPort addr
          runSettingsSocket settings socket app
      )
