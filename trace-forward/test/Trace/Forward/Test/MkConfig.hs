module Trace.Forward.Test.MkConfig
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           Data.IORef (IORef)
import           Data.Text (Text)

import           Cardano.BM.Data.LogItem (LogObject (..))

import           Trace.Forward.Configuration (AcceptorConfiguration (..),
                                              ForwarderConfiguration (..),
                                              HowToConnect (..))
import           Trace.Forward.Protocol.Type (Request (..))

mkAcceptorConfig
  :: HowToConnect
  -> IORef Bool
  -> Request
  -> AcceptorConfiguration (LogObject Text)
mkAcceptorConfig endpoint weAreDone request =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = endpoint
    , whatToRequest     = request
    , actionOnReply     = const $ return ()
    , shouldWeStop      = weAreDone
    , actionOnDone      = return ()
    }

mkForwarderConfig
  :: HowToConnect
  -> ForwarderConfiguration (LogObject Text)
mkForwarderConfig endpoint =
  ForwarderConfiguration
    { forwarderTracer  = nullTracer
    , acceptorEndpoint = endpoint
    , nodeBasicInfo    = return [("NodeName", "node-1")]
    , actionOnRequest  = const $ return ()
    }
