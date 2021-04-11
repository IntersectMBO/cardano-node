module Trace.Forward.Test.MkConfig
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           Data.IORef (IORef)
import           Data.Text (Text)
import           Data.Time.Clock (secondsToNominalDiffTime)

import           Trace.Forward.Configuration (AcceptorConfiguration (..),
                                              ForwarderConfiguration (..),
                                              HowToConnect (..))
import           Trace.Forward.ReqResp (Request (..))

mkAcceptorConfig
  :: HowToConnect
  -> IORef Bool
  -> Request
  -> AcceptorConfiguration Text
mkAcceptorConfig endpoint weAreDone request =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = endpoint
    , requestFrequency  = secondsToNominalDiffTime 0.05
    , whatToRequest     = request
    , actionOnResponse  = const $ return ()
    , shouldWeStop      = weAreDone
    , actionOnDone      = return ()
    }

mkForwarderConfig
  :: HowToConnect
  -> ForwarderConfiguration Text
mkForwarderConfig endpoint =
  ForwarderConfiguration
    { forwarderTracer    = nullTracer
    , acceptorEndpoint   = endpoint
    , reConnectFrequency = secondsToNominalDiffTime 1
    , actionOnRequest    = const $ return ()
    }
