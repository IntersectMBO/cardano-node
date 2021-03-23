module Test.MkConfig
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
    , requestFrequency  = secondsToNominalDiffTime 1
    , whatToRequest     = request
    , actionOnResponse  = \_ -> return ()
    , shouldWeStop      = weAreDone
    , actionOnDone      = putStrLn "Acceptor: we are done!"
    }

mkForwarderConfig
  :: HowToConnect
  -> ForwarderConfiguration Text
mkForwarderConfig endpoint =
  ForwarderConfiguration
    { forwarderTracer    = nullTracer
    , acceptorEndpoint   = endpoint
    , reConnectFrequency = secondsToNominalDiffTime 1
    , actionOnRequest    = \_ -> return ()
    }
