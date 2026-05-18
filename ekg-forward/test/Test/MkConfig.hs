module Test.MkConfig
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Tracer (nullTracer)
import           Data.Time.Clock (secondsToNominalDiffTime)

import           System.Metrics.Configuration (AcceptorConfiguration (..),
                                               ForwarderConfiguration (..),
                                               HowToConnect (..))
import           System.Metrics.ReqResp (Request (..))

mkAcceptorConfig
  :: HowToConnect
  -> TVar Bool
  -> Request
  -> AcceptorConfiguration
mkAcceptorConfig endpoint weAreDone request =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = endpoint
    , requestFrequency  = secondsToNominalDiffTime 1
    , whatToRequest     = request
    , shouldWeStop      = weAreDone
    }

mkForwarderConfig
  :: HowToConnect
  -> ForwarderConfiguration
mkForwarderConfig endpoint =
  ForwarderConfiguration
    { forwarderTracer    = nullTracer
    , acceptorEndpoint   = endpoint
    , reConnectFrequency = secondsToNominalDiffTime 1
    , actionOnRequest    = \_ -> return ()
    , useDummyForwarder  = False
    }
