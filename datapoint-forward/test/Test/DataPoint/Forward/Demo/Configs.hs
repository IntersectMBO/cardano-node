module Test.DataPoint.Forward.Demo.Configs
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           GHC.Conc (TVar)

import           DataPoint.Forward.Configuration

mkAcceptorConfig
  :: HowToConnect
  -> TVar Bool
  -> AcceptorConfiguration
mkAcceptorConfig ep weAreDone =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = ep
    , shouldWeStop      = weAreDone
    }

mkForwarderConfig
  :: HowToConnect
  -> ForwarderConfiguration
mkForwarderConfig ep =
  ForwarderConfiguration
    { forwarderTracer  = nullTracer
    , acceptorEndpoint = ep
    }
