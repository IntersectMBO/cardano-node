module Trace.Forward.Configuration
  ( AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  , HowToConnect (..)
  , Host
  , Port
  ) where

import           Control.Tracer (Tracer)
import           Data.IORef (IORef)
import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Word (Word16)
import           Ouroboros.Network.Driver (TraceSendRecv)

import           Trace.Forward.Protocol.Type (TraceForward)
import           Trace.Forward.ReqResp (Request, Response)

type Host = Text
type Port = Word16

-- | Specifies how to connect to the peer.
data HowToConnect
  = LocalPipe    !FilePath    -- ^ Local pipe (UNIX or Windows).
  | RemoteSocket !Host !Port  -- ^ Remote socket (host and port).

-- | Acceptor configuration.
data AcceptorConfiguration a = AcceptorConfiguration
  { -- | The tracer that will be used by the acceptor in its network layer.
    -- For more info about tracers please read its [documentation](https://github.com/input-output-hk/iohk-monitoring-framework/tree/master/contra-tracer).
    acceptorTracer    :: !(Tracer IO (TraceSendRecv (TraceForward Request (Response a))))
    -- | The endpoint that will be used to listen to the forwarder.
  , forwarderEndpoint :: !HowToConnect
    -- | Specifies how often the acceptor will ask the framework for new 'LogObject's.
    -- It can be specified as seconds or as fraction of second.
  , requestFrequency  :: !NominalDiffTime
    -- | The request specifies how many 'LogObject's will be requested.
  , whatToRequest     :: !Request
    -- | Additional action that will be performed every time the acceptor will
    -- receive the response from the forwarder.
  , actionOnResponse  :: !(Response a -> IO ())
    -- | 'IORef' that can be used as a brake: if an external thread will set it to
    -- 'True', the acceptor will send 'MsgDone' message to the forwarder and their
    -- session will be closed.
  , shouldWeStop      :: !(IORef Bool)
    -- | An action that will be performed before sending 'MsgDone' message.
  , actionOnDone      :: !(IO ())
  }

-- | Forwarder configuration.
data ForwarderConfiguration a = ForwarderConfiguration
  { -- | The tracer that will be used by the forwarder in its network layer.
    forwarderTracer    :: !(Tracer IO (TraceSendRecv (TraceForward Request (Response a))))
    -- | The endpoint that will be used to connect to the acceptor.
  , acceptorEndpoint   :: !HowToConnect
    -- | If the connection with the acceptor will fail, the forwarder will attempt
    -- to re-establish the connection after this delay.
    -- It can be specified as seconds or as fraction of second.
  , reConnectFrequency :: !NominalDiffTime
    -- | Additional action that will be performed every time the forwarder will
    -- receive the request from the acceptor.
  , actionOnRequest    :: !(Request -> IO ())
  }
