{-# LANGUAGE DataKinds #-}

module Trace.Forward.Run.TraceObject.Forwarder
  ( forwardTraceObjectsInit
  , forwardTraceObjectsResp
  ) where

import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MiniProtocolCb (..), MuxMode (..), RunMiniProtocol (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)

import           Trace.Forward.Configuration.TraceObject (ForwarderConfiguration (..))
import qualified Trace.Forward.Protocol.TraceObject.Codec as Forwarder
import qualified Trace.Forward.Protocol.TraceObject.Forwarder as Forwarder
import           Trace.Forward.Utils.TraceObject

forwardTraceObjectsInit
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> RunMiniProtocol 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwardTraceObjectsInit config sink =
  InitiatorProtocolOnly $ runPeerWithSink config sink

forwardTraceObjectsResp
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> RunMiniProtocol 'ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
forwardTraceObjectsResp config sink =
  ResponderProtocolOnly $ runPeerWithSink config sink

runPeerWithSink
  :: (ShowProxy lo, CBOR.Serialise lo)
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> MiniProtocolCb ctx LBS.ByteString IO ()
runPeerWithSink config sink =
  MiniProtocolCb $ \_ctx channel ->
    runPeer
      (forwarderTracer config)
      (Forwarder.codecTraceObjectForward CBOR.encode CBOR.decode
                                         CBOR.encode CBOR.decode)
      channel
      (Forwarder.traceObjectForwarderPeer $ readFromSink sink)
