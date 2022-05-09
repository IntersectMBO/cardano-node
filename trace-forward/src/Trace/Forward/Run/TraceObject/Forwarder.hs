{-# LANGUAGE DataKinds #-}

module Trace.Forward.Run.TraceObject.Forwarder
  ( forwardTraceObjectsInit
  , forwardTraceObjectsResp
  ) where

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..), RunMiniProtocol (..))

import qualified Trace.Forward.Protocol.TraceObject.Forwarder as Forwarder
import qualified Trace.Forward.Protocol.TraceObject.Codec as Forwarder
import           Trace.Forward.Utils.TraceObject
import           Trace.Forward.Configuration.TraceObject (ForwarderConfiguration (..))

forwardTraceObjectsInit
  :: CBOR.Serialise lo
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardTraceObjectsInit config sink =
  InitiatorProtocolOnly $ runPeerWithSink config sink

forwardTraceObjectsResp
  :: CBOR.Serialise lo
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
forwardTraceObjectsResp config sink =
  ResponderProtocolOnly $ runPeerWithSink config sink

runPeerWithSink
  :: CBOR.Serialise lo
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> MuxPeer LBS.ByteString IO ()
runPeerWithSink config sink =
  MuxPeerRaw $ \channel ->
    runPeer
      (forwarderTracer config)
      (Forwarder.codecTraceObjectForward CBOR.encode CBOR.decode
                                         CBOR.encode CBOR.decode)
      channel
      (Forwarder.traceObjectForwarderPeer $ readFromSink sink)
