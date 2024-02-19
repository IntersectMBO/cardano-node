{-# LANGUAGE DataKinds #-}

module Trace.Forward.Run.DataPoint.Forwarder
  ( forwardDataPointsInit
  , forwardDataPointsResp
  ) where

import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MiniProtocolCb (..), MuxMode (..), RunMiniProtocol (..))

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)

import           Trace.Forward.Configuration.DataPoint (ForwarderConfiguration (..))
import qualified Trace.Forward.Protocol.DataPoint.Codec as Forwarder
import qualified Trace.Forward.Protocol.DataPoint.Forwarder as Forwarder
import           Trace.Forward.Utils.DataPoint

forwardDataPointsInit
  :: ForwarderConfiguration
  -> DataPointStore
  -> RunMiniProtocol 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwardDataPointsInit config dpStore =
  InitiatorProtocolOnly $ runPeerWithDPStore config dpStore

forwardDataPointsResp
  :: ForwarderConfiguration
  -> DataPointStore
  -> RunMiniProtocol 'ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
forwardDataPointsResp config dpStore =
  ResponderProtocolOnly $ runPeerWithDPStore config dpStore

runPeerWithDPStore
  :: ForwarderConfiguration
  -> DataPointStore
  -> MiniProtocolCb ctx LBS.ByteString IO ()
runPeerWithDPStore config dpStore =
  MiniProtocolCb $ \_ctx channel ->
    runPeer
      (forwarderTracer config)
      (Forwarder.codecDataPointForward CBOR.encode CBOR.decode
                                       CBOR.encode CBOR.decode)
      channel
      (Forwarder.dataPointForwarderPeer $ readFromStore dpStore)
