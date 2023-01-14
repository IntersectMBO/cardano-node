{-# LANGUAGE DataKinds #-}

module Trace.Forward.Run.DataPoint.Forwarder
  ( forwardDataPointsInit
  , forwardDataPointsResp
  ) where

import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..), RunMiniProtocol (..))

import           Trace.Forward.Configuration.DataPoint (ForwarderConfiguration (..))
import           Trace.Forward.Utils.DataPoint
import qualified Trace.Forward.Protocol.DataPoint.Forwarder as Forwarder
import qualified Trace.Forward.Protocol.DataPoint.Codec as Forwarder

forwardDataPointsInit
  :: ForwarderConfiguration
  -> DataPointStore
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardDataPointsInit config dpStore =
  InitiatorProtocolOnly $ runPeerWithDPStore config dpStore

forwardDataPointsResp
  :: ForwarderConfiguration
  -> DataPointStore
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
forwardDataPointsResp config dpStore =
  ResponderProtocolOnly $ runPeerWithDPStore config dpStore

runPeerWithDPStore
  :: ForwarderConfiguration
  -> DataPointStore
  -> MuxPeer LBS.ByteString IO ()
runPeerWithDPStore config dpStore = 
  MuxPeerRaw $ \channel ->
    runPeer
      (forwarderTracer config)
      (Forwarder.codecDataPointForward CBOR.encode CBOR.decode
                                       CBOR.encode CBOR.decode)
      channel
      (Forwarder.dataPointForwarderPeer $ readFromStore dpStore)
