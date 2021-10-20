{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module DataPoint.Forward.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptDataPoints
  , acceptDataPointsInit
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (race_, wait)
import           Control.Monad.Extra (ifM)
import           Control.Monad.STM (atomically, check)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVar, readTVarIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                                                               simpleSingletonVersions)

import qualified DataPoint.Forward.Protocol.Acceptor as Acceptor
import qualified DataPoint.Forward.Protocol.Codec as Acceptor
import           DataPoint.Forward.Protocol.Type (DataPointName)
import           DataPoint.Forward.Configuration (AcceptorConfiguration (..), HowToConnect (..))
import           DataPoint.Forward.Utils (DataPointAsker (..))

listenToForwarder
  :: IOManager
  -> AcceptorConfiguration
  -> DataPointAsker
  -> IO ()
listenToForwarder iomgr config@AcceptorConfiguration{forwarderEndpoint} dpAsker = do
  let (LocalPipe localPipe) = forwarderEndpoint
      snocket = localSnocket iomgr
      address = localAddressFromPath localPipe
  doListenToForwarder snocket address noTimeLimitsHandshake app
 where
  app =
    -- TODO: There's _shouldStopSTM and 'shouldWeStop' in
    -- 'AcceptorConfiguration'.  Currently 'ouroboros-network' does not exposes
    -- the write end of `_shouldStopSTM`, if it did we could use it instead of
    -- 'shouldWeStop'.
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
          { miniProtocolNum    = MiniProtocolNum 1
          , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
          , miniProtocolRun    = acceptDataPoints config dpAsker
          }
      ]

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address timeLimits app = do
  networkState <- newNetworkMutableState
  race_ (cleanNetworkMutableState networkState)
        $ withServerNode
            snocket
            nullNetworkServerTracers
            networkState
            (AcceptedConnectionsLimit maxBound maxBound 0)
            address
            unversionedHandshakeCodec
            timeLimits
            (cborTermVersionDataCodec unversionedProtocolDataCodec)
            acceptableVersion
            (simpleSingletonVersions
              UnversionedProtocol
              UnversionedProtocolData
              (SomeResponderApplication app))
            nullErrorPolicies
            $ \_ serverAsync -> wait serverAsync -- Block until async exception.

acceptDataPoints
  :: AcceptorConfiguration
  -> DataPointAsker
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptDataPoints config dpAsker =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (acceptorTracer config)
        (Acceptor.codecDataPointForward CBOR.encode CBOR.decode
                                        CBOR.encode CBOR.decode)
        channel
        (Acceptor.dataPointAcceptorPeer $ acceptorActions config dpAsker [])

acceptDataPointsInit
  :: AcceptorConfiguration
  -> DataPointAsker
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
acceptDataPointsInit config dpAsker =
  InitiatorProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (acceptorTracer config)
        (Acceptor.codecDataPointForward CBOR.encode CBOR.decode
                                        CBOR.encode CBOR.decode)
        channel
        (Acceptor.dataPointAcceptorPeer $ acceptorActions config dpAsker [])

acceptorActions
  :: AcceptorConfiguration
  -> DataPointAsker
  -> [DataPointName]
  -> Acceptor.DataPointAcceptor IO ()
acceptorActions config@AcceptorConfiguration{shouldWeStop}
                dpAsker@DataPointAsker{askDataPoints, dataPointsNames, dataPointsAreHere, dataPointsReply}
                dpNames =
  Acceptor.SendMsgDataPointsRequest dpNames $ \replyWithDataPoints -> do
    -- Reply with 'DataPoint's is here, update the asker.
    atomically $ do
      modifyTVar' dataPointsReply $ const replyWithDataPoints
      -- To notify external context that answer was received.
      modifyTVar' dataPointsAreHere $ const True
      -- To prevent new automatic request.
      modifyTVar' askDataPoints $ const False
    ifM (readTVarIO shouldWeStop)
      (return $ Acceptor.SendMsgDone $ return ())
      $ do
        -- Block here until external context ask for 'DataPoint's again.
        atomically $ readTVar askDataPoints >>= check
        -- Ok, external context asked for 'DataPoint's again.
        dpNames' <- atomically $ do
          modifyTVar' dataPointsAreHere $ const False
          readTVar dataPointsNames
        return $ acceptorActions config dpAsker dpNames'
