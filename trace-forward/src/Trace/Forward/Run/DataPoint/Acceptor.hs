{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Run.DataPoint.Acceptor
  ( acceptDataPointsInit
  , acceptDataPointsResp
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Monad.Extra (ifM)
import           Control.Monad.STM (atomically, check)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVar, readTVarIO)
import           Control.Concurrent.STM.TMVar (putTMVar)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..), RunMiniProtocol (..))
import           Ouroboros.Network.Driver.Simple (runPeer)

import qualified Trace.Forward.Protocol.DataPoint.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.DataPoint.Codec as Acceptor
import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)
import           Trace.Forward.Configuration.DataPoint (AcceptorConfiguration (..))
import           Trace.Forward.Utils.DataPoint (DataPointAsker (..))

acceptDataPointsInit
  :: AcceptorConfiguration
  -> DataPointAsker
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
acceptDataPointsInit config dpAsker =
  InitiatorProtocolOnly $ runPeerWithAsker config dpAsker

acceptDataPointsResp
  :: AcceptorConfiguration
  -> DataPointAsker
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptDataPointsResp config dpAsker =
  ResponderProtocolOnly $ runPeerWithAsker config dpAsker

runPeerWithAsker
  :: AcceptorConfiguration
  -> DataPointAsker
  -> MuxPeer LBS.ByteString IO ()
runPeerWithAsker config dpAsker = 
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
                dpAsker@DataPointAsker{askDataPoints, dataPointsNames, dataPointsReply}
                dpNames =
  Acceptor.SendMsgDataPointsRequest dpNames $ \replyWithDataPoints -> do
    -- Ok, reply with 'DataPoint's is already here, update the asker.
    atomically $ do
      putTMVar dataPointsReply replyWithDataPoints
      -- To prevent new automatic request.
      modifyTVar' askDataPoints $ const False
    ifM (readTVarIO shouldWeStop)
      (return $ Acceptor.SendMsgDone $ return ())
      $ do
        -- Block here until external context explicitly ask for 'DataPoint's again.
        atomically $ readTVar askDataPoints >>= check
        -- Ok, external context asked for 'DataPoint's, take their names.
        dpNames' <- readTVarIO dataPointsNames
        -- Ask.
        return $ acceptorActions config dpAsker dpNames'
