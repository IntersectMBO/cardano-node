{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Trace.Forward.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptTraceObjects
  , acceptTraceObjectsInit
  , Timeout (..)
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (race, race_, wait)
import           Control.Monad.Extra (ifM)
import           Control.Monad.STM (atomically, check)
import           Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO, registerDelay)
import           Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Typeable (Typeable)
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
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import qualified Trace.Forward.Protocol.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.Codec as Acceptor
import           Trace.Forward.Protocol.Type
import           Trace.Forward.Queue (getTraceObjects)
import           Trace.Forward.Configuration (AcceptorConfiguration (..), HowToConnect (..))

listenToForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  =>  IOManager
  -> AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> (NodeInfo -> IO ())
  -> IO ()
listenToForwarder iomgr config@AcceptorConfiguration{forwarderEndpoint} loHandler niHandler = do
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
          , miniProtocolRun    = acceptTraceObjects config loHandler niHandler
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

acceptTraceObjects
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> (NodeInfo -> IO ())
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptTraceObjects config loHandler niHandler =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel ->
      timeoutWhenStopped
        (shouldWeStop config)
        15_000 -- 15sec
        $ runPeer
            (acceptorTracer config)
            (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                        CBOR.encode CBOR.decode
                                        CBOR.encode CBOR.decode)
            channel
            (Acceptor.traceAcceptorPeer $
              acceptorActions config loHandler niHandler True)

acceptTraceObjectsInit
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> (NodeInfo -> IO ())
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
acceptTraceObjectsInit config loHandler niHandler =
  InitiatorProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (acceptorTracer config)
        (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                    CBOR.encode CBOR.decode
                                    CBOR.encode CBOR.decode)
        channel
        (Acceptor.traceAcceptorPeer $
          acceptorActions config loHandler niHandler True)

acceptorActions
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> ([lo] -> IO ())          -- ^ The handler for accepted 'TraceObject's.
  -> (NodeInfo -> IO ())      -- ^ The handler for accepted info about the node.
  -> Bool                     -- ^ The flag for node's info request: only once in the beginning.
  -> Acceptor.TraceAcceptor lo IO ()
acceptorActions config@AcceptorConfiguration{whatToRequest, shouldWeStop} loHandler niHandler askForNI =
  -- We are able to send request for:
  -- 1. node's info,
  -- 2. new 'TraceObject's.
  -- But request for node's info should be sent only once (in the beginning of session).
  if askForNI
    then
      Acceptor.SendMsgNodeInfoRequest $ \replyWithNI -> do
        niHandler replyWithNI
        checkIfWeShouldStop
    else
      Acceptor.SendMsgTraceObjectsRequest TokBlocking whatToRequest $ \replyWithTraceObjects -> do
        loHandler $ getTraceObjects replyWithTraceObjects
        checkIfWeShouldStop
 where
  checkIfWeShouldStop =
    ifM (readTVarIO shouldWeStop)
      (return $ Acceptor.SendMsgDone $ return ())
      (return $ acceptorActions config loHandler niHandler False)

data Timeout = Timeout
  deriving (Typeable, Show)

instance Exception Timeout where

-- | Timeout shutdown of an action.  It can run only for specified miliseconds
-- once the 'TVar' is set to 'True'.
--
timeoutWhenStopped :: TVar Bool
                   -> Int -- timeout in miliseconds
                   -> IO a
                   -> IO a
timeoutWhenStopped stopVar delay io =
  either id id <$>
    race io
         ( do atomically (readTVar stopVar >>= check)
              v <- registerDelay delay    
              atomically (readTVar v >>= check) 
              throwIO Timeout
         )
