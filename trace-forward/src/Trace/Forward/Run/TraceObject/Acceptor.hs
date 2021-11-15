{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Trace.Forward.Run.TraceObject.Acceptor
  ( acceptTraceObjectsInit
  , acceptTraceObjectsResp
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (race)
import           Control.Monad.Extra (ifM)
import           Control.Monad.STM (atomically, check)
import           Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO, registerDelay)
import           Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..), RunMiniProtocol (..))
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import qualified Trace.Forward.Protocol.TraceObject.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.TraceObject.Codec as Acceptor
import           Trace.Forward.Protocol.TraceObject.Type
import           Trace.Forward.Utils.TraceObject (getTraceObjectsFromReply)
import           Trace.Forward.Configuration.TraceObject (AcceptorConfiguration (..))

acceptTraceObjectsInit
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> ([lo] -> IO ())          -- ^ The handler for accepted 'TraceObject's.
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
acceptTraceObjectsInit config loHandler =
  InitiatorProtocolOnly $ runPeerWithHandler config loHandler

acceptTraceObjectsResp
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> ([lo] -> IO ())          -- ^ The handler for accepted 'TraceObject's.
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptTraceObjectsResp config loHandler =
  ResponderProtocolOnly $ runPeerWithHandler config loHandler

runPeerWithHandler
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> MuxPeer LBS.ByteString IO ()
runPeerWithHandler config@AcceptorConfiguration{acceptorTracer, shouldWeStop} loHandler =
  MuxPeerRaw $ \channel ->
    timeoutWhenStopped
      shouldWeStop
      15_000 -- 15sec
      $ runPeer
          acceptorTracer
          (Acceptor.codecTraceObjectForward CBOR.encode CBOR.decode
                                            CBOR.encode CBOR.decode)
          channel
          (Acceptor.traceObjectAcceptorPeer $ acceptorActions config loHandler)

acceptorActions
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> ([lo] -> IO ())          -- ^ The handler for accepted 'TraceObject's.
  -> Acceptor.TraceObjectAcceptor lo IO ()
acceptorActions config@AcceptorConfiguration{whatToRequest, shouldWeStop} loHandler =
  Acceptor.SendMsgTraceObjectsRequest TokBlocking whatToRequest $ \replyWithTraceObjects -> do
    loHandler $ getTraceObjectsFromReply replyWithTraceObjects
    ifM (readTVarIO shouldWeStop)
      (return $ Acceptor.SendMsgDone $ return ())
      (return $ acceptorActions config loHandler)

data Timeout = Timeout
  deriving (Typeable, Show)

instance Exception Timeout where

-- | Timeout shutdown of an action. It can run only for specified miliseconds
--   once the 'TVar' is set to 'True'.
timeoutWhenStopped :: TVar Bool
                   -> Int -- ^ Timeout, in miliseconds.
                   -> IO a
                   -> IO a
timeoutWhenStopped stopVar delay action =
  either id id <$>
    race action
         ( do atomically (readTVar stopVar >>= check)
              v <- registerDelay delay    
              atomically (readTVar v >>= check) 
              throwIO Timeout
         )
