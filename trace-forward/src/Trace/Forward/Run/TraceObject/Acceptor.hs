{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Trace.Forward.Run.TraceObject.Acceptor
  ( acceptTraceObjectsInit
  , acceptTraceObjectsResp
  ) where

import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MiniProtocolCb (..), MuxMode (..), RunMiniProtocol (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO, registerDelay)
import           Control.Exception (Exception, finally, throwIO)
import           Control.Monad.Extra (ifM)
import           Control.Monad.STM (atomically, check)
import qualified Data.ByteString.Lazy as LBS
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Trace.Forward.Configuration.TraceObject (AcceptorConfiguration (..))
import qualified Trace.Forward.Protocol.TraceObject.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.TraceObject.Codec as Acceptor
import           Trace.Forward.Protocol.TraceObject.Type
import           Trace.Forward.Utils.TraceObject (getTraceObjectsFromReply)

acceptTraceObjectsInit
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> (initiatorCtx -> [lo] -> IO ()) -- ^ The handler for accepted 'TraceObject's.
  -> (initiatorCtx -> IO ())         -- ^ The handler for exceptions from 'runPeer'.
  -> RunMiniProtocol 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
acceptTraceObjectsInit config loHandler peerErrorHandler =
  InitiatorProtocolOnly $ runPeerWithHandler config loHandler peerErrorHandler

acceptTraceObjectsResp
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> (responderCtx -> [lo] -> IO ()) -- ^ The handler for accepted 'TraceObject's.
  -> (responderCtx -> IO ())         -- ^ The handler for exceptions from 'runPeer'.
  -> RunMiniProtocol 'ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
acceptTraceObjectsResp config loHandler peerErrorHandler =
  ResponderProtocolOnly $ runPeerWithHandler config loHandler peerErrorHandler

runPeerWithHandler
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> (ctx -> [lo] -> IO ())
  -> (ctx -> IO ())
  -> MiniProtocolCb ctx LBS.ByteString IO ()
runPeerWithHandler config@AcceptorConfiguration{acceptorTracer, shouldWeStop} loHandler peerErrorHandler =
  MiniProtocolCb $ \ctx channel ->
    timeoutWhenStopped
      shouldWeStop
      15_000 -- 15sec
      $ runPeer
          acceptorTracer
          (Acceptor.codecTraceObjectForward CBOR.encode CBOR.decode
                                            CBOR.encode CBOR.decode)
          channel
          (Acceptor.traceObjectAcceptorPeer $ acceptorActions config (loHandler ctx))
        `finally` peerErrorHandler ctx

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

-- | Timeout shutdown of an action. It can run only for specified milliseconds
--   once the 'TVar' is set to 'True'.
timeoutWhenStopped :: TVar Bool
                   -> Int -- ^ Timeout, in milliseconds.
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
