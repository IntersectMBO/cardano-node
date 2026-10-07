{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Test.Trace.Forward.Protocol.TraceObject.Tests
  ( tests
  ) where

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)

import qualified Codec.Serialise as CBOR
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)
import           Data.IORef (modifyIORef', newIORef, readIORef)
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.Properties
import           Network.TypedProtocol.Proofs

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Trace.Forward.Protocol.Common
import           Test.Trace.Forward.Protocol.TraceObject.Codec ()
import           Test.Trace.Forward.Protocol.TraceObject.Direct
import           Test.Trace.Forward.Protocol.TraceObject.Examples
import           Test.Trace.Forward.Protocol.TraceObject.Item

import           Trace.Forward.Configuration.TraceObject (ForwarderConfiguration (..))
import           Trace.Forward.Protocol.TraceObject.Acceptor
import           Trace.Forward.Protocol.TraceObject.Codec
import           Trace.Forward.Protocol.TraceObject.Forwarder
import           Trace.Forward.Protocol.TraceObject.Type
import           Trace.Forward.Utils.ForwardSink (ForwardSink (..))
import           Trace.Forward.Utils.TraceObject (initForwardSink, writeToSink)

tests :: TestTree
tests = testGroup "Trace.Forward.Protocol.TraceObject"
  [ testProperty "full queue reports dropped prefix and retains new item" (once prop_overflow_TraceObjectForward)
  , testProperty "codec"          prop_codec_TraceObjectForward
  , testProperty "codec 2-splits" prop_codec_splits2_TraceObjectForward
  , testProperty "codec 3-splits" (withNumTests 33 prop_codec_splits3_TraceObjectForward)
  , testProperty "direct"         prop_direct_TraceObjectForward
  , testProperty "connect"        prop_connect_TraceObjectForward
  , testProperty "channel ST"     prop_channel_ST_TraceObjectForward
  , testProperty "channel IO"     prop_channel_IO_TraceObjectForward
  ]

prop_codec_TraceObjectForward :: AnyMessage (TraceObjectForward TraceItem) -> Property
prop_codec_TraceObjectForward msg = runST $
  prop_codecM
    (codecTraceObjectForward CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode)
    msg

prop_codec_splits2_TraceObjectForward
  :: AnyMessage (TraceObjectForward TraceItem)
  -> Property
prop_codec_splits2_TraceObjectForward msg = runST $
  prop_codec_splitsM
    splits2
    (codecTraceObjectForward CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode)
    msg

prop_codec_splits3_TraceObjectForward
  :: AnyMessage (TraceObjectForward TraceItem)
  -> Property
prop_codec_splits3_TraceObjectForward msg = runST $
  prop_codec_splitsM
    splits3
    (codecTraceObjectForward CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode)
    msg

prop_direct_TraceObjectForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Property
prop_direct_TraceObjectForward f (NonNegative n) =
  runSimOrThrow (prop_direct f n)

prop_direct
  :: MonadSTM m
  => (Int -> Int)
  -> Int
  -> m Property
prop_direct f n = do
  fwcount <- traceObjectForwarderCount
  result <- direct fwcount (traceObjectAcceptorApply f 0 n)
  return $ result === (n, foldr ($) 0 (replicate n f))

prop_connect_TraceObjectForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Bool
prop_connect_TraceObjectForward f (NonNegative n) =
  runSimOrThrow (prop_connect f n)

prop_connect
  :: ( MonadST   m
     , MonadAsync m
     )
  => (Int -> Int)
  -> Int
  -> m Bool
prop_connect f n = do
  forwarder <- traceObjectForwarderPeer <$> traceObjectForwarderCount
  result <- connect forwarder (traceObjectAcceptorPeer $ traceObjectAcceptorApply f 0 n)
  case result of
    (s, c, TerminalStates SingDone SingDone) ->
      pure $ (s, c) == (n, foldr ($) 0 (replicate n f))

prop_channel
  :: ( MonadST    m
     , MonadAsync m
     , MonadCatch m
     , MonadEvaluate m
     )
  => (Int -> Int)
  -> Int
  -> m Property
prop_channel f n = do
  forwarder <- traceObjectForwarderPeer <$> traceObjectForwarderCount
  (s, c) <- runConnectedPeers createConnectedChannels
                              nullTracer
                              (codecTraceObjectForward CBOR.encode CBOR.decode
                                                       CBOR.encode CBOR.decode)
                              forwarder acceptor
  return $ (s, c) === (n, foldr ($) 0 (replicate n f))
 where
  acceptor = traceObjectAcceptorPeer $ traceObjectAcceptorApply f 0 n

prop_channel_ST_TraceObjectForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Property
prop_channel_ST_TraceObjectForward f (NonNegative n) =
  runSimOrThrow (prop_channel f n)

prop_channel_IO_TraceObjectForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Property
prop_channel_IO_TraceObjectForward f (NonNegative n) =
  ioProperty (prop_channel f n)

#if ! MIN_VERSION_QuickCheck(2,18,0)
withNumTests :: Testable prop => Int -> prop -> Property
withNumTests = withMaxSuccess
#endif

-- Exercise the unchanged production overload policy without a consumer.
-- Filling the same capacity as the stress test must flush exactly that prefix.
prop_overflow_TraceObjectForward :: Property
prop_overflow_TraceObjectForward = ioProperty $ do
  droppedRef <- newIORef []
  sink <- initForwardSink
    ForwarderConfiguration { forwarderTracer = nullTracer, queueSize = 768 }
    (\items -> modifyIORef' droppedRef (items :))
  mapM_ (writeToSink sink) ([0 .. 768] :: [Int])
  dropped <- readIORef droppedRef
  retained <- STM.atomically $ TBQueue.flushTBQueue (forwardQueue sink)
  pure $ conjoin
    [ counterexample "Overflow callback must report exactly the full old queue"
        (dropped === [[0 .. 767]])
    , counterexample "The new object must remain queued after the drop"
        (retained === [768])
    ]
