{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Test.Trace.Forward.Protocol.TraceObject.Tests
  ( tests
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.ST (runST)
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (nullTracer)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)

import           Trace.Forward.Protocol.TraceObject.Acceptor
import           Trace.Forward.Protocol.TraceObject.Forwarder
import           Trace.Forward.Protocol.TraceObject.Codec
import           Trace.Forward.Protocol.TraceObject.Type

import           Test.Trace.Forward.Protocol.TraceObject.Codec ()
import           Test.Trace.Forward.Protocol.TraceObject.Direct
import           Test.Trace.Forward.Protocol.TraceObject.Examples
import           Test.Trace.Forward.Protocol.TraceObject.Item

import           Test.Trace.Forward.Protocol.Common

tests :: TestTree
tests = testGroup "Trace.Forward.Protocol.TraceObject"
  [ testProperty "codec"          prop_codec_TraceObjectForward
  , testProperty "codec 2-splits" prop_codec_splits2_TraceObjectForward
  , testProperty "codec 3-splits" (withMaxSuccess 33 prop_codec_splits3_TraceObjectForward)
  , testProperty "direct"         prop_direct_TraceObjectForward
  , testProperty "connect"        prop_connect_TraceObjectForward
  , testProperty "channel ST"     prop_channel_ST_TraceObjectForward
  , testProperty "channel IO"     prop_channel_IO_TraceObjectForward
  ]

prop_codec_TraceObjectForward :: AnyMessage (TraceObjectForward TraceItem) -> Bool
prop_codec_TraceObjectForward msg = runST $
  prop_codecM
    (codecTraceObjectForward CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode)
    msg

prop_codec_splits2_TraceObjectForward
  :: AnyMessage (TraceObjectForward TraceItem)
  -> Bool
prop_codec_splits2_TraceObjectForward msg = runST $
  prop_codec_splitsM
    splits2
    (codecTraceObjectForward CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode)
    msg

prop_codec_splits3_TraceObjectForward
  :: AnyMessage (TraceObjectForward TraceItem)
  -> Bool
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
    runSimOrThrow
      (direct
         traceObjectForwarderCount
         (traceObjectAcceptorApply f 0 n))
  ===
    (n, foldr ($) 0 (replicate n f))

prop_connect_TraceObjectForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Bool
prop_connect_TraceObjectForward f (NonNegative n) =
  case runSimOrThrow
         (connect [] []
            (traceObjectForwarderPeer   traceObjectForwarderCount)
            (traceObjectAcceptorPeer  $ traceObjectAcceptorApply f 0 n)) of
    (s, c, TerminalStates SingDone SingDone) -> (s, c) == (n, foldr ($) 0 (replicate n f))

prop_channel
  :: ( MonadST    m
     , MonadAsync m
     , MonadLabelledSTM m
     , MonadMask  m
     , MonadThrow (STM m)
     )
  => (Int -> Int)
  -> Int
  -> m Property
prop_channel f n = do
  (s, c) <- runConnectedPeers createConnectedChannels
                              nullTracer
                              (codecTraceObjectForward CBOR.encode CBOR.decode
                                                       CBOR.encode CBOR.decode)
                              forwarder acceptor
  return $ (s, c) === (n, foldr ($) 0 (replicate n f))
 where
  forwarder = traceObjectForwarderPeer   traceObjectForwarderCount
  acceptor  = traceObjectAcceptorPeer  $ traceObjectAcceptorApply f 0 n

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
