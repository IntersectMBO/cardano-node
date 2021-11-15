{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Trace.Forward.Protocol.DataPoint.Tests
  ( tests
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)

import           Trace.Forward.Protocol.DataPoint.Acceptor
import           Trace.Forward.Protocol.DataPoint.Forwarder
import           Trace.Forward.Protocol.DataPoint.Codec
import           Trace.Forward.Protocol.DataPoint.Type

import           Test.Trace.Forward.Protocol.DataPoint.Codec ()
import           Test.Trace.Forward.Protocol.DataPoint.Direct
import           Test.Trace.Forward.Protocol.DataPoint.Examples

import           Test.Trace.Forward.Protocol.Common

tests :: TestTree
tests = testGroup "Trace.Forward.Protocol.DataPoint"
  [ testProperty "codec"          prop_codec_DataPointForward
  , testProperty "codec 2-splits" prop_codec_splits2_DataPointForward
  , testProperty "codec 3-splits" (withMaxSuccess 33 prop_codec_splits3_DataPointForward)
  , testProperty "direct"         prop_direct_DataPointForward
  , testProperty "connect"        prop_connect_DataPointForward
  , testProperty "channel ST"     prop_channel_ST_DataPointForward
  , testProperty "channel IO"     prop_channel_IO_DataPointForward
  ]

prop_codec_DataPointForward
  :: AnyMessageAndAgency DataPointForward
  -> Bool
prop_codec_DataPointForward msg = runST $
  prop_codecM
    (codecDataPointForward CBOR.encode CBOR.decode
                           CBOR.encode CBOR.decode)
    msg

prop_codec_splits2_DataPointForward
  :: AnyMessageAndAgency DataPointForward
  -> Bool
prop_codec_splits2_DataPointForward msg = runST $
  prop_codec_splitsM
    splits2
    (codecDataPointForward CBOR.encode CBOR.decode
                           CBOR.encode CBOR.decode)
    msg


prop_codec_splits3_DataPointForward
  :: AnyMessageAndAgency DataPointForward
  -> Bool
prop_codec_splits3_DataPointForward msg = runST $
  prop_codec_splitsM
    splits3
    (codecDataPointForward CBOR.encode CBOR.decode
                           CBOR.encode CBOR.decode)
    msg

prop_direct_DataPointForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Property
prop_direct_DataPointForward f (NonNegative n) =
    runSimOrThrow
      (direct
         dataPointForwarderCount
         (dataPointAcceptorApply f 0 n))
  ===
    (n, foldr ($) 0 (replicate n f))

prop_connect_DataPointForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Bool
prop_connect_DataPointForward f (NonNegative n) =
  case runSimOrThrow
         (connect
            (dataPointForwarderPeer   dataPointForwarderCount)
            (dataPointAcceptorPeer  $ dataPointAcceptorApply f 0 n)) of
    (s, c, TerminalStates TokDone TokDone) -> (s, c) == (n, foldr ($) 0 (replicate n f))

prop_channel
  :: ( MonadST    m
     , MonadAsync m
     , MonadCatch m
     )
  => (Int -> Int)
  -> Int
  -> m Property
prop_channel f n = do
  (s, c) <- runConnectedPeers createConnectedChannels
                              nullTracer
                              (codecDataPointForward CBOR.encode CBOR.decode
                                                     CBOR.encode CBOR.decode)
                              forwarder acceptor
  return $ (s, c) === (n, foldr ($) 0 (replicate n f))
 where
  forwarder = dataPointForwarderPeer   dataPointForwarderCount
  acceptor  = dataPointAcceptorPeer  $ dataPointAcceptorApply f 0 n

prop_channel_ST_DataPointForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Property
prop_channel_ST_DataPointForward f (NonNegative n) =
  runSimOrThrow (prop_channel f n)

prop_channel_IO_DataPointForward
  :: (Int -> Int)
  -> NonNegative Int
  -> Property
prop_channel_IO_DataPointForward f (NonNegative n) =
  ioProperty (prop_channel f n)
