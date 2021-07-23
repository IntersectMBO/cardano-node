module Test.Trace.Forward.Protocol.Tests
  ( tests
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Monad.ST (runST)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.TypedProtocol.Codec

import           Trace.Forward.Protocol.Codec
import           Trace.Forward.Protocol.Type

import           Test.Trace.Forward.Protocol.Codec ()
import           Test.Trace.Forward.Protocol.TraceItem

tests :: TestTree
tests = testGroup "Trace.Forward.Protocol"
  [ testProperty "codec" prop_codec_TraceForward
  ]

prop_codec_TraceForward :: AnyMessageAndAgency (TraceForward TraceItem) -> Bool
prop_codec_TraceForward msg =
  runST $ prop_codecM
          (codecTraceForward CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode
                             CBOR.encode CBOR.decode)
          msg
