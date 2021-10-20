module Test.DataPoint.Forward.Protocol.Tests
  ( tests
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Monad.ST (runST)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.TypedProtocol.Codec

import           DataPoint.Forward.Protocol.Codec
import           DataPoint.Forward.Protocol.Type

import           Test.DataPoint.Forward.Protocol.Codec ()

tests :: TestTree
tests = testGroup "DataPoint.Forward.Protocol"
  [ testProperty "codec" prop_codec_DataPointForward
  ]

prop_codec_DataPointForward :: AnyMessageAndAgency DataPointForward -> Bool
prop_codec_DataPointForward msg =
  runST $ prop_codecM
          (codecDataPointForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
          msg
