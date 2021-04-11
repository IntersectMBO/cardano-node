module Trace.Forward.Test.Codec
  ( tests
  ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.Serialise as S
import           Control.Monad.Class.MonadST (MonadST)
import qualified Control.Monad.ST as ST
import           Codec.CBOR.Read (DeserialiseFailure)
import qualified Data.ByteString.Lazy as LBS
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..), Codec, prop_codecM)
import           Ouroboros.Network.Codec (encode)

import           Trace.Forward.Protocol.Codec (codecTraceForward)

import           Trace.Forward.Test.Types (TraceForwardText)

tests :: TestTree
tests = testGroup "Test.Codec"
  [ testProperty "codec ST"      propCodec
  , testProperty "codec cbor ST" propCodecCBOR
  ]

propCodec
  :: AnyMessageAndAgency TraceForwardText
  -> Bool
propCodec msg = ST.runST $ prop_codecM codec msg

propCodecCBOR
  :: AnyMessageAndAgency TraceForwardText
  -> Bool
propCodecCBOR msg = ST.runST $ prop_codec_cborM codec msg

codec
  :: MonadST m
  => Codec TraceForwardText
           DeserialiseFailure
           m LBS.ByteString
codec = codecTraceForward S.encode S.decode
                          S.encode S.decode

-- | Check that the codec produces a valid CBOR term
-- that is decodeable by CBOR.decodeTerm.
prop_codec_cborM
  :: MonadST m
  => Codec TraceForwardText
           DeserialiseFailure
           m LBS.ByteString
  -> AnyMessageAndAgency TraceForwardText
  -> m Bool
prop_codec_cborM codec' (AnyMessageAndAgency stok msg)
  = case CBOR.deserialiseFromBytes CBOR.decodeTerm $ encode codec' stok msg of
      Left _err               -> return False
      Right (leftover, _term) -> return $ LBS.null leftover
