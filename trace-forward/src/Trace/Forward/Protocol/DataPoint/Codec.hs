{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Protocol.DataPoint.Codec
  ( codecDataPointForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import           Data.Singletons
import           Text.Printf (printf)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec (Codec, SomeMessage (..))
import           Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)

import           Trace.Forward.Protocol.DataPoint.Type

codecDataPointForward
  :: forall m.
     MonadST m
  => ([DataPointName] -> CBOR.Encoding)          -- ^ Encoder for 'Request'.
  -> (forall s . CBOR.Decoder s [DataPointName]) -- ^ Decoder for 'Request'.
  -> (DataPointValues -> CBOR.Encoding)          -- ^ Encoder for reply with list of 'DataPoint's values.
  -> (forall s . CBOR.Decoder s DataPointValues) -- ^ Decoder for reply with list of 'DataPoint's values.
  -> Codec DataPointForward
           DeserialiseFailure m LBS.ByteString
codecDataPointForward encodeRequest   decodeRequest
                      encodeReplyList decodeReplyList =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode
    :: forall (st  :: DataPointForward)
              (st' :: DataPointForward).
       Message DataPointForward st st'
    -> CBOR.Encoding

  encode (MsgDataPointsRequest request) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 1
    <> encodeRequest request

  encode MsgDone =
       CBOR.encodeListLen 1
    <> CBOR.encodeWord 2

  encode (MsgDataPointsReply reply) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 3
    <> encodeReplyList reply

  -- Decode messages
  decode
    :: forall (st :: DataPointForward) s.
       ActiveState st
    => Sing st
    -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (1, 2, SingIdle) ->
        SomeMessage . MsgDataPointsRequest <$> decodeRequest

      (2, 1, SingIdle) ->
        return $ SomeMessage MsgDone

      (3, 2, SingBusy) ->
        SomeMessage . MsgDataPointsReply <$> decodeReplyList

      -- Failures per protocol state
      (_, _, SingIdle) ->
        fail (printf "codecDataPointForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, SingBusy) ->
        fail (printf "codecDataPointForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, SingDone) -> notActiveState stok
