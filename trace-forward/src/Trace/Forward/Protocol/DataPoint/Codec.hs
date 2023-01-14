{-# LANGUAGE DataKinds #-}
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
import           Text.Printf (printf)

import           Network.TypedProtocol.Codec (Codec, PeerHasAgency (..),
                                              PeerRole (..), SomeMessage (..))
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
    :: forall (pr  :: PeerRole)
              (st  :: DataPointForward)
              (st' :: DataPointForward).
       PeerHasAgency pr st
    -> Message DataPointForward st st'
    -> CBOR.Encoding

  encode (ClientAgency TokIdle) (MsgDataPointsRequest request) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 1
    <> encodeRequest request

  encode (ClientAgency TokIdle) MsgDone =
       CBOR.encodeListLen 1
    <> CBOR.encodeWord 2

  encode (ServerAgency TokBusy) (MsgDataPointsReply reply) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 3
    <> encodeReplyList reply

  -- Decode messages
  decode
    :: forall (pr :: PeerRole)
              (st :: DataPointForward) s.
       PeerHasAgency pr st
    -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (1, 2, ClientAgency TokIdle) ->
        SomeMessage . MsgDataPointsRequest <$> decodeRequest

      (2, 1, ClientAgency TokIdle) ->
        return $ SomeMessage MsgDone

      (3, 2, ServerAgency TokBusy) ->
        SomeMessage . MsgDataPointsReply <$> decodeReplyList

      -- Failures per protocol state
      (_, _, ClientAgency TokIdle) ->
        fail (printf "codecDataPointForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency TokBusy) ->
        fail (printf "codecDataPointForward (%s) unexpected key (%d, %d)" (show stok) key len)
