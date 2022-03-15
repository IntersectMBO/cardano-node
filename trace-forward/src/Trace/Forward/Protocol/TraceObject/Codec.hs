{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Protocol.TraceObject.Codec
  ( codecTraceObjectForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import           Text.Printf (printf)

import           Network.TypedProtocol.Codec (Codec, PeerHasAgency (..),
                                              PeerRole (..), SomeMessage (..))
import           Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)

import           Trace.Forward.Protocol.TraceObject.Type

codecTraceObjectForward
  :: forall lo m.
     MonadST m
  => (NumberOfTraceObjects -> CBOR.Encoding)          -- ^ Encoder for 'Request'.
  -> (forall s . CBOR.Decoder s NumberOfTraceObjects) -- ^ Decoder for 'Request'.
  -> ([lo] -> CBOR.Encoding)                          -- ^ Encoder for reply with list of 'TraceObject's.
  -> (forall s . CBOR.Decoder s [lo])                 -- ^ Decoder for reply with list of 'TraceObject's.
  -> Codec (TraceObjectForward lo)
           DeserialiseFailure m LBS.ByteString
codecTraceObjectForward encodeRequest   decodeRequest
                        encodeReplyList decodeReplyList =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode
    :: forall (pr  :: PeerRole)
              (st  :: TraceObjectForward lo)
              (st' :: TraceObjectForward lo).
       PeerHasAgency pr st
    -> Message (TraceObjectForward lo) st st'
    -> CBOR.Encoding

  encode (ClientAgency TokIdle) (MsgTraceObjectsRequest blocking request) =
       CBOR.encodeListLen 3
    <> CBOR.encodeWord 1
    <> CBOR.encodeBool (case blocking of
                          TokBlocking    -> True
                          TokNonBlocking -> False)
    <> encodeRequest request

  encode (ClientAgency TokIdle) MsgDone =
       CBOR.encodeListLen 1
    <> CBOR.encodeWord 2

  encode (ServerAgency (TokBusy _)) (MsgTraceObjectsReply reply) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 3
    <> encodeReplyList replyList
   where
    replyList =
      case reply of
        BlockingReply    los -> NE.toList los
        NonBlockingReply los -> los

  -- Decode messages
  decode
    :: forall (pr :: PeerRole)
              (st :: TraceObjectForward lo) s.
       PeerHasAgency pr st
    -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (1, 3, ClientAgency TokIdle) -> do
        blocking <- CBOR.decodeBool
        request <- decodeRequest
        return $!
          if blocking then
            SomeMessage $ MsgTraceObjectsRequest TokBlocking request
          else
            SomeMessage $ MsgTraceObjectsRequest TokNonBlocking request

      (2, 1, ClientAgency TokIdle) ->
        return $ SomeMessage MsgDone

      (3, 2, ServerAgency (TokBusy blocking)) -> do
        replyList <- decodeReplyList
        case (blocking, replyList) of
          (TokBlocking, x:xs) ->
            return $ SomeMessage (MsgTraceObjectsReply (BlockingReply (x NE.:| xs)))

          (TokNonBlocking, los) ->
            return $ SomeMessage (MsgTraceObjectsReply (NonBlockingReply los))

          (TokBlocking, []) ->
            fail "codecTraceObjectForward: MsgTraceObjectsReply: empty list not permitted"

      -- Failures per protocol state
      (_, _, ClientAgency TokIdle) ->
        fail (printf "codecTraceObjectForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency (TokBusy TokBlocking)) ->
        fail (printf "codecTraceObjectForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency (TokBusy TokNonBlocking)) ->
        fail (printf "codecTraceObjectForward (%s) unexpected key (%d, %d)" (show stok) key len)
