{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Network.TypedProtocol.Codec (Codec, ActiveState, StateToken, notActiveState, SomeMessage (..))
import           Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)
import           Text.Printf (printf)

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
    :: forall (st  :: TraceObjectForward lo)
              (st' :: TraceObjectForward lo).
       Message (TraceObjectForward lo) st st'
    -> CBOR.Encoding


  encode (MsgTraceObjectsRequest blocking request) =
       CBOR.encodeListLen 3
    <> CBOR.encodeWord 1
    <> CBOR.encodeBool (case blocking of
                          TokBlocking    -> True
                          TokNonBlocking -> False)
    <> encodeRequest request

  encode MsgDone =
       CBOR.encodeListLen 1
    <> CBOR.encodeWord 2

  encode (MsgTraceObjectsReply reply) =
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
    :: forall (st :: TraceObjectForward lo) s.
       ActiveState st
    => StateToken st
    -> CBOR.Decoder s (SomeMessage st)
  decode stateToken = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stateToken) of
      (1, 3, SingIdle) -> do
        blocking <- CBOR.decodeBool
        request <- decodeRequest
        return $!
          if blocking then
            SomeMessage $ MsgTraceObjectsRequest TokBlocking request
          else
            SomeMessage $ MsgTraceObjectsRequest TokNonBlocking request

      (2, 1, SingIdle) ->
        return $ SomeMessage MsgDone

      (3, 2, SingBusy blocking) -> do
        replyList <- decodeReplyList
        case (blocking, replyList) of
          (TokBlocking, x:xs) ->
            return $ SomeMessage (MsgTraceObjectsReply (BlockingReply (x NE.:| xs)))

          (TokNonBlocking, los) ->
            return $ SomeMessage (MsgTraceObjectsReply (NonBlockingReply los))

          (TokBlocking, []) ->
            fail "codecTraceObjectForward: MsgTraceObjectsReply: empty list not permitted"

      -- Failures per protocol state
      (_, _, SingIdle) ->
        fail (printf "codecTraceObjectForward (%s) unexpected key (%d, %d)" (show stateToken) key len)
      (_, _, SingBusy TokBlocking) ->
        fail (printf "codecTraceObjectForward (%s) unexpected key (%d, %d)" (show stateToken) key len)
      (_, _, SingBusy TokNonBlocking) ->
        fail (printf "codecTraceObjectForward (%s) unexpected key (%d, %d)" (show stateToken) key len)
      (_, _, SingDone) -> notActiveState stateToken
