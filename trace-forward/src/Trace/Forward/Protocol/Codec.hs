{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Protocol.Codec (
  codecTraceForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf (printf)
import           Ouroboros.Network.Codec (Codec, PeerHasAgency (..),
                                          PeerRole (..), SomeMessage (..),
                                          mkCodecCborLazyBS)

import           Trace.Forward.Protocol.Type

codecTraceForward
  :: forall req resp m.
     (MonadST m)
  => (req -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s req)
  -> (resp -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s resp)
  -> Codec (TraceForward req resp)
           DeserialiseFailure m LBS.ByteString
codecTraceForward encodeReq  decodeReq
                encodeResp decodeResp =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode :: forall (pr  :: PeerRole)
                   (st  :: TraceForward req resp)
                   (st' :: TraceForward req resp).
            PeerHasAgency pr st
         -> Message (TraceForward req resp) st st'
         -> CBOR.Encoding

  encode (ClientAgency TokIdle) (MsgReq req) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> encodeReq req

  encode (ClientAgency TokIdle) MsgDone =
    CBOR.encodeListLen 1
      <> CBOR.encodeWord 1

  encode (ServerAgency TokBusy) (MsgResp resp) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> encodeResp resp

  -- Decode messages
  decode :: forall (pr :: PeerRole)
                   (st :: TraceForward req resp) s.
            PeerHasAgency pr st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (0, 2, ClientAgency TokIdle) ->
        SomeMessage . MsgReq <$> decodeReq

      (1, 1, ClientAgency TokIdle) ->
        return $ SomeMessage MsgDone

      (1, 2, ServerAgency TokBusy) ->
        SomeMessage . MsgResp <$> decodeResp

      -- Failures per protocol state
      (_, _, ClientAgency TokIdle) ->
        fail (printf "codecTraceForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency TokBusy) ->
        fail (printf "codecTraceForward (%s) unexpected key (%d, %d)" (show stok) key len)
