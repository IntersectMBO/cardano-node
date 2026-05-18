{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Metrics.Protocol.Codec (
  codecEKGForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf (printf)
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec (Codec, SomeMessage (..))
import           Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)

import           System.Metrics.Protocol.Type


codecEKGForward
  :: forall req resp m.
     (MonadST m)
  => (req -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s req)
  -> (resp -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s resp)
  -> Codec (EKGForward req resp)
           DeserialiseFailure m LBS.ByteString
codecEKGForward encodeReq  decodeReq
                encodeResp decodeResp =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode :: forall (st  :: EKGForward req resp)
                   (st' :: EKGForward req resp).
            Message (EKGForward req resp) st st'
         -> CBOR.Encoding

  encode (MsgReq req) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> encodeReq req

  encode MsgDone =
    CBOR.encodeListLen 1
      <> CBOR.encodeWord 1

  encode (MsgResp resp) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> encodeResp resp

  -- Decode messages
  decode :: forall (st :: EKGForward req resp) s.
            ActiveState st
         => StateToken st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (0, 2, SingIdle) ->
        SomeMessage . MsgReq <$> decodeReq

      (1, 1, SingIdle) ->
        return $ SomeMessage MsgDone

      (1, 2, SingBusy) ->
        SomeMessage . MsgResp <$> decodeResp

      -- Failures per protocol state
      (_, _, SingIdle) ->
        fail (printf "codecEKGForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, SingBusy) ->
        fail (printf "codecEKGForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, SingDone) -> notActiveState stok
