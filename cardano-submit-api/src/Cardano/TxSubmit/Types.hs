{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitWebApiError (..)
  , TxSubmitPort (..)
  , EnvSocketError(..)
  , TxCmdError(..)
  , RawCborDecodeError(..)
  , renderTxCmdError
  ) where

import           Cardano.Api (Error (..), TxId, TxValidationErrorInCardanoMode (..), pshow,
                   textShow)

import           Cardano.Binary (DecoderError)
import           Cardano.TxSubmit.Orphans ()

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Media ((//))

import           Servant (Accept (..), JSON, MimeRender (..), MimeUnrender (..), PostAccepted,
                   ReqBody, (:>))
import           Servant.API.Generic (ToServantApi, (:-))

newtype TxSubmitPort = TxSubmitPort Int

-- | The errors that the raw CBOR transaction parsing\/decoding functions can return.
--
newtype RawCborDecodeError = RawCborDecodeError [DecoderError]
  deriving (Eq, Generic, Show)

instance Error RawCborDecodeError where
  prettyError (RawCborDecodeError decodeErrors) = "RawCborDecodeError decode error: " <> pshow (fmap pshow decodeErrors)

deriving anyclass instance ToJSON RawCborDecodeError

-- | An error that can occur in the transaction submission web API.
data TxSubmitWebApiError
  = TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail !DecoderError
  | TxSubmitBadTx !Text
  | TxSubmitFail TxCmdError

deriving instance Generic TxSubmitWebApiError

instance ToJSON TxSubmitWebApiError where
  toJSON = \case
    TxSubmitDecodeHex -> Aeson.object
      [ "tag" .= String "TxSubmitDecodeHex"
      ]
    TxSubmitEmpty -> Aeson.object
      [ "tag" .= String "TxSubmitEmpty"
      ]
    TxSubmitDecodeFail err -> Aeson.object
      [ "tag" .= String "TxSubmitDecodeFail"
      , "contents" .= toJSON err
      ]
    TxSubmitBadTx err -> Aeson.object
      [ "tag" .= String "TxSubmitBadTx"
      , "contents" .= toJSON err
      ]
    TxSubmitFail err -> Aeson.object
      [ "tag" .= String "TxSubmitFail"
      , "contents" .= toJSON err
      ]

newtype EnvSocketError = CliEnvVarLookup Text
  deriving (Eq, Generic, Show)

instance ToJSON EnvSocketError where
  toJSON (CliEnvVarLookup msg) = Aeson.object
    [ "message" .= String msg
    ]

data TxCmdError
  = TxCmdSocketEnvError EnvSocketError
  | TxCmdTxReadError !RawCborDecodeError
  | TxCmdTxSubmitValidationError !TxValidationErrorInCardanoMode

deriving instance Generic TxCmdError

deriving anyclass instance ToJSON TxCmdError

renderTxCmdError :: TxCmdError -> Text
renderTxCmdError = \case
  TxCmdSocketEnvError socketError ->
    "socket env error " <> textShow socketError
  TxCmdTxReadError envelopeError ->
    "transaction read error " <> textShow envelopeError
  TxCmdTxSubmitValidationError e ->
    case e of
      TxValidationErrorInCardanoMode err -> "transaction submit error " <> T.pack (show err)
      TxValidationEraMismatch eraMismatch -> "transaction submit era mismatch" <> textShow eraMismatch

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi = "api" :> ToServantApi TxSubmitApiRecord

-- | A servant-generic record with all the methods of the API
newtype TxSubmitApiRecord route = TxSubmitApiRecord
  { _txSubmitPost :: route
        :- "submit"
        :> "tx"
        :> ReqBody '[CBORStream] ByteString
        :> PostAccepted '[JSON] TxId
  } deriving (Generic)

data CBORStream

instance Accept CBORStream where
  contentType _ = "application" // "cbor"

instance MimeRender CBORStream ByteString where
    mimeRender _ = LBS.fromStrict

instance MimeRender CBORStream LBS.ByteString where
    mimeRender _ = id

instance MimeUnrender CBORStream ByteString where
    mimeUnrender _ = Right . LBS.toStrict

instance MimeUnrender CBORStream LBS.ByteString where
    mimeUnrender _ = Right
