{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.TxSubmit.Types
  ( TxSubmitApi
  , TxSubmitApiRecord (..)
  , TxSubmitWebApiError (..)
  , TxSubmitPort (..)
  , renderTxSubmitWebApiError
  ) where

import Cardano.Api
    ( TxId )
import Cardano.Binary
    ( DecoderError )
import Cardano.TxSubmit.Tx
    ( TxSubmitError, renderTxSubmitError )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.ByteString.Char8
    ( ByteString )
import Data.Text
    ( Text )
import Formatting
    ( build, sformat )
import GHC.Generics
    ( Generic )
import Network.HTTP.Media
    ( (//) )
import Servant
    ( (:>)
    , Accept (..)
    , JSON
    , MimeRender (..)
    , MimeUnrender (..)
    , PostAccepted
    , ReqBody
    )
import Servant.API.Generic
    ( (:-), ToServantApi )

import qualified Data.ByteString.Lazy.Char8 as LBS

newtype TxSubmitPort
  = TxSubmitPort Int

-- | An error that can occur in the transaction submission web API.
data TxSubmitWebApiError
  = TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail !DecoderError
  | TxSubmitBadTx !Text
  | TxSubmitFail !TxSubmitError
  deriving Eq

instance ToJSON TxSubmitWebApiError where
  toJSON = convertJson

convertJson :: TxSubmitWebApiError -> Value
convertJson = String . renderTxSubmitWebApiError

renderTxSubmitWebApiError :: TxSubmitWebApiError -> Text
renderTxSubmitWebApiError st =
  case st of
    TxSubmitDecodeHex -> "Provided data was hex encoded and this webapi expects raw binary"
    TxSubmitEmpty -> "Provided transaction has zero length"
    TxSubmitDecodeFail err -> sformat build err
    TxSubmitBadTx tt -> mconcat ["Transactions of type '", tt, "' not supported"]
    TxSubmitFail err -> renderTxSubmitError err

-- | Servant API which provides access to tx submission webapi
type TxSubmitApi
    = "api" :> ToServantApi TxSubmitApiRecord

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
