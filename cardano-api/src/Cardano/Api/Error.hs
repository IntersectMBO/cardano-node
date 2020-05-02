{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Error
  ( ApiError (..)
  , convertTextViewError
  , renderApiError
  ) where

import           Cardano.Binary (DecoderError (..))

import           Cardano.Prelude

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Formatting (build, sformat)

import           Cardano.Config.TextView (TextViewError(..), TextViewType(..))

data ApiError
  = ApiError !Text
  | ApiErrorCBOR !DecoderError
  | ApiErrorIO !FilePath !IOException
  | ApiTextView !Text
  deriving (Eq, Show)

convertTextViewError :: TextViewError -> Either ApiError b
convertTextViewError err =
  Left $
    case err of
      TextViewFormatError msg -> ApiTextView msg

      TextViewTypeError [expected] actual ->
        ApiTextView $ mconcat
          [ "Expected file type ", Text.decodeLatin1 (unTextViewType expected)
          , ", but got type ", Text.decodeLatin1 (unTextViewType actual)
          ]

      TextViewTypeError expected actual ->
        ApiTextView $ mconcat
          [ "Expected file type to be one of "
          , Text.intercalate ", "
              [ Text.decodeLatin1 (unTextViewType t) | t <- expected ]
          , ", but got type ", Text.decodeLatin1 (unTextViewType actual)
          ]

      TextViewDecodeError derr -> ApiErrorCBOR derr


renderApiError :: ApiError -> Text
renderApiError ae =
  case ae of
    ApiError txt -> txt
    ApiErrorCBOR de -> sformat build de
    ApiErrorIO fp e -> mconcat [Text.pack fp, ": ", textShow e]
    ApiTextView txt -> mconcat ["TextView: ", txt]


textShow :: Show a => a -> Text
textShow = Text.pack . show
