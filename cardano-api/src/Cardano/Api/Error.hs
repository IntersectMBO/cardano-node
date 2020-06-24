{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Error
  ( ApiError (..)
  , renderApiError
  , textShow
  ) where

import           Cardano.Binary (DecoderError (..))

import           Cardano.Prelude

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Formatting (build, sformat)

import           Cardano.Api.TextView (TextViewError(..), TextViewType(..))

data ApiError
  = ApiError !Text
  | ApiErrorCBOR !DecoderError
  | ApiErrorIO !FilePath !IOException
  | ApiTextView !TextViewError
  deriving (Eq, Show)

renderApiError :: ApiError -> Text
renderApiError ae =
  case ae of
    ApiError txt -> txt
    ApiErrorCBOR de -> sformat build de
    ApiErrorIO fp e -> mconcat [Text.pack fp, ": ", textShow e]
    ApiTextView err -> case err of
      TextViewFormatError msg -> msg

      TextViewTypeError [expected] actual ->
        mconcat
          [ "Expected file type ", Text.decodeLatin1 (unTextViewType expected)
          , ", but got type ", Text.decodeLatin1 (unTextViewType actual)
          ]

      TextViewTypeError expected actual ->
        mconcat
          [ "Expected file type to be one of "
          , Text.intercalate ", "
              [ Text.decodeLatin1 (unTextViewType t) | t <- expected ]
          , ", but got type ", Text.decodeLatin1 (unTextViewType actual)
          ]

      TextViewDecodeError de -> sformat build de

textShow :: Show a => a -> Text
textShow = Text.pack . show
