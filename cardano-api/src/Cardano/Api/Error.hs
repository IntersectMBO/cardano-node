{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Error
  ( ApiError (..)
  , renderApiError
  ) where

import           Cardano.Binary (DecoderError (..))

import           Cardano.Prelude

import qualified Data.Text as Text

import           Formatting (build, sformat)

data ApiError
  = ApiError !Text
  | ApiErrorCBOR !DecoderError
  | ApiErrorIO !FilePath !IOException
  deriving (Eq, Show)

renderApiError :: ApiError -> Text
renderApiError ae =
  case ae of
    ApiError txt -> txt
    ApiErrorCBOR de -> sformat build de
    ApiErrorIO fp e -> mconcat [Text.pack fp, ": ", textShow e]


textShow :: Show a => a -> Text
textShow = Text.pack . show
