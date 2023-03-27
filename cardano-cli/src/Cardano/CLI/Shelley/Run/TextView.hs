{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.TextView
  ( ShelleyTextViewFileError(..)
  , renderShelleyTextViewFileError
  , runTextViewCmd
  ) where

import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Api

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

data ShelleyTextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderShelleyTextViewFileError :: ShelleyTextViewFileError -> Text
renderShelleyTextViewFileError err =
  case err of
    TextViewReadFileError fileErr -> Text.pack (displayError fileErr)
    TextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr


runTextViewCmd :: TextViewCmd -> ExceptT ShelleyTextViewFileError IO ()
runTextViewCmd cmd =
  case cmd of
    TextViewInfo fpath mOutfile -> runTextViewInfo fpath mOutfile

runTextViewInfo :: File 'In -> Maybe (File 'Out) -> ExceptT ShelleyTextViewFileError IO ()
runTextViewInfo fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just oFpath -> liftIO $ LBS.writeFile (unFile oFpath) lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
