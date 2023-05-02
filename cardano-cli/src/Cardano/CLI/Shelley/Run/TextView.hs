{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.TextView
  ( ShelleyTextViewFileError(..)
  , renderShelleyTextViewFileError
  , runTextViewCmd
  ) where

import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Api
import           Cardano.Api.Pretty

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

data ShelleyTextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderShelleyTextViewFileError :: ShelleyTextViewFileError -> Doc Ann
renderShelleyTextViewFileError err =
  case err of
    TextViewReadFileError fileErr -> displayError fileErr
    TextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr


runTextViewCmd :: TextViewCmd -> ExceptT ShelleyTextViewFileError IO ()
runTextViewCmd cmd =
  case cmd of
    TextViewInfo fpath mOutfile -> runTextViewInfo fpath mOutfile

runTextViewInfo :: FilePath -> Maybe (File () Out) -> ExceptT ShelleyTextViewFileError IO ()
runTextViewInfo fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just (File oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
