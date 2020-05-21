module Cardano.CLI.Shelley.Run.TextView
  ( ShelleyTextViewFileError(..)
  , renderShelleyTextViewFileError
  , runTextViewCmd
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Config.TextView

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Data.ByteString.Lazy.Char8 as LBS

data ShelleyTextViewFileError
  = ShelleyTextViewFileError' TextViewFileError
  | ShelleyTextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderShelleyTextViewFileError :: ShelleyTextViewFileError -> Text
renderShelleyTextViewFileError err =
  case err of
    ShelleyTextViewFileError' txtViewFileErr -> renderTextViewFileError txtViewFileErr
    ShelleyTextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr


runTextViewCmd :: TextViewCmd -> ExceptT ShelleyTextViewFileError IO ()
runTextViewCmd cmd =
  case cmd of
    TextViewInfo fpath -> runTextViewInfo fpath

runTextViewInfo :: FilePath -> ExceptT ShelleyTextViewFileError IO ()
runTextViewInfo fpath = do
  tv <- firstExceptT ShelleyTextViewFileError' $ newExceptT (readTextViewFile fpath)
  firstExceptT ShelleyTextViewCBORPrettyPrintError $ pPrintCBOR $ LBS.fromStrict (tvRawCBOR tv)
