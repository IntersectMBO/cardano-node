module Cardano.CLI.Shelley.Run.TextView
  ( ShelleyTextViewFileError(..)
  , renderShelleyTextViewFileError
  , runTextViewCmd
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Api.TextView

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
    TextViewInfo fpath mOutfile -> runTextViewInfo fpath mOutfile

runTextViewInfo :: FilePath -> Maybe OutputFile -> ExceptT ShelleyTextViewFileError IO ()
runTextViewInfo fpath mOutFile = do
  tv <- firstExceptT ShelleyTextViewFileError' $ newExceptT (readTextViewFile fpath)
  let lbCBOR = LBS.fromStrict (tvRawCBOR tv)
  case mOutFile of
    Just (OutputFile oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT ShelleyTextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
