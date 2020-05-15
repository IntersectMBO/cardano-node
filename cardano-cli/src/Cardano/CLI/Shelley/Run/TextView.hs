module Cardano.CLI.Shelley.Run.TextView
  ( ShelleyTextViewFileError(..)
  , runTextViewCmd
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR)
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Config.TextView

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Data.ByteString.Lazy.Char8 as LBS

data ShelleyTextViewFileError
  = ShelleyTextViewFileError' TextViewFileError
  | ShelleyCliTextViewHelpersError !HelpersError
  deriving Show


runTextViewCmd :: TextViewCmd -> ExceptT ShelleyTextViewFileError IO ()
runTextViewCmd cmd =
  case cmd of
    TextViewInfo fpath -> runTextViewInfo fpath

runTextViewInfo :: FilePath -> ExceptT ShelleyTextViewFileError IO ()
runTextViewInfo fpath = do
  tv <- firstExceptT ShelleyTextViewFileError' $ newExceptT (readTextViewFile fpath)
  firstExceptT ShelleyCliTextViewHelpersError $ pPrintCBOR $ LBS.fromStrict (tvRawCBOR tv)
