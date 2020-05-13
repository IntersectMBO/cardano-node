module Cardano.CLI.Shelley.Run.TextView
  ( runTextViewCmd
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Ops (CliError (..), pPrintCBOR)
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Config.TextView

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Data.ByteString.Lazy.Char8 as LBS

runTextViewCmd :: TextViewCmd -> ExceptT CliError IO ()
runTextViewCmd cmd =
  case cmd of
    TextViewInfo fpath -> runTextViewInfo fpath

runTextViewInfo :: FilePath -> ExceptT CliError IO ()
runTextViewInfo fpath = do
  tv <- firstExceptT CliTextViewFileError $ newExceptT (readTextViewFile fpath)
  pPrintCBOR $ LBS.fromStrict (tvRawCBOR tv)
