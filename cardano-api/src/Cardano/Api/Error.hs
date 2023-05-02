{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.Error
  ( Error(..)
  , throwErrorAsException
  , ErrorAsException(..)
  , FileError(..)
  ) where

import           Cardano.Api.Pretty (Ann, renderStringDefault)
import           Control.Exception (Exception (..), IOException, throwIO)
import           Prettyprinter (Doc)
import           System.IO (Handle)
import           Text.Pretty (Pretty (..))


class Error e where

    displayError :: e -> Doc Ann

instance Error () where
    displayError () = ""


-- | The preferred approach is to use 'Except' or 'ExceptT', but you can if
-- necessary use IO exceptions.
--
throwErrorAsException :: Error e => e -> IO a
throwErrorAsException e = throwIO (ErrorAsException e)

data ErrorAsException where
     ErrorAsException :: Error e => e -> ErrorAsException

instance Show ErrorAsException where
    show (ErrorAsException e) = renderStringDefault (displayError e)

instance Exception ErrorAsException where
    displayException (ErrorAsException e) = renderStringDefault (displayError e)


data FileError e = FileError FilePath e
                 | FileErrorTempFile
                     FilePath
                     -- ^ Target path
                     FilePath
                     -- ^ Temporary path
                     Handle
                 | FileIOError FilePath IOException
  deriving (Show, Eq, Functor)

instance Error e => Error (FileError e) where
  displayError (FileErrorTempFile targetPath tempPath h)=
    "Error creating temporary file at: " <> pretty tempPath <>
    "/n" <> "Target path: " <> pretty targetPath <>
    "/n" <> "Handle: " <> pretty (show h)
  displayError (FileIOError path ioe) =
    pretty path <> ": " <> pretty (displayException ioe)
  displayError (FileError path e) =
    pretty path <> ": " <> displayError e
