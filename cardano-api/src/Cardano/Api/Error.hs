{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.Error
  ( Error(..)
  , throwErrorAsException
  , ErrorAsException(..)
  ) where

import           Prelude
import           Control.Exception (Exception(..), throwIO)


class Show e => Error e where

    displayError :: e -> String

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
    show (ErrorAsException e) = show e

instance Exception ErrorAsException where
    displayException (ErrorAsException e) = displayError e

