module Cardano.Api.IO
  ( OutputFile(..)
  ) where

newtype OutputFile = OutputFile
  { unOutputFile :: FilePath
  }
  deriving Show
