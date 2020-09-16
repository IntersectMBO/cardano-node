{-# LANGUAGE TemplateHaskell #-}

module Cardano.CLI.GitRev
  ( gitRev
  ) where

import           Data.Text (Text)

import qualified Data.Text as T
import qualified Development.GitRev as GR

gitRev :: Text
gitRev = T.pack $(GR.gitHash)
