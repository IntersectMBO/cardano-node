{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api.Environment
  ( SocketPath(..)
  ) where

import           Data.Aeson

newtype SocketPath
  = SocketPath { unSocketPath :: FilePath }
  deriving (FromJSON, Show, Eq, Ord)
