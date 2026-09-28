module Testnet.Status.Types
  ( CheckStatusOptions (..)
  , OutputFormat(..)
  ) where

import           Cardano.Prelude (Nat)

import           Prelude

import           Data.Default.Class (Default (..))

data CheckStatusOptions = CheckStatusOptions
  { testnetDir :: Maybe FilePath
  , statusCheckTimeout :: Nat
  , outputFormat :: OutputFormat
  }

data OutputFormat = OutputFormatText | OutputFormatJson

instance Default OutputFormat where
  def = OutputFormatText
