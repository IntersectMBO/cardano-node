{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Environment
  ( EnvSocketError(..)
  , SocketPath(..)
  , renderEnvSocketError
  ) where

import           Data.Aeson
import           Data.Text (Text)
import qualified Prettyprinter as PP

import           Cardano.Api.Pretty

newtype SocketPath
  = SocketPath { unSocketPath :: FilePath }
  deriving (FromJSON, Show, Eq, Ord)

newtype EnvSocketError = CliEnvVarLookup Text deriving Show

renderEnvSocketError :: EnvSocketError -> Doc Ann
renderEnvSocketError err =
  case err of
    CliEnvVarLookup txt ->
      PP.vsep
        [ reflow "Error while looking up environment variable: CARDANO_NODE_SOCKET_PATH.  Error: "
        , PP.indent 2 $ pretty txt
        ]
