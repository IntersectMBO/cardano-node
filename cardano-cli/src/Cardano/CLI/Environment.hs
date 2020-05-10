{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Environment
  ( readEnvSocketPath
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Ops (CliError (..))

import           Cardano.Config.Types (SocketPath (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import qualified Data.Text as Text

import           Prelude (String)

import           System.Environment (lookupEnv)

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: ExceptT CliError IO SocketPath
readEnvSocketPath =
    maybe (left $ CliEnvVarLookup (Text.pack envName)) (pure . SocketPath)
      =<< liftIO (lookupEnv envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"
