{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Environment
  ( EnvSocketError (..),
    readEnvSocketPath,
    renderEnvSocketError,
  )
where

import Cardano.CLI.Helpers (textShow)
import Cardano.Config.Types (SocketPath (..))
import Cardano.Prelude
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (left)
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import Prelude (String)

data EnvSocketError
  = CliEnvVarLookup !Text
  deriving (Show)

renderEnvSocketError :: EnvSocketError -> Text
renderEnvSocketError err =
  case err of
    CliEnvVarLookup txt ->
      "Error while looking up environment variable: CARDANO_NODE_SOCKET_PATH " <> " Error: " <> textShow txt

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: ExceptT EnvSocketError IO SocketPath
readEnvSocketPath =
  maybe (left $ CliEnvVarLookup (Text.pack envName)) (pure . SocketPath)
    =<< liftIO (lookupEnv envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"
