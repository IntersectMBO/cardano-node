{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Environment
  ( EnvSocketError(..)
  , SocketPath(..)
  , readEnvSocketPath
  , renderEnvSocketError
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Environment (lookupEnv)

import           Cardano.Api.IO (File (..), FileDirection (..), MapFile)
import           Cardano.Api.Utils (textShow)

newtype SocketPath = SocketPath
  { unSocketPath :: File 'InOut
  } deriving newtype (Eq, Ord, Show, IsString, MapFile, FromJSON, ToJSON)

newtype EnvSocketError = CliEnvVarLookup Text deriving Show

renderEnvSocketError :: EnvSocketError -> Text
renderEnvSocketError err =
  case err of
    CliEnvVarLookup txt ->
      "Error while looking up environment variable: CARDANO_NODE_SOCKET_PATH " <> " Error: " <> textShow txt

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: IO (Either EnvSocketError SocketPath)
readEnvSocketPath = do
    mEnvName <- lookupEnv envName
    case mEnvName of
      Just sPath ->
        return . Right $ SocketPath $ File sPath
      Nothing ->
        return . Left $ CliEnvVarLookup (Text.pack envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"
