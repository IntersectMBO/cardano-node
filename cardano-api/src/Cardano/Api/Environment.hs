{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Environment
  ( EnvSocketLookupError(..)
  , SocketPath(..)
  , readEnvSocketPath
  , renderEnvSocketError
  , EnvNetworkIdError(..)
  , readEnvNetworkId
  , parseNetworkId
  ) where

import           Prelude

import           Control.Monad
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.ParserCombinators.ReadP

import           System.Environment (lookupEnv)

import           Cardano.Api.Error
import           Cardano.Api.NetworkId
import           Cardano.Api.Utils (textShow)

newtype SocketPath
  = SocketPath { unSocketPath :: FilePath }
  deriving (FromJSON, Show, Eq, Ord)

newtype EnvSocketLookupError = EnvSocketLookupError Text deriving Show

renderEnvSocketError :: EnvSocketLookupError -> Text
renderEnvSocketError err =
  case err of
    EnvSocketLookupError txt ->
      "Error while looking up environment variable: CARDANO_NODE_SOCKET_PATH " <> " Error: " <> textShow txt

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: IO (Either EnvSocketLookupError SocketPath)
readEnvSocketPath = do
    mEnvName <- lookupEnv envName
    case mEnvName of
      Just sPath ->
        return . Right $ SocketPath sPath
      Nothing ->
        return . Left $ EnvSocketLookupError (Text.pack envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"

networkIdEnvName :: String
networkIdEnvName = "CARDANO_NODE_NETWORK_ID"

readEnvNetworkId :: IO (Either EnvNetworkIdError NetworkId)
readEnvNetworkId
  = lookupEnv networkIdEnvName >>= return . \case
    Nothing -> Left EnvVarNotSetError
    Just val -> case parseNetworkId val of
      Right nid -> Right nid
      Left err -> Left $ ParsingEnvVarError err

data EnvNetworkIdError
  = EnvVarNotSetError
  | ParsingEnvVarError String
  deriving Show

instance Error EnvNetworkIdError where
  displayError = \case
    EnvVarNotSetError -> "Error while looking up environment variable: "<> networkIdEnvName
    ParsingEnvVarError msg -> "Error while parsing " <> networkIdEnvName <> ": " <> msg

parseNetworkId :: String -> Either String NetworkId
parseNetworkId env = case readP_to_S parser env of
  [(res, "")] -> Right $ res
  _other -> Left $ "Bad network ID : " <>  env
 where
  parser = choice [ mainnet, testnet]
  mainnet = string "mainnet" >> eof >> return Mainnet
  testnet = do
    void $ string "testnet-"
    w32 <- readS_to_P reads
    eof
    return $ Testnet $ NetworkMagic w32
