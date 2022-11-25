{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Environment
  ( EnvSocketError(..)
  , SocketPath(..)
  , readEnvSocketPath
  , renderEnvSocketError
  , EnvNetworkIdError(..)
  , readEnvNetworkId
  , parseNetworkId
  ) where

import           Prelude

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           System.Environment (lookupEnv)
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<|>))
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

import qualified Ouroboros.Network.Magic as Consensus

import           Cardano.Api.Error
import           Cardano.Api.NetworkId
import           Cardano.Api.Utils (formatParsecError, textShow)

newtype SocketPath
  = SocketPath { unSocketPath :: FilePath }
  deriving (FromJSON, Show, Eq, Ord)

newtype EnvSocketError = EnvSocketLookupError Text deriving Show

renderEnvSocketError :: EnvSocketError -> Text
renderEnvSocketError err =
  case err of
    EnvSocketLookupError txt ->
      "Error while looking up environment variable: CARDANO_NODE_SOCKET_PATH " <> " Error: " <> textShow txt

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: IO (Either EnvSocketError SocketPath)
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
      Just val ->
        case Parsec.parse ((parseMainnet <|> parseNetworkId) <* Parsec.eof) "" val of
          Right nid -> Right nid
          Left err -> Left $ ParsingEnvVarError err

data EnvNetworkIdError
  = EnvVarNotSetError
  | ParsingEnvVarError Parsec.ParseError
  deriving Show

instance Error EnvNetworkIdError where
  displayError = \case
    EnvVarNotSetError -> "Environment variable " <>  networkIdEnvName <> " not found"
    ParsingEnvVarError msg -> formatParsecError msg

parseMainnet :: Parsec.Parser NetworkId
parseMainnet = Parsec.string "mainnet" >> return Mainnet

decimal :: Parsec.Parser Integer
Parsec.TokenParser { Parsec.decimal = decimal } = Parsec.haskell

parseNetworkId :: Parsec.Parser NetworkId
parseNetworkId = do
  i <- decimal
  if i > toInteger (maxBound :: Word32)
  then fail $ "NetworkMagic " <> show i <> " exceeds the Word32 upper bound"
  else return $ mainnetOrTestnet $ fromIntegral i
 where
  mainnetOrTestnet :: Word32 -> NetworkId
  mainnetOrTestnet i
    | Consensus.unNetworkMagic mainnetNetworkMagic == i = fromNetworkMagic mainnetNetworkMagic
    | otherwise = Testnet $ Consensus.NetworkMagic i
