{-# LANGUAGE LambdaCase #-}
-- | Shelley CLI shared Utility functions
module Cardano.CLI.Shelley.Util
  (
    displayErrorText

  , NetworkIdError(..)
  , getNetworkId
  , getNetworkId'
  ) where

import           Prelude (error)
import           Data.Text as Text (pack)

import           Cardano.Api
import           Cardano.Prelude

import           Cardano.CLI.Shelley.Commands (NetworkIdArg(..) )

displayErrorText :: Error e => e -> Text
displayErrorText = Text.pack . displayError

data NetworkIdError
  = EnvVarNotSetError
  | ParsingEnvVarError
  deriving Show

instance Error NetworkIdError where
  displayError EnvVarNotSetError = "Can't read environment var"
  displayError ParsingEnvVarError = "Error while parsing environment var"

getNetworkId' :: NetworkIdArg -> IO (Either NetworkIdError NetworkId)
getNetworkId' arg = case arg of
  NetworkIdFromCLI x -> return $ Right x
  NetworkIdFromEnv -> do
    error "todo" --TODO
  
getNetworkId :: NetworkIdArg -> (NetworkIdError -> e) -> ExceptT e IO NetworkId
getNetworkId arg errorTag = error "todo"

getNetworkIdWithDefault :: NetworkIdArg -> (NetworkIdError -> e) -> NetworkId -> ExceptT e IO NetworkId
getNetworkIdWithDefault = error "todo"
