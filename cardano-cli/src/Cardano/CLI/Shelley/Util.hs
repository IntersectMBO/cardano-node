-- | Shelley CLI shared Utility functions
module Cardano.CLI.Shelley.Util
  ( displayErrorText
  , rescue

  , getNetworkId
  , getNetworkId'
  ) where

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import           Data.Text as Text (pack)

import           Cardano.Api
import           Cardano.Prelude

import           Cardano.CLI.Shelley.Commands (NetworkIdArg(..) )

displayErrorText :: Error e => e -> Text
displayErrorText = Text.pack . displayError

rescue :: Functor m =>  m (Either x a) -> (x -> y) -> ExceptT y m a
rescue action except = firstExceptT except $ newExceptT action

getNetworkId' :: NetworkIdArg -> IO (Either EnvNetworkIdError NetworkId)
getNetworkId' arg = case arg of
  NetworkIdFromCLI x -> return $ Right x
  NetworkIdFromEnv -> readEnvNetworkId

getNetworkId :: NetworkIdArg -> (EnvNetworkIdError -> e) -> ExceptT e IO NetworkId
getNetworkId arg errorTag
  = getNetworkId' arg `rescue` errorTag
