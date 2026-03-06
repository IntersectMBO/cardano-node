module Cardano.Timeseries.Interp.Types where
import           Cardano.Timeseries.AsText

import           Control.Monad.Except (ExceptT, throwError)
import           Control.Monad.State.Strict (State)
import           Data.Text (Text)

newtype QueryError =
  ErrorMessage { message :: Text }

instance AsText QueryError where
  asText ErrorMessage{message} = "Error: " <> message


type QueryM a = ExceptT QueryError (State Int) a

throwQueryError :: Text -> QueryM a
throwQueryError = throwError . ErrorMessage
