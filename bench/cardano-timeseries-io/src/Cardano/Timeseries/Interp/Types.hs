module Cardano.Timeseries.Interp.Types where
import           Control.Monad.Except       (ExceptT, throwError)
import           Control.Monad.State.Strict (State)
import           Cardano.Timeseries.AsText
import           Data.Text (Text)

newtype InterpError =
  InterpError { message :: Text }

instance AsText InterpError where
  asText InterpError{message} = "Interpretation error: " <> message

type InterpM a = ExceptT InterpError (State Int) a

throwInterpError :: Text -> InterpM a
throwInterpError = throwError . InterpError
