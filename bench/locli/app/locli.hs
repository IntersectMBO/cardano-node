import           Cardano.Prelude

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Cardano.Command (opts, pref, renderCommandError, runCommand)
import           Cardano.TopHandler


main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts

  orDie renderCommandError $ runCommand co
