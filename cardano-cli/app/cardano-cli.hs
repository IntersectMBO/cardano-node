{-# LANGUAGE OverloadedStrings #-}

import Cardano.CLI.Parsers (opts, pref)
import Cardano.CLI.Run (renderClientCommandError, runClientCommand)
import Cardano.Config.TopHandler
import Cardano.Prelude hiding (option)
import Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

main :: IO ()
main = toplevelExceptionHandler $ do
  co <- Opt.customExecParser pref opts

  orDie renderClientCommandError $ runClientCommand co
