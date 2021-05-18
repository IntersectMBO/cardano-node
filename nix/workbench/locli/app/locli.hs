{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Cardano.Unlog.Parsers (opts, pref)
import           Cardano.Unlog.Run (renderCommandError, runCommand)
import           Cardano.TopHandler


main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts

  orDie renderCommandError $ runCommand co
