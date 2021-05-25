{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Cardano.Analysis.TopHandler
import           Cardano.Unlog.Parsers (opts, pref)
import           Cardano.Unlog.Run (renderCommandError, runCommand)


main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts

  orDie renderCommandError $ runCommand co
