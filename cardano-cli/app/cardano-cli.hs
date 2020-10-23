{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Cardano.CLI.Parsers (opts, pref)
import           Cardano.CLI.Run (renderClientCommandError, runClientCommand)
import           Cardano.CLI.TopHandler
import qualified Cardano.Crypto.Libsodium as Crypto


main :: IO ()
main = toplevelExceptionHandler $ do
  -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
  Crypto.sodiumInit

  co <- Opt.customExecParser pref opts

  orDie renderClientCommandError $ runClientCommand co
