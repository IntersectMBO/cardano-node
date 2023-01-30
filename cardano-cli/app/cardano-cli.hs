{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Cardano.CLI.Parsers (opts, pref)
import           Cardano.CLI.Run (renderClientCommandError, runClientCommand)
import           Cardano.CLI.TopHandler
import qualified Cardano.Crypto.Libsodium as Crypto
#ifdef UNIX
import           System.Posix.Files
#endif

import qualified GHC.IO.Encoding as GHC

main :: IO ()
main = toplevelExceptionHandler $ do
  -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
  Crypto.sodiumInit
  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding
#ifdef UNIX
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
#endif
  co <- Opt.customExecParser pref opts

  orDie renderClientCommandError $ runClientCommand co
