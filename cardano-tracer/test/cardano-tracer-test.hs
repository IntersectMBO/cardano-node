{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

import qualified Cardano.Tracer.Test.DataPoint.Tests as DataPoint
import qualified Cardano.Tracer.Test.Logs.Tests as Logs
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils

import           Control.Exception
import           Control.Monad.Extra
import           Data.Monoid
import qualified System.Directory as Sys
import           System.Environment (setEnv, unsetEnv)
import           System.PosixCompat.Files (fileExist)

import           Test.Tasty

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "1" -- For sequential running of tests (because of Windows).

    ts' <- getTestSetup
             TestSetup
             { tsTime         = Last $ Just 10.0
             , tsThreads      = Last $ Just 5
             , tsMessages     = Last   Nothing
             , tsSockInternal = Last $ Just "tracer.sock"
             , tsSockExternal = Last $ Just "tracer-external.sock"
             , tsNetworkMagic = Last $ Just $ NetworkMagic 42
             , tsWorkDir      = Last $ Just "./cardano-tracer-test"
             }

    -- 1. Prepare directory hierarchy
    tracerRoot <- Sys.canonicalizePath $ unI (tsWorkDir ts')
    putStrLn . mconcat $ [ "tsWorkDir ts: ", tracerRoot ]
    -- Weird:  using path canonicalisation leads to process shutdown failures
    whenM (fileExist                         tracerRoot) $
      Sys.removeDirectoryRecursive           tracerRoot
    Sys.createDirectoryIfMissing True       (tracerRoot <> "/logs")
    Sys.setCurrentDirectory                  tracerRoot

    let ts = ts' { tsWorkDir      = Identity tracerRoot
                 }
    putStrLn $ "Test setup:  " <> show ts

    defaultMain
      (testGroup "Tests"
       [      Logs.tests ts
       , DataPoint.tests ts
--       ,   Restart.tests ts
--       ,   Queue.tests ts
       ])
     `catch` (\ (e :: SomeException) -> do
                 unsetEnv "TASTY_NUM_THREADS"
                 throwIO e)
