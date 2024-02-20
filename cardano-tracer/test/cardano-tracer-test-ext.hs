{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

import           Cardano.Logging
import           Cardano.Tracer.Test.ForwardingStressTest.Script
import           Cardano.Tracer.Test.ForwardingStressTest.Types
import           Cardano.Tracer.Test.Utils
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad.Extra
import           Data.Functor ((<&>))
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified System.Directory as Sys
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import qualified System.IO as Sys
import           System.PosixCompat.Files (fileExist)
import qualified System.Process as Sys

import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "1" -- For sequential running of tests (because of Windows).
    mbWorkdir <- lookupEnv "WORKDIR"

    ts' <- getTestSetup
             TestSetup
             { tsTime         = Last $ Just 10.0
             , tsThreads      = Last $ Just 5
             , tsMessages     = Last   Nothing
             , tsSockInternal = Last $ Just "tracer.sock"
             , tsSockExternal = Last $ Just "tracer.sock"
             , tsNetworkMagic = Last $ Just $ NetworkMagic 42
             , tsWorkDir      = Last $ Just $ fromMaybe "/tmp/testTracerExt" mbWorkdir
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

    -- 2. Actual tests
    msgCounterRef   <- newIORef 0
    tracerRef       <- newIORef Nothing
    let tracerGetter = getExternalTracerState ts tracerRef
    defaultMain (allTests ts msgCounterRef (tracerGetter <&> snd))
        `catch` (\ (e :: SomeException) -> do
            unsetEnv "TASTY_NUM_THREADS"
            trState <- readIORef tracerRef
            case trState of
              Nothing -> pure ()
              Just (tracerHdl, _) ->
                Sys.cleanupProcess (Nothing, Nothing, Nothing, tracerHdl)
            throwIO e)

allTests ::
     TestSetup Identity
  -> IORef Int
  -> IO (Trace IO Message)
  -> TestTree
allTests ts msgCounter externalTracerGetter =
    testGroup "Tests"
    [ localOption (QuickCheckTests 10) $ testGroup "trace-forwarder"
        [ testProperty "multi-threaded forwarder stress test" $
            runScriptForwarding ts msgCounter externalTracerGetter
        ]
    ]

-- Caution:  non-thread-safe!
getExternalTracerState ::
     TestSetup Identity
  -> IORef (Maybe (Sys.ProcessHandle, Trace IO Message))
  -> IO (Sys.ProcessHandle, Trace IO Message)
getExternalTracerState TestSetup{..} ref = do
  state <- readIORef ref
  case state of
    Just st -> pure st
    Nothing -> do
      stdTr <- standardTracer
      (procHdl, fwdTr) <- setupFwdTracer
      tr <- mkCardanoTracer
              stdTr fwdTr Nothing
              ["Test"]
      let st = (procHdl, tr)
      writeIORef ref $ Just st
      pure st
 where
   setupFwdTracer :: IO (Sys.ProcessHandle, Trace IO FormattedMessage)
   setupFwdTracer = do
     Sys.writeFile "config.yaml" . L.unlines $
       [ "networkMagic: " <> show (unNetworkMagic $ unI tsNetworkMagic)
       , "network:"
       , "  tag: AcceptAt"
       , "  contents: \""<> unI tsSockExternal <>"\""
       , "logging:"
       , "- logRoot: \"logs\""
       , "  logMode: FileMode"
       , "  logFormat: ForMachine"
       ]
     externalTracerHdl <- Sys.spawnProcess "cardano-tracer"
       [ "--config" ,    "config.yaml"
       , "--state-dir" , unI tsWorkDir <> "/tracer-statedir"
       ]
     threadDelay 1_000_000 --wait 1 seconds
     res <- Sys.getProcessExitCode externalTracerHdl
     case res of
       Nothing   -> putStrLn "cardano-tracer started.."
       Just code ->
         error $ "cardano-tracer failed to start with code " <> show code
     -- TODO: check if this is the correct way to use withIOManager
     (forwardSink, _dpStore) <- withIOManager $ \iomgr -> do
       -- For simplicity, we are always 'Initiator',
       -- so 'cardano-tracer' is always a 'Responder'.
       let tracerSocketMode = Just (unI tsSockExternal, Initiator)
           forwardingConf = fromMaybe defaultForwarder (tcForwarder simpleTestConfig)
       initForwarding iomgr forwardingConf (unI tsNetworkMagic) Nothing tracerSocketMode
     pure (externalTracerHdl, forwardTracer forwardSink)
