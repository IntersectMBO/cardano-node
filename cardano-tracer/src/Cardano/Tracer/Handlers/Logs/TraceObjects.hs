{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.TraceObjects
  ( traceObjectsHandler
  , deregisterNodeId
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.Extra (whenJust)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import System.IO (openFile, hClose, Handle, IOMode (AppendMode))
import System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))

import Cardano.Logging (TraceObject)

import Cardano.Tracer.Configuration
import Cardano.Tracer.Environment
import Cardano.Tracer.Handlers.Logs.File
import Cardano.Tracer.Handlers.Logs.Journal
import Cardano.Tracer.Handlers.RTView.Run
import Cardano.Tracer.Handlers.Logs.Utils
import Cardano.Tracer.Types
import Cardano.Tracer.Utils

-- | This handler is called periodically by 'TraceObjectForward' protocol
--   from 'trace-forward' library.
traceObjectsHandler
  :: TracerEnv         -- ^ Tracer environment.
  -> NodeId            -- ^ An id of the node 'TraceObject's were received from.
  -> [TraceObject]     -- ^ The list of received 'TraceObject's (may be empty).
  -> IO ()
traceObjectsHandler _ _ [] = return ()
traceObjectsHandler tracerEnv nodeId traceObjects = do
  nodeName <- askNodeName tracerEnv nodeId

  forConcurrently_ logging \loggingParams@LoggingParams{logMode, logRoot, logFormat} -> do
    showProblemIfAny verbosity do
      case logMode of
        FileMode -> 
          writeTraceObjectsToFile teRegistry
             loggingParams nodeName teCurrentLogLock traceObjects
        JournalMode -> 
          writeTraceObjectsToJournal nodeName traceObjects
  whenJust hasRTView \_ ->
    saveTraceObjects teSavedTO nodeId traceObjects
  teReforwardTraceObjects traceObjects 

  where
    TracerEnv
      { teConfig = TracerConfig{logging, verbosity, hasRTView}
      , teCurrentLogLock
      , teSavedTO
      , teReforwardTraceObjects
      , teRegistry
      } = tracerEnv

deregisterNodeId :: TracerEnv -> NodeId -> IO ()
deregisterNodeId tracerEnv@TracerEnv{ teConfig = TracerConfig { logging }, teRegistry = Registry registry } nodeId = do
  nodeName <- askNodeName tracerEnv nodeId

  forConcurrently_ logging \loggingParams@LoggingParams{logMode, logRoot, logFormat} -> do

    case logMode of
      FileMode -> do
        modifyMVar_ registry \reg -> do
          case Map.updateLookupWithKey alwaysDelete (nodeName, loggingParams) reg of
            (Nothing, _newReg) -> pure reg
            (Just (handle, _filePath), newReg) -> do
              hClose handle
              pure newReg
      JournalMode -> 
        pure ()

  where

  alwaysDelete key value = Nothing

{-
1. checkRootDir :: Lock -> Registry (NodeName, Handle) -> ..
 + logSubDirs are node names
 + We can filter here, check if subdir exists in registry
 + leave lock in place
 + we only have a read from Registry, rotator does not shut down handles
2. In checkLogs as well?
3. writeTraceObjectsToFile :: Registry (NodeName, Handle) -> ..
 + leave 'withLock currentLogLock' lock in place
 + pathToCurrentLog
  + lookup handle
 + Maybe it makes sense to do it one step earlier, no: it has to happen after locking.
4. traceObjectHandler
5. Registry would have to be included in tracerEnv (in Run.hs)
6. In Tracer/../Run.hs
 + First approximation:
 + withNodeConnection :: NodeName -> TracerEnv -> IO (ConnectionId LocalAddress IO ())
   withNodeConnection nodeName tracerEnv = do
     -- handle <- createEmptyLogRotation nodeName
     -- update env (Registry (nodeName, handle))
     -- traceObjectsHandler
     pure $ traceObjectsHandler env
 + runTraceObjectsAcceptor tracerEnv tfConfig errorHandler = 
     acceptTraceObjectsResp
       tfConfig
       (\(connIdToNodeId . rcConnectionId -> mm) -> withNodeConnection mm tracerEnv >>= id)
       ..
 + Leave rotator code untouched.
7. make cleanup = close handles
8. runTraceObjectsAcceptor .. = do
    (handler, teardown) <- bracketLike ...
    undefined 
    ..

Many consumers, only one producer. (no rotator needs to change the
handle)

teardown: lookup NodeName -> close handle -> update MVar, deleting tuple

+ Don't touch traceForwarder, only in cardano-tracer
-}

{-
Yes, the rotator and the error handler must be able to close all open
handles.

Additionally, the rotator needs to check if there's an open Handle for
some NodeName, the latter being reflected in the subdir name. There
might be more subdirs where nothing's being logged into, so the
rotator should skip them (edited)

But then should the registry not be a map yet again from NodeName to
Handle? Otherwise you can't check the MVar (Set (NodeName, Handle))
without having access to both

The Handle should be opened before the trace...Handler is run. It was
that kind of wrapper, or bracket-like, construction that we thought
of. I'm aware it's only one possible approach.
-}
