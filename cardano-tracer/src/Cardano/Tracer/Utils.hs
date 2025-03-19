{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Tracer.Utils
  ( applyBrake
  , askNodeId
  , askNodeName
  , askNodeNameRaw
  , beforeProgramStops
  , connIdToNodeId
  , initAcceptedMetrics
  , initConnectedNodes
  , initConnectedNodesNames
  , initDataPointRequestors
  , initProtocolsBrake
  , logTrace
  , forMM
  , forMM_
  , nl
  , runInLoop
  , showProblemIfAny
  , memberRegistry
  , showRegistry
  , newRegistry
  , lookupRegistry
  , elemsRegistry
  , clearRegistry
  , modifyRegistry_
  , readRegistry
  , getProcessId
  , sequenceConcurrently_
  ) where

import           Cardano.Logging.Types.NodeInfo (NodeInfo(..))
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Utils
import qualified Cardano.Logging as Tracer (traceWith)
import           Cardano.Tracer.MetaTrace hiding (traceWith)
import           Cardano.Tracer.Types
import           Ouroboros.Network.Socket (ConnectionId (..))

import           Control.Concurrent (killThread, mkWeakThreadId, myThreadId)
import           Control.Concurrent.Async (Concurrently(..))
import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.MVar (newMVar, swapMVar, readMVar, tryReadMVar, modifyMVar_)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', stateTVar, readTVarIO, newTVarIO)
import           Control.Exception (SomeAsyncException (..), SomeException, finally, fromException,
                   try, tryJust)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJustM)
import           "contra-tracer" Control.Tracer (stdoutTracer, traceWith)
import           Data.Word (Word32)
import qualified Data.Bimap as BM
import           Data.Bimap (Bimap)
import           Data.Foldable (for_, traverse_)
import           Data.Functor ((<&>), void)
import           Data.List.Extra (dropPrefix, dropSuffix, replace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           System.IO (hClose, hFlush, stdout)
import           System.Mem.Weak (deRefWeak)
import qualified System.Signal as S
import           System.Time.Extra (sleep)

#if defined(mingw32_HOST_OS)
import           System.Win32.Process (getCurrentProcessId)
#else
import           System.Posix.Process (getProcessID)
import           System.Posix.Types (CPid (..))
#endif

-- | Run monadic action in a loop. If there's an exception,
--   it will re-run the action again, after pause that grows.
runInLoop
  :: IO ()           -- ^ An IO-action that can throw an exception.
  -> Maybe Verbosity -- ^ Tracer's verbosity.
  -> FilePath        -- ^ Local socket.
  -> Word            -- ^ Current delay, in seconds.
  -> IO ()
runInLoop action verb localSocket prevDelay =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      case verb of
        Just Minimum -> return ()
        _ -> logTrace $ "cardano-tracer, connection with " <> show localSocket <> " failed: " <> show e
      sleep $ fromIntegral currentDelay
      runInLoop action verb localSocket currentDelay
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e

  !currentDelay =
    if prevDelay < 60
      then prevDelay * 2
      else 60 -- After we reached 60+ secs delay, repeat an attempt every minute.

showProblemIfAny
  :: Maybe Verbosity -- ^ Tracer's verbosity.
  -> IO ()           -- ^ An IO-action that can throw an exception.
  -> IO ()
showProblemIfAny verb action =
  try action >>= \case
    Left (e :: SomeException) ->
      case verb of
        Just Minimum -> return ()
        _ -> logTrace $ "cardano-tracer, the problem: " <> show e
    Right _ -> return ()

logTrace :: String -> IO ()
logTrace = traceWith stdoutTracer

connIdToNodeId :: Show addr => ConnectionId addr -> NodeId
connIdToNodeId ConnectionId{remoteAddress} = NodeId preparedAddress
 where
  -- We have to remove "wrong" symbols from 'NodeId',
  -- to make it appropriate for the name of the subdirectory.
  !preparedAddress =
      T.pack
    . dropPrefix "-"
    . dropSuffix "-"
    . replace "--" ""
    . replace " " "-"
    . replace "\"" "-"
    . replace "/" "-"
    . replace "\\" "-"
    . replace "pipe" "" -- For Windows.
    . replace "." "" -- For Windows.
    . replace "LocalAddress" "" -- There are only local addresses by design.
    $ show remoteAddress

initConnectedNodes :: IO ConnectedNodes
initConnectedNodes = newTVarIO S.empty

initConnectedNodesNames :: IO ConnectedNodesNames
initConnectedNodesNames = newTVarIO BM.empty

initAcceptedMetrics :: IO AcceptedMetrics
initAcceptedMetrics = newTVarIO Map.empty

initDataPointRequestors :: IO DataPointRequestors
initDataPointRequestors = newTVarIO Map.empty

initProtocolsBrake :: IO ProtocolsBrake
initProtocolsBrake = newTVarIO False

askNodeName
  :: TracerEnv
  -> NodeId
  -> IO NodeName
askNodeName TracerEnv{teTracer, teConnectedNodesNames, teDPRequestors, teCurrentDPLock} =
  askNodeNameRaw teTracer teConnectedNodesNames teDPRequestors teCurrentDPLock

askNodeNameRaw
  :: Trace IO TracerTrace
  -> ConnectedNodesNames
  -> DataPointRequestors
  -> Lock
  -> NodeId
  -> IO NodeName
askNodeNameRaw tracer connectedNodesNames dpRequestors currentDPLock nodeId@(NodeId anId) = do
  nodesNames <- readTVarIO connectedNodesNames
  case BM.lookup nodeId nodesNames of
    Just nodeName -> return nodeName
    Nothing -> do
      -- There is no name yet, so we have to ask for 'NodeInfo' datapoint to get the name.
      nodeName <-
        askDataPoint dpRequestors currentDPLock nodeId "NodeInfo" >>= \case
          Nothing -> return anId
          Just NodeInfo{niName} -> return $ if T.null niName then anId else niName

      -- Overlapping node names are considered a misconfiguration.
      -- However using the unique node ID as a fallback still ensures no
      -- trace messages or metrics get lost.
      maybePair <- atomically do
        stateTVar connectedNodesNames \oldBimap ->
          let
             maybePair :: Maybe (NodeId, T.Text)
             maybePair
               | BM.member nodeId oldBimap
               = Nothing
               | BM.memberR nodeName oldBimap
               = Just (nodeId, anId)
               | otherwise
               = Just (nodeId, nodeName)

             newBimap :: Bimap NodeId NodeName
             newBimap = maybe oldBimap (\(k, v) -> BM.insert k v oldBimap) maybePair

          in (maybePair, newBimap)

      for_ @Maybe maybePair \pair ->
        Tracer.traceWith tracer TracerAddNewNodeIdMapping
          { ttBimapping = pair
          }

      return nodeName

askNodeId
  :: TracerEnv
  -> NodeName
  -> IO (Maybe NodeId)
askNodeId TracerEnv{teConnectedNodesNames} nodeName = do
  nodesNames <- readTVarIO teConnectedNodesNames
  return $! if nodeName `BM.memberR` nodesNames
              then Just $ nodesNames BM.!> nodeName
              else Nothing

-- | Stop the protocols. As a result, 'MsgDone' will be sent and interaction
--   between acceptor's part and forwarder's part will be finished.
applyBrake :: ProtocolsBrake -> IO ()
applyBrake stopProtocols = atomically $ modifyTVar' stopProtocols . const $ True

forMM :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m (t b)
forMM mVals f = mVals >>= mapM f

forMM_ :: (Foldable t, Monad m) => m (t a) -> (a -> m ()) -> m ()
forMM_ mVals f = mVals >>= mapM_ f

nl :: T.Text
#ifdef UNIX
nl = "\n"
#else
nl = "\r\n"
#endif

-- | If 'cardano-tracer' process is going to die (by receiving some system signal),
--   we want to do something before it stops.
beforeProgramStops :: IO () -> IO ()
beforeProgramStops action = do
  mainThreadIdWk <- mkWeakThreadId =<< myThreadId
  forM_ signals $ \sig ->
    S.installHandler sig . const $ do
      putStrLn " Program is stopping, please wait..."
      hFlush stdout
      action
        `finally` whenJustM (deRefWeak mainThreadIdWk) killThread
 where
  signals =
    [ S.sigABRT
    , S.sigINT
    , S.sigTERM
    ]

memberRegistry :: Ord a => a -> Registry a b -> IO Bool
memberRegistry a (Registry registry) = do
  tryReadMVar registry <&> \case
    Nothing -> False
    Just set -> Map.member a set

showRegistry :: Show a => Show b => Registry a b -> IO ()
showRegistry (Registry registry) = do
  tryReadMVar registry >>= \case
    Nothing -> error "showRegistry: tryReadMVar failed."
    Just set -> print set

newRegistry :: IO (Registry a b)
newRegistry = Registry <$> newMVar Map.empty

lookupRegistry :: Ord a => Ord b => a -> b -> Registry (a, b) c -> IO (Maybe c)
lookupRegistry key key1 (Registry registry) = do
  Map.lookup (key, key1) <$> readMVar registry

elemsRegistry :: Registry a b -> IO [b]
elemsRegistry (Registry registry) = do
  fmap Map.elems (readMVar registry)

clearRegistry :: HandleRegistry -> IO ()
clearRegistry registry@(Registry mvar) = do
  elemsRegistry registry >>= traverse_ (hClose . fst)
  void do
    swapMVar mvar Map.empty

modifyRegistry_ :: Registry a b -> (Map.Map a b -> IO (Map.Map a b)) -> IO ()
modifyRegistry_ (Registry registry) = modifyMVar_ registry

readRegistry :: Registry a b -> IO (Map.Map a b)
readRegistry (Registry registry) = readMVar registry

getProcessId :: IO Word32
getProcessId =
#if defined(mingw32_HOST_OS)
  getCurrentProcessId
#else
  do CPid pid <- getProcessID
     return $ fromIntegral pid
#endif

sequenceConcurrently_ :: Traversable t => t (IO a) -> IO ()
sequenceConcurrently_ = runConcurrently . traverse_ Concurrently
