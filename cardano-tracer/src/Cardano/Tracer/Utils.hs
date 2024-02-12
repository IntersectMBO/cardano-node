{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
  , lift2M
  , lift3M
  , forMM
  , forMM_
  , nl
  , runInLoop
  , showProblemIfAny
  , fromSTMSetIO
  , fromSTMMap
  , fromSTMSet
  , toList
  , stmMapToList
  , memberRegistry
  ) where

#if MIN_VERSION_base(4,18,0)
-- Do not know why.
import           Control.Applicative (liftA3)
#else
import           Control.Applicative (liftA2, liftA3)
#endif
import           Control.Concurrent (killThread, mkWeakThreadId, myThreadId)
import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.MVar (tryReadMVar)
import           Control.Exception (SomeAsyncException (..), SomeException, finally, fromException,
                   try, tryJust)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJustM)
import           "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)
import           Data.Functor ((<&>))
import           Data.List.Extra (dropPrefix, dropSuffix, replace)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Tuple.Extra (uncurry3)

import           System.IO (hFlush, stdout)
import           System.Mem.Weak (deRefWeak)
import qualified System.Signal as S
import           System.Time.Extra (sleep)

import           Cardano.Node.Startup (NodeInfo (..))

import           Ouroboros.Network.Socket (ConnectionId (..))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

import ListT qualified
import Control.Concurrent.STM
import StmContainers.Set   qualified as STM.Set
import StmContainers.Bimap qualified as STM.Bimap
import StmContainers.Map   qualified as STM.Map

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
logTrace = traceWith $ showTracing stdoutTracer

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
initConnectedNodes = STM.Set.newIO

initConnectedNodesNames :: IO ConnectedNodesNames
initConnectedNodesNames = STM.Bimap.newIO

initAcceptedMetrics :: IO AcceptedMetrics
initAcceptedMetrics = STM.Map.newIO

initDataPointRequestors :: IO DataPointRequestors
initDataPointRequestors = STM.Map.newIO

initProtocolsBrake :: IO ProtocolsBrake
initProtocolsBrake = newTVarIO False

askNodeName
  :: TracerEnv
  -> NodeId
  -> IO NodeName
askNodeName TracerEnv{teConnectedNodesNames, teDPRequestors, teCurrentDPLock} =
  askNodeNameRaw teConnectedNodesNames teDPRequestors teCurrentDPLock

askNodeNameRaw
  :: ConnectedNodesNames
  -> DataPointRequestors
  -> Lock
  -> NodeId
  -> IO NodeName
askNodeNameRaw connectedNodesNames dpRequestors currentDPLock nodeId@(NodeId anId) = do
  mnodesNames <- atomically do
    STM.Bimap.lookupLeft nodeId connectedNodesNames
  case mnodesNames of
    Just nodeName -> return nodeName
    Nothing -> do
      -- There is no name yet, so we have to ask for 'NodeInfo' datapoint to get the name.
      nodeName <-
        askDataPoint dpRequestors currentDPLock nodeId "NodeInfo" >>= \case
          Nothing -> return anId
          Just NodeInfo{niName} -> return if T.null niName then anId else niName
      -- Store it in for the future using.
      atomically do
        STM.Bimap.insertRight nodeId nodeName connectedNodesNames
      return nodeName

askNodeId
  :: TracerEnv
  -> NodeName
  -> IO (Maybe NodeId)
askNodeId TracerEnv{teConnectedNodesNames} nodeName = do
  atomically do
    STM.Bimap.lookupRight nodeName teConnectedNodesNames

-- | Stop the protocols. As a result, 'MsgDone' will be sent and interaction
--   between acceptor's part and forwarder's part will be finished.
applyBrake :: ProtocolsBrake -> IO ()
applyBrake stopProtocols = atomically do
  modifyTVar' stopProtocols \_ -> True

-- | Like 'liftM2', but for monadic function.
lift2M :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2M f x y = liftA2 (,) x y >>= uncurry f

-- | Like 'liftM3', but for monadic function.
lift3M :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
lift3M f x y z = liftA3 (,,) x y z >>= uncurry3 f

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
  forM_ signals \sig ->
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

fromSTMSet :: Ord a => STM.Set.Set a -> STM (S.Set a)
fromSTMSet set = S.fromList <$> ListT.toList (STM.Set.listT set)

toList :: STM.Set.Set a -> STM [a]
toList set = ListT.toList (STM.Set.listT set)

fromSTMMap :: Ord k => STM.Map.Map k a -> STM (Map k a)
fromSTMMap set = Map.fromList <$> ListT.toList (STM.Map.listT set)

stmMapToList :: STM.Map.Map k a -> STM [(k, a)]
stmMapToList set = ListT.toList (STM.Map.listT set)

fromSTMSetIO :: Ord a => STM.Set.Set a -> IO (S.Set a)
fromSTMSetIO = atomically . fromSTMSet

memberRegistry :: Ord a => a -> Registry a b -> IO Bool
memberRegistry a (Registry registry) = do
  tryReadMVar registry <&> \case
    Nothing -> False
    Just set -> Map.member a set

-- lookupRegistry :: Ord a => a -> Registry a b -> IO Bool
-- lookupRegistry a (Registry registry) = do
--   tryReadMVar registry <&> \case
--     Nothing -> False
--     Just set -> Map.member a set
