{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Utils
  ( applyBrake
  , connIdToNodeId
  , initAcceptedMetrics
  , initConnectedNodes
  , initDataPointRequestors
  , initProtocolsBrake
  , lift2M
  , lift3M
  , runInLoop
  , showProblemIfAny
  ) where

import           Control.Applicative (liftA2, liftA3)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import           Control.Exception (SomeException, SomeAsyncException (..),
                   fromException, try, tryJust)
import           "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Tuple.Extra (uncurry3)
import           System.Time.Extra (sleep)

import           Ouroboros.Network.Socket (ConnectionId (..))

import           Cardano.Tracer.Configuration (Verbosity (..))
import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes,
                   DataPointRequestors, NodeId (..), ProtocolsBrake)

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
  preparedAddress =
      T.replace "\\\\.\\pipe\\" "" -- For Windows.
    . T.replace "--" ""
    . T.replace "LocalAddress" "" -- There are only local addresses by design.
    . T.replace " " "-"
    . T.replace "\"" ""
    . T.replace "/" "-"
    . T.pack
    $ show remoteAddress

initConnectedNodes :: IO ConnectedNodes
initConnectedNodes = newTVarIO S.empty

initAcceptedMetrics :: IO AcceptedMetrics
initAcceptedMetrics = newTVarIO M.empty

initDataPointRequestors :: IO DataPointRequestors
initDataPointRequestors = newTVarIO M.empty

initProtocolsBrake :: IO ProtocolsBrake
initProtocolsBrake = newTVarIO False

-- | Stop the protocols. As a result, 'MsgDone' will be sent and interaction
--   between acceptor's part and forwarder's part will be finished.
applyBrake :: ProtocolsBrake -> IO ()
applyBrake stopProtocols = atomically $ modifyTVar' stopProtocols . const $ True

-- | Like 'liftM2', but for monadic function.
lift2M :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2M f x y = liftA2 (,) x y >>= uncurry f

-- | Like 'liftM3', but for monadic function.
lift3M :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
lift3M f x y z = liftA3 (,,) x y z >>= uncurry3 f
