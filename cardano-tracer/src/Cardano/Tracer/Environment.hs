{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Environment
  ( TracerEnv (..)
  , TracerEnvRTView (..)
  , RawMessage (..)
  , InternalMessage (..)
  , Tag (..)
  , CardanoTracerMessage
  , onRawMessage
  , onInternal
  , onUser
  , blockUntilShutdown
  , dieOnShutdown
  , forever'tilShutdown
  ) where

import           Cardano.Logging.Types
import           Cardano.Logging.Resources.Types (ResourceStats)
import           Cardano.Tracer.Configuration
#if RTVIEW
import           Cardano.Tracer.Handlers.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.State.TraceObjects
#endif
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types

import           Control.Concurrent (myThreadId)
import           Control.Exception (AsyncException(ThreadKilled), throwTo)
import           Control.Concurrent.Chan.Unagi (InChan, OutChan, readChan, tryReadChan, tryRead)
import           Control.Concurrent.Extra (Lock)
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)
import           Data.Kind (Type)

-- | Environment for all functions.
data TracerEnv = TracerEnv
  { teConfig                :: !TracerConfig
  , teConnectedNodes        :: !ConnectedNodes
  , teConnectedNodesNames   :: !ConnectedNodesNames
  , teAcceptedMetrics       :: !AcceptedMetrics
  , teCurrentLogLock        :: !Lock
  , teCurrentDPLock         :: !Lock
  , teDPRequestors          :: !DataPointRequestors
  , teProtocolsBrake        :: !ProtocolsBrake
  , teTracer                :: !(Trace IO TracerTrace)
  , teReforwardTraceObjects :: !([TraceObject] -> IO ())
  , teRegistry              :: !HandleRegistry
  , teStateDir              :: !(Maybe FilePath)
  , teMetricsHelp           :: ![(Text, Builder)]
  , teInChan                :: !(InChan (CardanoTracerMessage ()))
  }

#if RTVIEW
-- | Environment for all functions.
data TracerEnvRTView = TracerEnvRTView
  { teSavedTO               :: !SavedTraceObjects
  , teBlockchainHistory     :: !BlockchainHistory
  , teResourcesHistory      :: !ResourcesHistory
  , teTxHistory             :: !TransactionsHistory
  , teEventsQueues          :: !EventsQueues
  , teRTViewPageOpened      :: !WebPageStatus
  }
#else
data TracerEnvRTView = TracerEnvRTView
#endif

type CardanoTracerMessage userMsg = RawMessage InternalMessage userMsg

type RawMessage :: Type -> Type -> Type
data RawMessage internal user
  = Shutdown
  | InternalMessage internal
  | UserMessage     user

type InternalMessage :: Type
data InternalMessage where
  ResourceMessage :: Tag ex -> (ex -> IO ()) -> InternalMessage

type Tag :: Type -> Type
data Tag a where
  TagResource :: Tag (ResourceStats, Trace IO TracerTrace)

blockUntilShutdown :: OutChan (RawMessage internal user) -> IO ()
blockUntilShutdown outChan = go where
  go :: IO ()
  go = readChan outChan >>= \case
         Shutdown -> pure ()
         _        -> go

onRawMessage :: (internal -> IO ()) -> (user -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
onRawMessage internalAction userAction outChan = do
  (element, _out) <- tryReadChan outChan
  tryRead element >>= \case
    Just Shutdown                   -> myThreadId >>= (`throwTo` ThreadKilled)
    Just (InternalMessage internal) -> internalAction internal
    Just (UserMessage user)         -> userAction user
    Nothing                         -> pure ()

onInternal :: (internal -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
onInternal = (`onRawMessage` mempty)

onUser :: (user -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
onUser = (mempty `onRawMessage`)

dieOnShutdown :: OutChan (RawMessage internal user) -> IO ()
dieOnShutdown = onRawMessage mempty mempty

forever'tilShutdown :: OutChan (RawMessage internal user) -> IO () -> IO ()
forever'tilShutdown outChan action = do
  (element, _out) <- tryReadChan outChan
  tryRead element >>= \case
    Just Shutdown -> pure ()
    Just _  -> forever'tilShutdown outChan action
    Nothing -> action *> forever'tilShutdown outChan action
