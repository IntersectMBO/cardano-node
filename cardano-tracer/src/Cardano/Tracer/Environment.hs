{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExplicitNamespaces #-}
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
  , forever'til
  , forever'tilShutdown

  , type MessageHandler
  , handleInternal
  , handleMessage
  , handleMessageWithShutdown
  , handleMessages
  , handleMessagesWithShutdown
  , handleNoop
  , handleShutdown
  , handleTerminateOnShutdown
  , handleUser
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
import           Control.Exception (AsyncException(ThreadKilled), Exception, throwTo, throwIO, catch)
import           Control.Concurrent.Chan.Unagi (InChan, OutChan, readChan, tryReadChan, tryRead)
import           Control.Concurrent.Extra (Lock)
import           Data.Foldable (traverse_)
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
  HandleInternalMessage :: Tag ex -> (ex -> IO ()) -> InternalMessage

type Tag :: Type -> Type
data Tag a where
  ResourceStatsTag :: Tag (ResourceStats, Trace IO TracerTrace)

-- | Polls the channel until a @Shutdown@ message is received.
blockUntilShutdown :: OutChan (RawMessage internal user) -> IO ()
blockUntilShutdown outChan = go where
  go :: IO ()
  go = readChan outChan >>= \case
         Shutdown -> pure ()
         _        -> go

-- | Serves a channel with a composable `MessageHandler'-function.
--
-- @
-- onInternal    = handleMessageWithShutdown . handleInternal
-- onUser        = handleMessageWithShutdown . handleUser
-- dieOnShutdown = handleMessageWithShutdown mempty
-- @
--
-- These handlers are composable with the function Monoid instance.
--
-- @
--   handleMessage  (handleInternal handle1 <> handleUser handle2)
-- = handleMessages [handleInternal handle1, handleUser handle2]
-- @
--
-- Where @handleMessage (a <> b <> c)@ is equivalent to @handleMessages [a, b, c]@.
--
-- Instantiations:
--
-- @
-- handleMessage :: (RawMessage internal user -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
-- handleMessage :: MessageHandler internal user        -> OutChan (RawMessage internal user) -> IO ()
-- @
handleMessage :: (chan -> IO ()) -> OutChan chan -> IO ()
handleMessage handler outChan = do
  (element, _out) <- tryReadChan outChan
  tryRead element >>= traverse_ @Maybe handler

handleMessages :: [MessageHandler internal user] -> OutChan (RawMessage internal user) -> IO ()
handleMessages = handleMessage . mconcat

handleMessageWithShutdown :: MessageHandler internal user -> OutChan (RawMessage internal user) -> IO ()
handleMessageWithShutdown handler = handleMessage (handler <> handleShutdown)

handleMessagesWithShutdown :: [MessageHandler internal user] -> OutChan (RawMessage internal user) -> IO ()
handleMessagesWithShutdown = handleMessageWithShutdown . mconcat

onRawMessage :: (internal -> IO ()) -> (user -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
onRawMessage internal user = handleMessagesWithShutdown
  [ handleInternal internal
  , handleUser user
  ]

-- onInternal = (`onRawMessage` mempty)
onInternal :: (internal -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
onInternal = handleMessageWithShutdown . handleInternal

-- onUser = (mempty `onRawMessage`)
onUser :: (user -> IO ()) -> OutChan (RawMessage internal user) -> IO ()
onUser = handleMessageWithShutdown . handleUser

-- dieOnShutdown = onRawMessage mempty mempty
dieOnShutdown :: OutChan (RawMessage internal user) -> IO ()
dieOnShutdown = handleMessagesWithShutdown []

-- | An infinite loop (@forever@) that runs an action every iteration,
-- after checking for a message from the out-channel. If a message is
-- received it is handled by the message handler.
--
-- To terminate the loop use handler like @handleTerminateOnShutdown@
-- that throws a @Terminate@ exception.
forever'til :: MessageHandler internal user -> OutChan (RawMessage internal user) -> IO () -> IO ()
forever'til handler outChan action = do
  (element, _out) <- tryReadChan outChan -- non-blocking
  tryRead element >>= \case
    -- Channel empty, returns Nothing immediately if channel is empty.
    Nothing -> do
      action
      forever'til handler outChan action
    Just message -> do
      catch (handler message *> action *> forever'til handler outChan action) \Terminate ->
        pure ()

forever'tilShutdown :: MessageHandler internal user -> OutChan (RawMessage internal user) -> IO () -> IO ()
forever'tilShutdown handler = forever'til (handleTerminateOnShutdown <> handler)

-- | Composable handlers, with a functional Monoidal instance.
--
-- Monoid instance via 'Ap (RawMessage internal user ->) (IO ())'.
--
-- @
--   instance Semigroup (MessageHandler internal user) where
--     (<>) = liftA2 (<>)
--   instance Monoid (MessageHandler internal user) where
--     mempty = pure mempty
-- @
--
-- The handler functions are composed together, the incoming argument
-- gets passed pointwise to each function.
--
-- @
--   (handleShutdown <> handleInternal internal <> handleUser user) Shutdown
-- = handleShutdown Shutdown <> handleInternal internal Shutdown <> handleUser user Shutdown
-- = handleShutdown Shutdown <> mempty <> mempty
-- = myThreadId >>= (`throwTo` ThreadKilled)
--
--   (handleShutdown <> handleInternal internal <> handleUser user) (InternalMessage message)
-- = handleShutdown (InternalMessage message) <> handleInternal internal (InternalMessage message) <> handleUser user (InternalMessage message)
-- = internal message
-- @

type MessageHandler :: Type -> Type -> Type
type MessageHandler internal user = RawMessage internal user -> IO ()

-- | Exception that terminates.
data MessageException = Terminate
  deriving stock Show
  deriving anyclass Exception

handleShutdown :: MessageHandler internal user
handleShutdown = \case
  Shutdown -> myThreadId >>= (`throwTo` ThreadKilled)
  _        -> mempty

-- | Message handler for internal messages.
--
-- @
-- handleInternal :: Monoid m => (internal -> m) -> RawMessage internal user -> m
-- @
handleInternal :: (internal -> IO ()) -> MessageHandler internal user
handleInternal handler = \case
  InternalMessage internal -> handler internal
  _                        -> mempty

-- | Message handler for user messages.
--
-- @
-- handleUser :: Monoid m => (user -> m) -> RawMessage internal user -> m
-- @
handleUser :: (user -> IO ()) -> MessageHandler internal user
handleUser handler = \case
  UserMessage user -> handler user
  _                -> mempty

-- | Message handler that throws a @Terminate@ exception on @Shutdown@.
handleTerminateOnShutdown :: MessageHandler internal user
handleTerminateOnShutdown Shutdown = throwIO Terminate
handleTerminateOnShutdown _        = pure ()

handleNoop :: MessageHandler internal user
handleNoop = mempty

{- | UNSAFE shorthand

  doing \case
    A -> res

is shorthand for

  \case
    A -> res
    _ -> mempty

-- mapMaybe' = mapMaybe . partialBinding
-- Just 10 >>= partialBinding \10 -> "10"
partialBinding :: (a -> b) -> (a -> Maybe b)
partialBinding f a = unsafePerformIO do
  try @PatternMatchFail (evaluate (f a)) >>= \case
    Left (PatternMatchFail err)
      | any (`isSuffixOf` err)
          [ ": Non-exhaustive patterns in lambda\n"
          , ": Non-exhaustive patterns in \\case\n"
          , ": Non-exhaustive patterns in \\cases\n"
          ]
     -> pure @IO (Nothing)
    Left err ->
      throwIO err
    Right b ->
      pure @IO (Just b)

doing :: Monoid m => (a -> m) -> (a -> m)
doing f a = case partialBinding f a of
  Nothing -> mempty
  Just b -> b
-}
