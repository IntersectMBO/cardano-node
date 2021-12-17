{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Handlers.Shutdown
  (
  -- * Generalised shutdown handling
    ShutdownConfig (..)
  , PartialShutdownConfig (..)
  , ShutdownFDs
  , withShutdownHandling

  -- * Requesting shutdown
  , ShutdownDoorbell
  , getShutdownDoorbell
  , triggerShutdown

  , ShutdownTrace (..)

  -- * Watch ChainDB for passing a configured slot sync limit threshold,
  --   translating it to a graceful shutdown.
  , maybeSpawnOnSlotSyncedShutdownHandler
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)

import qualified Control.Concurrent.Async as Async
import           Data.Aeson (FromJSON, ToJSON)
import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()

import qualified GHC.IO.Handle.FD as IO (fdToHandle)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           System.Posix.Types (Fd (Fd))
import qualified System.Process as IO (createPipeFd)

import           Cardano.Slotting.Slot (WithOrigin (..))
import "contra-tracer" Control.Tracer
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (Watcher(..), forkLinkedWatcher)
import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo, pointSlot)

-- | 'ShutdownFDs' mediate the graceful shutdown requests,
-- either external or internal to the process.
--
-- In the external mediation case, the parent process passes us the file descriptor
-- number of the read end of a pipe, via the CLI with @--shutdown-ipc FD@.
-- In the internal mediation case, we create our own pipe.
-- In both cases we store the accessible ends in 'ShutdownFDs'.
--
-- In either case, if the write end gets closed, either deliberately:
--   - by internal call of 'triggerShutdown' on 'ShutdownFDs', or
--   - by parent process
-- ..or automatically, because the parent process itself terminated,
-- then we initiate a clean shutdown.
data ShutdownFDs
  = NoShutdownFDs
  | ExternalShutdown !ShutdownListener
  -- ^ Shutdown to be provided by external process.
  | InternalShutdown !ShutdownListener !ShutdownDoorbell
  -- ^ Shutdown to be provided from within the process.

-- | FD used to send an EOF-based request for shutdown.
newtype ShutdownDoorbell = ShutdownDoorbell { _doorbellFd :: Fd }

-- | FD we're listening on for the EOF signalling the shutdown.
newtype ShutdownListener = ShutdownListener { _listenerFd :: Fd }

data ShutdownTrace
  = ShutdownRequested -- "received shutdown request"
  | RequestingShutdown Text -- "Ringing the node shutdown doorbell:  " <> reason
  | ShutdownArmedAtSlot SlotNo -- "will terminate upon reaching " <> pack (show maxSlot)
  deriving (Generic, FromJSON, ToJSON)

-- | Gracefully handle shutdown requests, if requested by 'ShutdownFDs'.
--
-- The file descriptor wrapped in a 'ShutdownListener' designates the
-- receiving end of the shutdown signalling communication channel.
-- The opposite end might be either internal or external to the node process.
withShutdownHandler :: Maybe ShutdownListener -> Tracer IO ShutdownTrace -> IO () -> IO ()
withShutdownHandler listener tr action
  | Just (ShutdownListener fd) <- listener =
      Async.race_
        (wrapUninterruptableIO $ waitForEOF fd)
        action
  | otherwise = action
  where
    waitForEOF :: Fd -> IO ()
    waitForEOF (Fd fd) = do
      hnd <- IO.fdToHandle fd
      r   <- try $ IO.hGetChar hnd
      case r of
        Left e
          | IO.isEOFError e -> traceWith tr ShutdownRequested
          | otherwise       -> throwIO e

        Right _  ->
          throwIO $ IO.userError "--shutdown-ipc FD does not expect input"

sfdsListener :: ShutdownFDs -> Maybe ShutdownListener
sfdsListener = \case
  ExternalShutdown r -> Just r
  InternalShutdown r _w -> Just r
  _ -> Nothing

-- | Windows blocking file IO calls like 'hGetChar' are not interruptable by
-- asynchronous exceptions, as used by async 'cancel' (as of base-4.12).
--
-- This wrapper works around that problem by running the blocking IO in a
-- separate thread. If the parent thread receives an async cancel then it
-- will return. Note however that in this circumstance the child thread may
-- continue and remain blocked, leading to a leak of the thread. As such this
-- is only reasonable to use a fixed number of times for the whole process.
--
wrapUninterruptableIO :: IO a -> IO a
wrapUninterruptableIO action = async action >>= wait

-- | If 'ShutdownFDs' supports internal shutdown requests,
-- return its shutdown doorbell.
getShutdownDoorbell :: ShutdownFDs -> Maybe ShutdownDoorbell
getShutdownDoorbell (InternalShutdown _l doorbell) = Just doorbell
getShutdownDoorbell _ = Nothing

-- | Given the 'ShutdownDoorbell' component of 'ShutdownFDs',
--   and an explanation of the reason, request a graceful shutdown.
triggerShutdown :: ShutdownDoorbell -> Tracer IO ShutdownTrace -> Text -> IO ()
triggerShutdown (ShutdownDoorbell (Fd shutFd)) tr reason = do
  traceWith tr (RequestingShutdown reason)
  IO.hClose =<< IO.fdToHandle shutFd

data ShutdownConfig
  = ShutdownConfig
    { scIPC          :: !(Maybe Fd)
    , scOnSlotSynced :: !MaxSlotNo
    }
    deriving (Eq, Show)

data PartialShutdownConfig
  = PartialShutdownConfig
    { pscIPC          :: !(Last (Maybe Fd))
    , pscOnSlotSynced :: !(Last MaxSlotNo)
    }
    deriving (Generic, Eq, Show)

instance Semigroup PartialShutdownConfig where
  (<>) = gmappend

-- | We provide an optional cross-platform method to politely request shut down.
--
-- For the duration of 'action', we gracefully handle shutdown requests,
-- external or internal, as requested by configuration in 'NodeCLI',
-- while allocating corresponding 'ShutdownFDs', and providing them to the 'action'.
withShutdownHandling
  :: ShutdownConfig
  -> Tracer IO ShutdownTrace
  -> (ShutdownFDs -> IO ())
  -> IO ()
withShutdownHandling sc tr action = do
  sfds <- decideShutdownFds sc
  withShutdownHandler (sfdsListener sfds) tr (action sfds)
 where
   decideShutdownFds :: ShutdownConfig -> IO ShutdownFDs
   decideShutdownFds ShutdownConfig{scIPC = Just fd} =
     pure $ ExternalShutdown (ShutdownListener fd)
   decideShutdownFds ShutdownConfig{scOnSlotSynced = MaxSlotNo{}} =
     mkInternalShutdown
   decideShutdownFds _ = pure NoShutdownFDs

   mkInternalShutdown :: IO ShutdownFDs
   mkInternalShutdown = do
     (r, w) <- IO.createPipeFd
     pure $ InternalShutdown (ShutdownListener $ Fd r) (ShutdownDoorbell $ Fd w)

-- | If configuration in 'NodeCLI' and 'ShutdownFDs' agree,
-- spawn a thread that would cause node to shutdown upon ChainDB reaching the
-- configuration-defined slot.
maybeSpawnOnSlotSyncedShutdownHandler
  :: ShutdownConfig
  -> ShutdownFDs
  -> Tracer IO ShutdownTrace
  -> ResourceRegistry IO
  -> ChainDB.ChainDB IO blk
  -> IO ()
maybeSpawnOnSlotSyncedShutdownHandler sc sfds tr registry chaindb =
  case (scOnSlotSynced sc, sfds) of
    (MaxSlotNo maxSlot, InternalShutdown _sl sd) -> do
      traceWith tr (ShutdownArmedAtSlot maxSlot)
      spawnSlotLimitTerminator maxSlot sd
    (MaxSlotNo{}, _) -> panic
      "internal error: slot-limited shutdown requested, but no proper ShutdownFDs passed."
    _ -> pure ()
 where
  spawnSlotLimitTerminator :: SlotNo -> ShutdownDoorbell -> IO ()
  spawnSlotLimitTerminator maxSlot sd =
    void $ forkLinkedWatcher registry "slotLimitTerminator" Watcher {
        wFingerprint = identity
      , wInitial     = Nothing
      , wNotify      = \case
          Origin -> pure ()
          At cur -> when (cur >= maxSlot) $
            triggerShutdown sd tr
            ("spawnSlotLimitTerminator: reached target " <> show cur)
      , wReader      = pointSlot <$> ChainDB.getTipPoint chaindb
      }
