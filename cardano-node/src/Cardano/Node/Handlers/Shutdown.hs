{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Handlers.Shutdown
  (
  -- * Generalised shutdown handling
    ShutdownConfig (..)
  , withShutdownHandling

  , ShutdownTrace (..)

  -- * Watch ChainDB for passing a configured slot sync limit threshold,
  --   translating it to a graceful shutdown.
  , maybeSpawnOnSlotSyncedShutdownHandler
  )
where

import           Data.Aeson (FromJSON, ToJSON)
import           Generic.Data (Generic)
import           Generic.Data.Orphans ()
import           Prelude

import           Control.Concurrent.Async (race_)
import           Control.Exception
import           Control.Monad
import           Data.Text (Text, pack)
import qualified GHC.IO.Handle.FD as IO (fdToHandle)
import           System.Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           System.Posix.Types (Fd (Fd))

import           Cardano.Slotting.Slot (WithOrigin (..))
import           "contra-tracer" Control.Tracer
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)
import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo, pointSlot)


data ShutdownTrace
  = ShutdownRequested
  -- ^ Received shutdown request
  | AbnormalShutdown
  -- ^ Non-isEOFError shutdown request
  | ShutdownUnexpectedInput Text
  -- ^ Received shutdown request but found unexpected input in --shutdown-ipc FD:
  | RequestingShutdown Text
  -- ^ Ringing the node shutdown doorbell for reason
  | ShutdownArmedAtSlot SlotNo
  -- ^ Will terminate upon reaching maxSlot
  deriving (Generic, FromJSON, ToJSON)

data ShutdownConfig
  = ShutdownConfig
    { scIPC          :: !(Maybe Fd)
    , scOnSlotSynced :: !(Maybe MaxSlotNo)
    }
    deriving (Eq, Show)

-- | We provide an optional cross-platform method to politely request shut down.
-- The parent process passes us the file descriptor number of the read end of a pipe,
-- via the CLI with @--shutdown-ipc FD@
withShutdownHandling
  :: ShutdownConfig
  -> Tracer IO ShutdownTrace
  -> IO ()
  -- ^ Action to potentially shutdown via file descriptor
  -> IO ()
withShutdownHandling ShutdownConfig{scIPC = Nothing} _ action = action
withShutdownHandling ShutdownConfig{scIPC = Just fd} tr action = do
  race_ (waitForEOF fd) action
 where
   waitForEOF :: Fd -> IO ()
   waitForEOF (Fd fileDesc) = do
     hnd <- IO.fdToHandle fileDesc
     r <- try $ IO.hGetChar hnd
     case r of
       Left e
         | IO.isEOFError e ->
             traceWith tr ShutdownRequested
         | otherwise -> do
             traceWith tr AbnormalShutdown
             throwIO e
       Right inp  ->
         traceWith tr (ShutdownUnexpectedInput . pack $ show inp)

-- | Spawn a thread that would cause node to shutdown upon ChainDB reaching the
-- configuration-defined slot.
maybeSpawnOnSlotSyncedShutdownHandler
  :: ShutdownConfig
  -> Tracer IO ShutdownTrace
  -> ResourceRegistry IO
  -> ChainDB.ChainDB IO blk
  -> IO ()
maybeSpawnOnSlotSyncedShutdownHandler sc tr registry chaindb =
  case scOnSlotSynced sc of
    Just (MaxSlotNo maxSlot) -> do
      traceWith tr (ShutdownArmedAtSlot maxSlot)
      spawnSlotLimitTerminator maxSlot
    _ -> pure ()
 where
  spawnSlotLimitTerminator :: SlotNo -> IO ()
  spawnSlotLimitTerminator maxSlot =
    void $ forkLinkedWatcher registry "slotLimitTerminator" Watcher {
        wFingerprint = id
      , wInitial     = Nothing
      , wNotify      = \case
          Origin -> pure ()
          At cur -> when (cur >= maxSlot) $ do
            traceWith tr (RequestingShutdown $ "spawnSlotLimitTerminator: reached target "
                                                 <> (pack . show) cur)
            throwIO ExitSuccess
      , wReader      = pointSlot <$> ChainDB.getTipPoint chaindb
      }
