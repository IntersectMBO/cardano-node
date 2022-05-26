{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import           Generic.Data.Orphans ()
import           Cardano.Prelude

import           Data.Text (pack)
import qualified GHC.IO.Handle.FD as IO (fdToHandle)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           System.Posix.Types (Fd (Fd))

import           Cardano.Slotting.Slot (WithOrigin (..))
import           "contra-tracer" Control.Tracer
import           Ouroboros.Consensus.Block (Header)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)
import           Ouroboros.Network.Block (HasHeader, BlockNo (..), MaxSlotNo (..), SlotNo, pointSlot)


data ShutdownTrace
  = ShutdownRequested
  -- ^ Received shutdown request
  | AbnormalShutdown
  -- ^ Non-isEOFError shutdown request
  | ShutdownUnexpectedInput Text
  -- ^ Received shutdown request but found unexpected input in --shutdown-ipc FD:
  | RequestingShutdown Text
  -- ^ Ringing the node shutdown doorbell for reason
  | ShutdownArmedAtSlotBlock (Maybe SlotNo) (Maybe BlockNo)
  -- ^ Will terminate upon reaching slotno / blockno
  deriving (Generic, FromJSON, ToJSON)

deriving instance FromJSON BlockNo
deriving instance ToJSON BlockNo

data ShutdownConfig
  = ShutdownConfig
    { scIPC           :: !(Maybe Fd)
    , scOnSlotSynced  :: !(Maybe MaxSlotNo)
    , scOnBlockSynced :: !(Maybe (Maybe BlockNo))
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
  :: HasHeader (Header blk)
  => ShutdownConfig
  -> Tracer IO ShutdownTrace
  -> ResourceRegistry IO
  -> ChainDB.ChainDB IO blk
  -> IO ()
maybeSpawnOnSlotSyncedShutdownHandler sc tr registry chaindb =
  case (scOnSlotSynced sc, scOnBlockSynced sc) of
    (Nothing, Nothing) -> pure ()
    (maxSlot, maxBlock) -> do
      traceWith tr (ShutdownArmedAtSlotBlock (joinMayMaxSlotNo maxSlot) (join maxBlock))
      spawnLimitTerminator (joinMayMaxSlotNo maxSlot) (join maxBlock)
 where
  joinMayMaxSlotNo :: Maybe MaxSlotNo -> Maybe SlotNo
  joinMayMaxSlotNo = \case
    Just (MaxSlotNo x) -> Just x
    _ -> Nothing

  spawnLimitTerminator :: Maybe SlotNo -> Maybe BlockNo -> IO ()
  spawnLimitTerminator maxSlot maxBlock =
    void $ forkLinkedWatcher registry "slotLimitTerminator" Watcher {
        wFingerprint = identity
      , wInitial     = Nothing
      , wReader      = (,)
                       <$> sequence (maxSlot  $> (pointSlot <$> ChainDB.getTipPoint chaindb))
                       <*> sequence (maxBlock $> ChainDB.getTipBlockNo chaindb)
      , wNotify      = \(mSlot, mBlock) -> do
          forM_ ((,) <$> maxSlot <*> mSlot) $ \case
            (slot, At cur) ->
              when (cur >= slot) $ do
                traceWith tr (RequestingShutdown $ "spawnLimitTerminator: reached target slot "
                              <> (pack . show) cur)
                throwIO ExitSuccess
            _ -> pure ()
          forM_ ((,) <$> maxBlock <*> mBlock) $ \case
            (block, At cur) ->
              when (cur >= block) $ do
                traceWith tr (RequestingShutdown $ "spawnLimitTerminator: reached target block "
                              <> (pack . show) cur)
                throwIO ExitSuccess
            _ -> pure ()
      }
