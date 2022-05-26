{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Handlers.Shutdown
  ( SlotOrBlock (..)
  , parseShutdownOnLimit

  -- * Generalised shutdown handling
  , ShutdownConfig (..)
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
import qualified Options.Applicative as Opt
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           System.Posix.Types (Fd (Fd))

import           Cardano.Slotting.Slot (WithOrigin (..))
import           "contra-tracer" Control.Tracer
import           Ouroboros.Consensus.Block (Header)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)
import           Ouroboros.Network.Block (HasHeader, BlockNo (..), SlotNo (..), pointSlot)


data SlotOrBlock f
  = ASlot  !(f SlotNo)
  | ABlock !(f BlockNo)
  deriving (Generic)

deriving instance Eq (SlotOrBlock Identity)
deriving instance Show (SlotOrBlock Identity)
deriving instance FromJSON (SlotOrBlock Identity)
deriving instance ToJSON (SlotOrBlock Identity)

parseShutdownOnLimit :: Opt.Parser (Maybe (SlotOrBlock Identity))
parseShutdownOnLimit =
    optional (Opt.option (ASlot . Identity . SlotNo <$> Opt.auto) (
         Opt.long "shutdown-on-slot-synced"
      <> Opt.metavar "SLOT"
      <> Opt.help "Shut down the process after ChainDB is synced up to the specified slot"
      <> Opt.hidden
    ))
    <|>
    optional (Opt.option (ABlock . Identity . BlockNo <$> Opt.auto) (
         Opt.long "shutdown-on-block-synced"
      <> Opt.metavar "BLOCK"
      <> Opt.help "Shut down the process after ChainDB is synced up to the specified block"
      <> Opt.hidden
    ))

data ShutdownTrace
  = ShutdownRequested
  -- ^ Received shutdown request
  | AbnormalShutdown
  -- ^ Non-isEOFError shutdown request
  | ShutdownUnexpectedInput Text
  -- ^ Received shutdown request but found unexpected input in --shutdown-ipc FD:
  | RequestingShutdown Text
  -- ^ Ringing the node shutdown doorbell for reason
  | ShutdownArmedAt (SlotOrBlock Identity)
  -- ^ Will terminate upon reaching a ChainDB sync limit
  deriving (Generic, FromJSON, ToJSON)

deriving instance FromJSON BlockNo
deriving instance ToJSON BlockNo

newtype AndWithOrigin a = AndWithOrigin (a, WithOrigin a) deriving (Eq)

deriving instance Eq (SlotOrBlock AndWithOrigin)

data ShutdownConfig
  = ShutdownConfig
    { scIPC         :: !(Maybe Fd)
    , scOnSyncLimit :: !(Maybe (SlotOrBlock Identity))
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
  case scOnSyncLimit sc of
    Nothing -> pure ()
    Just lim -> do
      traceWith tr (ShutdownArmedAt lim)
      spawnLimitTerminator lim
 where
  spawnLimitTerminator :: SlotOrBlock Identity -> IO ()
  spawnLimitTerminator limit =
    void $ forkLinkedWatcher registry "slotLimitTerminator" Watcher {
        wFingerprint = identity
      , wInitial     = Nothing
      , wReader      =
          case limit of
            ASlot  (Identity x) -> ASlot  . AndWithOrigin . (x,) <$>
                                   (pointSlot <$> ChainDB.getTipPoint chaindb)
            ABlock (Identity x) -> ABlock . AndWithOrigin . (x,) <$>
                                   ChainDB.getTipBlockNo chaindb
      , wNotify      = \case
          ASlot (AndWithOrigin (lim, At cur)) ->
              when (cur >= lim) $ do
                traceWith tr (RequestingShutdown $ "spawnLimitTerminator: reached target slot "
                              <> (pack . show) cur)
                throwIO ExitSuccess
          ABlock (AndWithOrigin (lim, At cur)) ->
              when (cur >= lim) $ do
                traceWith tr (RequestingShutdown $ "spawnLimitTerminator: reached target block "
                              <> (pack . show) cur)
                throwIO ExitSuccess
          _ -> pure ()
      }
