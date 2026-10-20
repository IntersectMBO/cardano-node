{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
#define UNIX
#else
{-# OPTIONS_GHC -Wno-unused-imports #-}
#endif

-- | Persisting the evolved KES signing key back to its file.
--
-- A file-based block producer loads its KES signing key at the evolution the
-- file holds and evolves it in memory, erasing every earlier evolution. Until
-- the file is rewritten, the evolution-zero key, which can sign for every
-- period of the operational certificate, stays on disk. The writer here
-- replaces the file with the current evolution after every evolution the
-- hot key performs, so that a later reader of the path only ever finds the
-- key the node is currently signing with.
--
-- The hot key hands the writer a serialised copy of the evolved key in
-- locked memory through a 'KESPersistHook' (see
-- "Ouroboros.Consensus.Protocol.Praos.AgentClient"); the hook never blocks
-- the forging thread and never throws. A single writer thread performs the
-- writes, coalescing to the newest evolution when it falls behind. Each
-- write re-reads the file first and refuses to overwrite anything it does not
-- recognise as an older evolution of the same key, writes a temporary file
-- next to the target, syncs, renames it over the target, syncs the
-- directory, and verifies the result by reading it back. Every failure is
-- traced as a 'TraceKESPersist' and never affects forging.
--
-- The writer is POSIX only; on other platforms the hook only erases the
-- buffers it is handed and traces that persistence is unavailable.
module Cardano.Node.KES.Persist
  ( KESPersistConfig (..)
  , TraceKESPersist (..)
  , withKESPersistWriter
  ) where

import           Cardano.Crypto.KES.Class (VerKeyKES, algorithmNameKES, deriveVerKeyKES,
                   forgetSignKeyKES, signKeySizeKES)
import           Cardano.Crypto.KES.EnvelopeLocked (KESEnvelopeType (..), LockedKESEnvelope (..),
                   finalizeLockedKESEnvelope, readLockedKESEnvelope, writeLockedKESEnvelope)
import           Cardano.Crypto.KES.Locked (deserialiseSignKeyKESFromLockedBuf)
import           Cardano.Crypto.Libsodium.Memory (MLockedForeignPtr, finalizeMLockedForeignPtr,
                   withMLockedForeignPtr, zeroMem)
import           Cardano.Protocol.Crypto (KES, StandardCrypto)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import           Ouroboros.Consensus.Protocol.Praos.AgentClient (KESPersistHook (..),
                   MonadKESAgent (..))

import           Control.Concurrent.Async (wait, withAsync)
import           Control.Concurrent.STM (atomically, newTVarIO, readTVar, retry, writeTVar)
import           Control.Exception (Exception (..), SomeAsyncException (..), SomeException, bracket,
                   bracketOnError, catch, finally, onException, throwIO)
import           Control.Monad (unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Tracer (Tracer, traceWith)
import           Data.Either (fromLeft)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import           System.Directory (canonicalizePath, doesPathExist, pathIsSymbolicLink, removeFile,
                   renameFile)
import           System.FilePath (splitFileName, takeDirectory, (<.>))
import qualified System.IO as IO
import           System.IO (hPutBuf, openBinaryTempFile)
#ifdef UNIX
import           System.Posix.Files (deviceID, getFileStatus)
import           System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, handleToFd, openFd)
import           System.Posix.Unistd (fileSynchronise)
#endif

import           Foreign.C.Types (CChar)

-- | What the writer needs to know.
data KESPersistConfig = KESPersistConfig
  { kpcKeyPath        :: FilePath
    -- ^ The KES signing key file given on the command line, as given.
  , kpcExpectedVerKey :: VerKeyKES (KES StandardCrypto)
    -- ^ The hot key of the operational certificate. Only a file holding a
    -- key with this verification key is ever overwritten, and the written
    -- file must read back with it.
  , kpcTracer         :: Tracer IO TraceKESPersist
  }

-- | What the writer reports. Periods are absolute KES periods; the key
-- itself never appears in a trace.
data TraceKESPersist
  = KESPersisted Word64
    -- ^ The key at this period was written and read back successfully.
  | KESPersistSkipped Word64 Text
    -- ^ The key at this period was not written, because of what the writer
    -- found at the path: no file, a symbolic link, a mount point, a file it
    -- cannot parse, a file holding another key, or a file at a higher
    -- period. The text names the reason.
  | KESPersistFailed Word64 Text
    -- ^ Writing the key at this period failed: an IO error, or the file did
    -- not read back as written. The text names the reason.
  | KESFilePeriodWentBackwards Word64 Word64
    -- ^ The file held a lower period than the writer last wrote (the period
    -- found on disk, the period last written): something restored an older
    -- copy of the key file, see the design notes on configuration
    -- management. The file is overwritten again. An evolution-zero
    -- envelope carries no period and is reported as period 0.
  deriving (Show, Eq)

-- | Run an action with a 'KESPersistHook' backed by a writer thread for the
-- configured key file. The hook takes ownership of the buffer it is handed
-- (as 'Ouroboros.Consensus.Protocol.Ledger.HotKey.KESPersistCallback'
-- requires), stores it for the writer and returns at once; when the writer
-- has not yet picked up an earlier buffer, that one is erased and replaced,
-- so only the newest evolution reaches disk. When the action returns, the
-- hook stops accepting buffers (erasing any it is still handed), the writer
-- finishes the write in progress and any pending one, and only then does
-- this return.
withKESPersistWriter :: KESPersistConfig -> (KESPersistHook -> IO a) -> IO a
#ifdef UNIX
withKESPersistWriter cfg@KESPersistConfig{kpcTracer} k = do
  slot <- newTVarIO Nothing
  closed <- newTVarIO False
  lastWritten <- newIORef Nothing

  let hook = KESPersistHook $ \(KESPeriod period) buf len -> do
        -- Never throws (short of an asynchronous exception, after which the
        -- buffer has been erased): the transaction cannot fail, and the
        -- buffer is erased on every path that does not store it.
        let pending = Pending (fromIntegral period) buf len
        evicted <-
          atomically
            ( do
                isClosed <- readTVar closed
                if isClosed
                  then pure (Just pending)
                  else do
                    old <- readTVar slot
                    writeTVar slot (Just pending)
                    pure old
            )
            `onException` erasePending pending
        mapM_ erasePending evicted

      takeNext = atomically $ do
        readTVar slot >>= \case
          Just pending -> do
            writeTVar slot Nothing
            pure (Just pending)
          Nothing -> do
            isClosed <- readTVar closed
            if isClosed then pure Nothing else retry

      writerLoop =
        takeNext >>= \case
          Nothing -> pure ()
          Just pending@(Pending period buf len) -> do
            ( persistOnce cfg lastWritten period buf len
                `catchSync` (pure . KESPersistFailed period . describeException)
                >>= traceWith kpcTracer
              )
              `finally` erasePending pending
            writerLoop

  withAsync writerLoop $ \writer ->
    k hook `finally` (atomically (writeTVar closed True) >> wait writer)

-- | A serialised key waiting to be written.
data Pending = Pending !Word64 !(MLockedForeignPtr CChar) !Int

erasePending :: Pending -> IO ()
erasePending (Pending _ buf len) = eraseBuffer buf len

-- | One attempt to persist the key at the given period. Side observations
-- (a file whose period went backwards) are traced here; the outcome is
-- returned for the caller to trace once the buffer is erased.
persistOnce
  :: KESPersistConfig
  -> IORef (Maybe Word64)
  -> Word64
  -> MLockedForeignPtr CChar
  -> Int
  -> IO TraceKESPersist
persistOnce KESPersistConfig{kpcKeyPath, kpcExpectedVerKey, kpcTracer} lastWritten period keyBuf keyLen =
  fromLeft (KESPersisted period) <$> runExceptT (do
    -- Where the file really is. A symbolic link is refused rather than
    -- followed: renaming over the link would replace the link itself, and
    -- whoever set the link up did not expect the node to write there.
    exists <- io (doesPathExist kpcKeyPath)
    unless exists $ skip "the key file does not exist"
    isLink <- io (pathIsSymbolicLink kpcKeyPath)
    when isLink $ skip "the key file path is a symbolic link"
    path <- io (canonicalizePath kpcKeyPath)
    let (dir, _) = splitFileName path

    -- A file that is itself a mount point (a bind-mounted secret) cannot be
    -- renamed over.
    fileStatus <- io (getFileStatus path)
    dirStatus <- io (getFileStatus (takeDirectory path))
    when (deviceID fileStatus /= deviceID dirStatus) $
      skip "the key file is a mount point"

    -- Compare and swap: only an older evolution of this very key is
    -- replaced. Anything else at the path is left alone.
    onDisk <- io (readKeyFile path)
    case onDisk of
      Left reason -> skip ("the key file on disk could not be read: " <> reason)
      Right (OnDisk vk mPeriod) -> do
        when (vk /= kpcExpectedVerKey) $
          skip "the key file on disk holds a different key"
        case mPeriod of
          Just p | p > period -> skip "the key file on disk is at a higher period"
          _ -> pure ()
        previous <- io (readIORef lastWritten)
        case previous of
          Just lastP | fromMaybe 0 mPeriod < lastP ->
            io $ traceWith kpcTracer (KESFilePeriodWentBackwards (fromMaybe 0 mPeriod) lastP)
          _ -> pure ()

    -- Write a temporary file in the same directory, sync it, rename it over
    -- the target and sync the directory, so that the path always holds a
    -- complete file and the rename survives a power failure.
    io $ withMLockedForeignPtr keyBuf $ \key ->
      writeLockedKESEnvelope algorithmName (Just period) key keyLen $ \json jsonLen ->
        bracketOnError
          (openBinaryTempFile dir (snd (splitFileName path) <.> "tmp"))
          (\(tmpPath, h) -> IO.hClose h >> removeFile tmpPath)
          (\(tmpPath, h) -> do
              hPutBuf h json jsonLen
              -- 'handleToFd' flushes and closes the handle.
              bracket (handleToFd h) closeFd fileSynchronise
              renameFile tmpPath path
              bracket (openFd dir ReadOnly defaultFileFlags) closeFd fileSynchronise)

    -- Read back what is now at the path.
    written <- io (readKeyFile path)
    case written of
      Left reason -> failed ("the written file does not read back: " <> reason)
      Right (OnDisk vk mPeriod) -> do
        when (vk /= kpcExpectedVerKey) $
          failed "the written file reads back with a different key"
        when (mPeriod /= Just period) $
          failed "the written file reads back with a different period"
    io (writeIORef lastWritten (Just period)))
  where
    io :: IO a -> ExceptT TraceKESPersist IO a
    io = liftIO

    skip, failed :: Text -> ExceptT TraceKESPersist IO a
    skip = throwE . KESPersistSkipped period
    failed = throwE . KESPersistFailed period

-- | What a KES signing key file holds, as far as the writer cares.
data OnDisk = OnDisk !(VerKeyKES (KES StandardCrypto)) !(Maybe Word64)

-- | Read the key file through locked memory and derive its verification key.
-- The reason for a failure never contains any of the file's contents.
readKeyFile :: FilePath -> IO (Either Text OnDisk)
readKeyFile path =
  withKESKeyFileLocked path parse `catchSync` (pure . Left . describeException)
  where
    parse filePtr fileLen =
      readLockedKESEnvelope algorithmName filePtr fileLen >>= \case
        Left err -> pure (Left (T.pack (show err)))
        Right envelope ->
          ( do
              let keyLen = lkeKeyLen envelope
              if keyLen /= expectedKeyLen
                then pure (Left ("wrong key length " <> T.pack (show keyLen)))
                else do
                  sk <- withMLockedForeignPtr (lkeKey envelope) $ \key ->
                    deserialiseSignKeyKESFromLockedBuf @(KES StandardCrypto) key keyLen
                  vk <- deriveVerKeyKES sk `finally` forgetSignKeyKES sk
                  let mPeriod = case lkeType envelope of
                        KESKeyEvolutionZero -> Nothing
                        KESKeyWithPeriod -> lkePeriod envelope
                  pure (Right (OnDisk vk mPeriod))
          )
            `finally` finalizeLockedKESEnvelope envelope

    expectedKeyLen = fromIntegral (signKeySizeKES (Proxy @(KES StandardCrypto)))

algorithmName :: String
algorithmName = algorithmNameKES (Proxy @(KES StandardCrypto))

-- | Catch synchronous exceptions only; asynchronous ones (the node shutting
-- down) propagate.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync act handler =
  act `catch` \e -> case fromException e of
    Just (SomeAsyncException _) -> throwIO e
    Nothing -> handler e

describeException :: SomeException -> Text
describeException = T.pack . displayException
#else
withKESPersistWriter KESPersistConfig{kpcTracer} k =
  k $ KESPersistHook $ \(KESPeriod period) buf len -> do
    eraseBuffer buf len
    traceWith kpcTracer (KESPersistSkipped (fromIntegral period) "persisting the KES key is not supported on this platform")
#endif

-- | Zero and release a locked buffer.
eraseBuffer :: MLockedForeignPtr CChar -> Int -> IO ()
eraseBuffer buf len = do
  withMLockedForeignPtr buf $ \ptr -> zeroMem ptr (fromIntegral len)
  finalizeMLockedForeignPtr buf
