{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | Tests for "Cardano.Node.KES.Persist": the writer that replaces the KES
-- signing key file with the evolved key. Each test hands the writer a key at
-- some absolute period through its 'KESPersistHook', lets the writer finish
-- (the bracket only returns once every pending write is done) and inspects
-- the directory and the traces. The writer is POSIX only, and so are these
-- tests.
module Test.Cardano.Node.KESPersist
  ( tests
  ) where

#ifdef UNIX
import           Cardano.Api (AsType (AsKesKey), File (..), KesKey, SigningKey (..),
                   VerificationKey (..), generateSigningKey, getVerificationKey,
                   serialiseToRawBytes, writeFileTextEnvelope)

import           Cardano.Crypto.KES.Class (SignKeyKES, VerKeyKES, algorithmNameKES,
                   forgetSignKeyKES, unsoundPureSignKeyKESToSoundSignKeyKES)
import           Cardano.Crypto.KES.EnvelopeLocked (KESEnvelopeType (..), LockedKESEnvelope (..),
                   finalizeLockedKESEnvelope, kesWithPeriodTypeString, readLockedKESEnvelope,
                   writeLockedKESEnvelope)
import           Cardano.Crypto.KES.Locked (serialiseSignKeyKESToLockedBuf)
import           Cardano.Crypto.Libsodium.Memory (MLockedForeignPtr, withMLockedForeignPtr)
import           Cardano.Node.KES.Persist (KESPersistConfig (..), TraceKESPersist (..),
                   removeStaleKESTempFiles, withKESPersistWriter)
import           Cardano.Protocol.Crypto (KES, StandardCrypto)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import           Ouroboros.Consensus.Protocol.Praos.AgentClient (KESPersistHook (..))

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (bracket)
import           Control.Monad (forM_, unless, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           "contra-tracer" Control.Tracer (arrow, emit)
import           Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.List (isSuffixOf, sort)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import           System.Directory (createFileLink, listDirectory, renameFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (ReadMode), hClose, openBinaryFile)
import           System.Posix.Files (createLink, fileMode, getFileStatus, setFileMode)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

import           Foreign.C.Types (CChar)
import           Foreign.Ptr (castPtr)

-- | A fresh KES key: the cardano-api signing key (for writing files the way
-- @cardano-cli@ does) and its verification key.
newKey :: (MonadIO m, H.MonadTest m) => m (SigningKey KesKey, VerKeyKES (KES StandardCrypto))
newKey = do
  sk <- H.evalIO $ generateSigningKey AsKesKey
  let KesVerificationKey vk = getVerificationKey sk
  pure (sk, vk)

rawKeyBytes :: SigningKey KesKey -> BS.ByteString
rawKeyBytes = serialiseToRawBytes

-- | The key serialised into a fresh locked buffer, as the hot key hands it to
-- the hook. The hook takes ownership of the buffer.
lockedCopy :: SigningKey KesKey -> IO (MLockedForeignPtr CChar, Int)
lockedCopy (KesSigningKey skUnsound) = do
  sk :: SignKeyKES (KES StandardCrypto) <- unsoundPureSignKeyKESToSoundSignKeyKES skUnsound
  buf <- serialiseSignKeyKESToLockedBuf sk
  forgetSignKeyKES sk
  pure buf

-- | Hand the key at the given period to a hook.
persist :: (KESPeriod -> MLockedForeignPtr CChar -> Int -> IO ()) -> Word64 -> SigningKey KesKey -> IO ()
persist hook period sk = do
  (buf, len) <- lockedCopy sk
  hook (KESPeriod (fromIntegral period)) buf len

-- | Write the key file in the evolution-zero envelope, as @cardano-cli@ does.
writeEvolutionZeroFile :: (MonadIO m, H.MonadTest m) => FilePath -> SigningKey KesKey -> m ()
writeEvolutionZeroFile path sk =
  H.evalIO (writeFileTextEnvelope (File path) Nothing sk) >>= H.evalEither

-- | Write the key file in the period-carrying envelope at the given period.
writeWithPeriodFile :: MonadIO m => FilePath -> Word64 -> SigningKey KesKey -> m ()
writeWithPeriodFile path period sk = liftIO $
  BS.unsafeUseAsCStringLen (rawKeyBytes sk) $ \(key, keyLen) ->
    writeLockedKESEnvelope algorithmName (Just period) key keyLen $ \json jsonLen ->
      BS.packCStringLen (castPtr json, jsonLen) >>= BS.writeFile path

-- | What the key file holds: envelope type, period and raw key bytes.
readKeyFile :: (MonadIO m, H.MonadTest m) => FilePath -> m (KESEnvelopeType, Maybe Word64, BS.ByteString)
readKeyFile path = do
  contents <- H.evalIO $ BS.readFile path
  result <- H.evalIO $ BS.unsafeUseAsCStringLen contents $ \(ptr, len) ->
    readLockedKESEnvelope algorithmName (castPtr ptr) len
  envelope <- H.evalEither result
  key <- H.evalIO $ withMLockedForeignPtr (lkeKey envelope) $ \ptr ->
    BS.packCStringLen (ptr, lkeKeyLen envelope)
  H.evalIO $ finalizeLockedKESEnvelope envelope
  pure (lkeType envelope, lkePeriod envelope, key)

algorithmName :: String
algorithmName = algorithmNameKES (Proxy @(KES StandardCrypto))

-- | Run the writer for the given key file and expected key, with the traces
-- collected in an 'IORef' (newest first), and return them in order once the
-- writer has finished.
withWriter
  :: MonadIO m
  => FilePath
  -> VerKeyKES (KES StandardCrypto)
  -> (IORef [TraceKESPersist] -> (KESPeriod -> MLockedForeignPtr CChar -> Int -> IO ()) -> IO ())
  -> m [TraceKESPersist]
withWriter path vk act = liftIO $ do
  traces <- newIORef []
  let cfg = KESPersistConfig
        { kpcKeyPath = path
        , kpcExpectedVerKey = vk
        , kpcTracer = arrow $ emit $ \ev -> modifyIORef' traces (ev :)
        }
  withKESPersistWriter cfg $ \(KESPersistHook hook) -> act traces hook
  reverse <$> readIORef traces

-- | Run the writer, hand it one key per listed period, return the traces.
runWriter
  :: MonadIO m
  => FilePath
  -> VerKeyKES (KES StandardCrypto)
  -> SigningKey KesKey
  -> [Word64]
  -> m [TraceKESPersist]
runWriter path vk sk periods =
  withWriter path vk $ \_ hook -> forM_ periods $ \period -> persist hook period sk

-- | Wait (up to ten seconds) until the traces satisfy a predicate.
waitForTraces :: IORef [TraceKESPersist] -> ([TraceKESPersist] -> Bool) -> IO ()
waitForTraces ref done = go (1000 :: Int)
  where
    go 0 = fail "waitForTraces: timed out"
    go n = do
      ok <- done <$> readIORef ref
      unless ok $ threadDelay 10_000 >> go (n - 1)

-- | The files in the workspace, without the @module@ file hedgehog-extras
-- puts there.
workspaceFiles :: (MonadIO m, H.MonadTest m) => FilePath -> m [FilePath]
workspaceFiles dir = sort . filter (/= "module") <$> H.evalIO (listDirectory dir)

-- | The temporary files the writer leaves behind, if any.
tempSiblings :: (MonadIO m, H.MonadTest m) => FilePath -> m [FilePath]
tempSiblings dir = filter (".tmp" `isSuffixOf`) <$> workspaceFiles dir

assertSkipped :: (MonadIO m, H.MonadTest m) => Word64 -> Text -> [TraceKESPersist] -> m ()
assertSkipped period fragment traces = case traces of
  [KESPersistSkipped p reason] -> do
    p === period
    H.note_ (T.unpack reason)
    H.assert (fragment `T.isInfixOf` reason)
  _ -> do
    H.note_ ("expected a single KESPersistSkipped, got " <> show traces)
    H.failure

-- | An evolution-zero file is replaced by the period-carrying envelope with
-- the same key bytes, owner-only permissions and no temporary file left.
prop_persistsEvolvedKey :: Property
prop_persistsEvolvedKey = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path sk
  traces <- runWriter path vk sk [17]
  traces === [KESPersisted 17]
  (ty, period, key) <- readKeyFile path
  ty === KESKeyWithPeriod
  period === Just 17
  key === rawKeyBytes sk
  mode <- fileMode <$> H.evalIO (getFileStatus path)
  (mode .&. 0o777) === 0o600
  tempSiblings dir >>= (=== [])
  -- The header the node's own type check looks at.
  contents <- H.evalIO (BS.readFile path)
  H.assert (kesWithPeriodTypeString algorithmName `BS.isInfixOf` contents)

-- | Several evolutions in a row each replace the previous one; a
-- period-carrying file at a lower period is an older evolution and is
-- overwritten.
prop_laterEvolutionsReplaceEarlierOnes :: Property
prop_laterEvolutionsReplaceEarlierOnes = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path sk
  traces <- runWriter path vk sk [3, 4, 9]
  H.note_ (show traces)
  -- The writer may coalesce 3 and 4 if it is slower than the hook.
  H.assert (traces `elem` [[KESPersisted 3, KESPersisted 4, KESPersisted 9], [KESPersisted 3, KESPersisted 9], [KESPersisted 4, KESPersisted 9], [KESPersisted 9]])
  (_, period, _) <- readKeyFile path
  period === Just 9

-- | A file at a higher period than the key being persisted is left alone.
prop_newerFileOnDiskIsNotOverwritten :: Property
prop_newerFileOnDiskIsNotOverwritten = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  writeWithPeriodFile path 20 sk
  before <- H.evalIO (BS.readFile path)
  traces <- runWriter path vk sk [19]
  assertSkipped 19 "higher period" traces
  H.evalIO (BS.readFile path) >>= (=== before)

-- | A file holding another key is left alone.
prop_foreignKeyOnDiskIsNotOverwritten :: Property
prop_foreignKeyOnDiskIsNotOverwritten = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  (other, _) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path other
  before <- H.evalIO (BS.readFile path)
  traces <- runWriter path vk sk [5]
  assertSkipped 5 "different key" traces
  H.evalIO (BS.readFile path) >>= (=== before)

-- | A key file path that is a symbolic link is refused: the link and its
-- target are both left as they were.
prop_symlinkedPathIsRefused :: Property
prop_symlinkedPathIsRefused = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let target = dir </> "real.skey"
      link = dir </> "kes.skey"
  writeEvolutionZeroFile target sk
  H.evalIO $ createFileLink target link
  before <- H.evalIO (BS.readFile target)
  traces <- runWriter link vk sk [5]
  assertSkipped 5 "symbolic link" traces
  H.evalIO (BS.readFile target) >>= (=== before)
  workspaceFiles dir >>= (=== ["kes.skey", "real.skey"])

-- | A file that is not a KES signing key envelope is left alone.
prop_unparsableFileIsNotOverwritten :: Property
prop_unparsableFileIsNotOverwritten = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
      junk = "{\"type\": \"KesSigningKey_ed25519_kes_2^6\", \"description\": \"\", \"cborHex\": \"zz\"}"
  H.evalIO $ BS.writeFile path junk
  traces <- runWriter path vk sk [5]
  assertSkipped 5 "could not be read" traces
  H.evalIO (BS.readFile path) >>= (=== junk)

-- | A missing file is never recreated: removing the key file is how an
-- operator disables forging.
prop_missingFileIsNotRecreated :: Property
prop_missingFileIsNotRecreated = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  traces <- runWriter path vk sk [5]
  assertSkipped 5 "does not exist" traces
  workspaceFiles dir >>= (=== [])

-- | A directory the node cannot write to makes the write fail, with the
-- file untouched and no temporary file left.
prop_readOnlyDirectoryIsAFailure :: Property
prop_readOnlyDirectoryIsAFailure = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path sk
  before <- H.evalIO (BS.readFile path)
  traces <- H.evalIO $ bracket (setFileMode dir 0o500) (const (setFileMode dir 0o700)) $ \() ->
    runWriter path vk sk [5]
  case traces of
    [KESPersistFailed 5 reason] -> H.note_ (T.unpack reason)
    _ -> do
      H.note_ ("expected a single KESPersistFailed, got " <> show traces)
      H.failure
  H.evalIO (BS.readFile path) >>= (=== before)
  tempSiblings dir >>= (=== [])

-- | A file whose period is lower than the last one written is overwritten
-- again, with a trace saying so (something restored an older copy). An
-- evolution-zero file restored after a write is reported as period 0.
prop_periodWentBackwardsIsTraced :: Property
prop_periodWentBackwardsIsTraced = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
      restored = dir </> "restored.skey"
      original = dir </> "original.skey"
  writeEvolutionZeroFile path sk
  writeWithPeriodFile restored 6 sk
  writeEvolutionZeroFile original sk
  traces <- withWriter path vk $ \ref hook -> do
    persist hook 8 sk
    waitForTraces ref (KESPersisted 8 `elem`)
    renameFile restored path
    persist hook 9 sk
    waitForTraces ref (KESPersisted 9 `elem`)
    renameFile original path
    persist hook 10 sk
  traces ===
    [ KESPersisted 8
    , KESFilePeriodWentBackwards 6 8, KESPersisted 9
    , KESFilePeriodWentBackwards 0 9, KESPersisted 10
    ]
  (_, period, _) <- readKeyFile path
  period === Just 10

-- | Keys handed to the hook while the writer is busy are coalesced: only the
-- newest reaches disk, the others are dropped. The writer is held busy by a
-- tracer that blocks while reporting the first write.
prop_coalescesToNewestPeriod :: Property
prop_coalescesToNewestPeriod = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path sk
  traces <- H.evalIO $ do
    ref <- newIORef []
    gate <- newEmptyMVar
    let cfg = KESPersistConfig
          { kpcKeyPath = path
          , kpcExpectedVerKey = vk
          , kpcTracer = arrow $ emit $ \ev -> do
              modifyIORef' ref (ev :)
              when (ev == KESPersisted 11) $ takeMVar gate
          }
    withKESPersistWriter cfg $ \(KESPersistHook hook) -> do
      -- The writer writes 11 and then blocks in the tracer.
      persist hook 11 sk
      waitForTraces ref (KESPersisted 11 `elem`)
      -- 12 waits in the slot; 13 replaces it.
      persist hook 12 sk
      persist hook 13 sk
      putMVar gate ()
    reverse <$> readIORef ref
  traces === [KESPersisted 11, KESPersisted 13]
  (_, period, _) <- readKeyFile path
  period === Just 13

-- | The inode of the replaced file is overwritten with zeros: a descriptor
-- opened on the old file before the write reads only zeros afterwards.
prop_replacedFileIsZeroed :: Property
prop_replacedFileIsZeroed = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path sk
  before <- H.evalIO (BS.readFile path)
  h <- H.evalIO $ openBinaryFile path ReadMode
  traces <- runWriter path vk sk [5]
  traces === [KESPersisted 5]
  old <- H.evalIO $ BS.hGetContents h
  H.evalIO $ hClose h
  BS.length old === BS.length before
  H.assert (BS.all (== 0) old)
  (_, period, _) <- readKeyFile path
  period === Just 5

-- | A file with a second hard link is replaced but not zeroed: the other
-- link keeps the old contents, and the writer says why.
prop_hardLinkedFileIsNotZeroed :: Property
prop_hardLinkedFileIsNotZeroed = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, vk) <- newKey
  let path = dir </> "kes.skey"
      link = dir </> "backup.skey"
  writeEvolutionZeroFile path sk
  H.evalIO $ createLink path link
  before <- H.evalIO (BS.readFile path)
  traces <- runWriter path vk sk [5]
  case traces of
    [KESOrphanNotZeroed reason, KESPersisted 5] -> do
      H.note_ (T.unpack reason)
      H.assert ("1 other hard link" `T.isInfixOf` reason)
    _ -> do
      H.note_ ("expected KESOrphanNotZeroed then KESPersisted 5, got " <> show traces)
      H.failure
  H.evalIO (BS.readFile link) >>= (=== before)
  (_, period, _) <- readKeyFile path
  period === Just 5

-- | Temporary files left by an interrupted write are removed; the key file,
-- other files and a missing directory are left alone.
prop_removesStaleTempFiles :: Property
prop_removesStaleTempFiles = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  (sk, _) <- newKey
  let path = dir </> "kes.skey"
  writeEvolutionZeroFile path sk
  forM_ ["kes.skey1234.tmp", "kes.skeyabcd.tmp", "other.tmp", "kes.skey.bak", "tmp"] $ \f ->
    H.evalIO $ BS.writeFile (dir </> f) "stale"
  traces <- H.evalIO $ do
    ref <- newIORef []
    removeStaleKESTempFiles (arrow $ emit $ \ev -> modifyIORef' ref (ev :)) path
    reverse <$> readIORef ref
  traces === [KESStaleTempRemoved (dir </> "kes.skey1234.tmp"), KESStaleTempRemoved (dir </> "kes.skeyabcd.tmp")]
  workspaceFiles dir >>= (=== ["kes.skey", "kes.skey.bak", "other.tmp", "tmp"])
  readKeyFile path >>= \(ty, _, _) -> ty === KESKeyEvolutionZero
  -- A missing directory is not an error.
  missing <- H.evalIO $ do
    ref <- newIORef []
    removeStaleKESTempFiles (arrow $ emit $ \ev -> modifyIORef' ref (ev :)) (dir </> "nowhere" </> "kes.skey")
    readIORef ref
  missing === []

tests :: IO Bool
tests =
  H.checkSequential $ H.Group "Test.Cardano.Node.KESPersist"
    [ ("prop_persistsEvolvedKey", prop_persistsEvolvedKey)
    , ("prop_laterEvolutionsReplaceEarlierOnes", prop_laterEvolutionsReplaceEarlierOnes)
    , ("prop_newerFileOnDiskIsNotOverwritten", prop_newerFileOnDiskIsNotOverwritten)
    , ("prop_foreignKeyOnDiskIsNotOverwritten", prop_foreignKeyOnDiskIsNotOverwritten)
    , ("prop_symlinkedPathIsRefused", prop_symlinkedPathIsRefused)
    , ("prop_unparsableFileIsNotOverwritten", prop_unparsableFileIsNotOverwritten)
    , ("prop_missingFileIsNotRecreated", prop_missingFileIsNotRecreated)
    , ("prop_readOnlyDirectoryIsAFailure", prop_readOnlyDirectoryIsAFailure)
    , ("prop_periodWentBackwardsIsTraced", prop_periodWentBackwardsIsTraced)
    , ("prop_coalescesToNewestPeriod", prop_coalescesToNewestPeriod)
    , ("prop_replacedFileIsZeroed", prop_replacedFileIsZeroed)
    , ("prop_hardLinkedFileIsNotZeroed", prop_hardLinkedFileIsNotZeroed)
    , ("prop_removesStaleTempFiles", prop_removesStaleTempFiles)
    ]
#else
tests :: IO Bool
tests = pure True
#endif
