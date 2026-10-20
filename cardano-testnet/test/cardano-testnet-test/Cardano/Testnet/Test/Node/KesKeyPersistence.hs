{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | Persistence of the evolved KES signing key (@PersistEvolvedKESKey@) on a
-- single-SPO testnet whose KES periods last a few seconds. The tests watch
-- the node's stdout for the @Forge.KESPersist@ traces, inspect the key file
-- between periods, restart the node from the persisted file and inject the
-- failures an operator's deployment can produce.
--
-- Execute these tests with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/KES key persistence/"'@
module Cardano.Testnet.Test.Node.KesKeyPersistence
  ( hprop_persist_evolved_kes_key_across_evolutions_and_restarts
  , hprop_kes_key_file_ahead_of_clock_stops_forging
  , hprop_kes_persist_failure_injections_keep_forging
  ) where

import           Cardano.Api (AsType (..), File (..), SigningKey (..), deserialiseFromRawBytes,
                   generateSigningKey, serialiseToRawBytes, writeFileTextEnvelope)

import           Cardano.Crypto.KES.Class (algorithmNameKES, unsoundPureUpdateKES)
import           Cardano.Crypto.KES.EnvelopeLocked (KESEnvelopeType (..), LockedKESEnvelope (..),
                   finalizeLockedKESEnvelope, readLockedKESEnvelope, writeLockedKESEnvelope)
import           Cardano.Crypto.Libsodium.Memory (withMLockedForeignPtr)
import           Cardano.Protocol.Crypto (KES, StandardCrypto)
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Unsafe as BS
import           Data.Default.Class (def)
import           Data.Foldable (foldl')
import           Data.List (isInfixOf)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           System.Directory (copyFile, createFileLink, removeFile, renameFile)
import           System.FilePath (takeDirectory, (</>))
#ifdef UNIX
import           System.Posix.Files (createLink, setFileMode)
#endif
import           System.Process (terminateProcess, waitForProcess)

import           Testnet.Defaults (defaultNodeDataDir, defaultNodeName,
                   defaultSpoByronDelegateKeyFp, defaultSpoByronDelegationCertFp,
                   defaultSpoKesSKeyFp, defaultSpoOpcertCertFp, defaultSpoVrfSKeyFp)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Runtime (startNode)
import           Testnet.Signal (reloadNodeConfiguration)
import           Testnet.Start.Cardano (liftToIntegration)
import           Testnet.Start.Types (GenesisHashesPolicy (..), PraosCredentialsSource (..))

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

import           Foreign.Ptr (castPtr)

-- * The testnet

-- | Slots per KES period on these testnets: with 0.1 s slots, a period
-- lasts five seconds, so the key evolves every five seconds and 60
-- evolutions last five minutes.
slotsPerKESPeriod :: Int
slotsPerKESPeriod = 50

-- | A single-SPO testnet whose SPO reads its KES key from a file and
-- persists the evolved key, with short KES periods. Returns the SPO node and
-- the workspace directory (every default path is relative to it).
startPersistingTestnet :: HasCallStack => FilePath -> H.Integration (TestnetRuntime, TestnetNode, FilePath)
startPersistingTestnet tempAbsBasePath' = withFrozenCallStack $ do
  conf0 <- mkConf tempAbsBasePath'
  -- The Shelley genesis is rewritten below, so the configuration must not
  -- pin its hash.
  let conf@Conf { tempAbsPath = TmpAbsolutePath work } = conf0 { genesisHashesPolicy = WithoutHashes }
      creationOptions = def
  liftToIntegration $ createTestnetEnv creationOptions conf

  -- Short KES periods.
  let shelleyGenesisFile' = work </> "shelley-genesis.json"
  rewriteJsonObject shelleyGenesisFile' $
    KeyMap.insert "slotsPerKESPeriod" (Aeson.toJSON slotsPerKESPeriod)

  -- Persistence on. A non-empty TraceOptions replaces the node's default
  -- trace configuration rather than extending it, so every namespace the
  -- tests read is listed at Info: Startup (BlockForgingUpdate on SIGHUP),
  -- Forge.Loop (forged blocks, forge state errors), Forge.StateInfo (the
  -- KES evolution) and Forge.KESPersist.
  let configurationFile' = work </> "configuration.yaml"
      info = Aeson.object [ "severity" Aeson..= ("Info" :: String) ]
      traceOptions = Aeson.object
        [ "Startup" Aeson..= info
        , "Forge.Loop" Aeson..= info
        , "Forge.StateInfo" Aeson..= info
        , "Forge.KESPersist" Aeson..= info
        ]
  rewriteJsonObject configurationFile' $
    KeyMap.insert "PersistEvolvedKESKey" (Aeson.Bool True) . KeyMap.insert "TraceOptions" traceOptions

  -- The node starts from the evolution-zero file cardano-cli wrote (checked
  -- before the start: the first evolution replaces it within seconds).
  (ty0, _, _) <- readKeyFile (spoKesKeyFile work)
  ty0 === KESKeyEvolutionZero

  let runtimeOptions = def
        { runtimeKESSource = UseKesKeyFile
        , runtimeEnableChainStallWatchdog = False
        }
  runtime <- liftToIntegration $ cardanoTestnet (creationNodes creationOptions) runtimeOptions conf
  spo <- case spoNodes runtime of
    [spo] -> pure spo
    spos -> do
      H.note_ $ "Expected exactly one SPO node, found: " <> show (nodeName <$> spos)
      H.failure
  H.note_ (nodeStdout spo)
  pure (runtime, spo, work)

rewriteJsonObject :: (HasCallStack, MonadIO m, H.MonadTest m) => FilePath -> (Aeson.Object -> Aeson.Object) -> m ()
rewriteJsonObject path f = withFrozenCallStack $ do
  value <- H.evalIO (Aeson.eitherDecodeFileStrict' path) >>= H.evalEither
  obj <- case value of
    Aeson.Object o -> pure o
    _ -> H.failure
  H.lbsWriteFile path $ Aeson.encodePretty $ Aeson.Object (f obj)

-- | The SPO's KES signing key file.
spoKesKeyFile :: FilePath -> FilePath
spoKesKeyFile work = work </> defaultSpoKesSKeyFp 1

-- | Stop the SPO (SIGTERM, then wait for it to exit).
stopNode :: (HasCallStack, MonadIO m, H.MonadTest m) => TestnetNode -> m ()
stopNode node = withFrozenCallStack $ H.evalIO $ do
  terminateProcess (nodeProcessHandle node)
  void $ waitForProcess (nodeProcessHandle node)

-- | Start the SPO again with the arguments 'cardanoTestnet' gave it. Its
-- stdout log starts afresh.
restartSpo :: HasCallStack => TestnetRuntime -> FilePath -> TestnetNode -> H.Integration TestnetNode
restartSpo runtime work spo = withFrozenCallStack $ do
  let name = defaultNodeName 1
      args =
        [ "run"
        , "--config", unFile (configurationFile runtime)
        , "--topology", work </> defaultNodeDataDir 1 </> "topology.json"
        , "--database-path", work </> defaultNodeDataDir 1 </> "db"
        , "--shelley-kes-key", spoKesKeyFile work
        , "--shelley-vrf-key", work </> defaultSpoVrfSKeyFp 1
        , "--shelley-operational-certificate", work </> defaultSpoOpcertCertFp 1
        , "--byron-delegation-certificate", work </> defaultSpoByronDelegationCertFp 1
        , "--byron-signing-key", work </> defaultSpoByronDelegateKeyFp 1
        ]
  result <- liftToIntegration $ runExceptT $
    startNode (TmpAbsolutePath work) name (nodeIpv4 spo) (nodePort spo) (testnetMagic runtime) Nothing args
  case result of
    Left err -> do
      H.note_ ("restarting the SPO failed: " <> show err)
      H.failure
    Right node -> pure node { poolKeys = poolKeys spo }

-- * Reading the log

-- | The @data@ objects of the log lines traced under the given namespace.
logEntries :: MonadIO m => FilePath -> Text -> m [Aeson.Object]
logEntries logFile namespace = liftIO $ do
  contents <- BS.readFile logFile
  pure $ mapMaybe entry (BS8.lines contents)
  where
    entry line = do
      Aeson.Object o <- Aeson.decodeStrict' line
      Aeson.String ns <- KeyMap.lookup "ns" o
      Aeson.Object d <- KeyMap.lookup "data" o
      if ns == namespace then Just d else Nothing

field :: Aeson.FromJSON a => Text -> Aeson.Object -> Maybe a
field key obj = do
  v <- KeyMap.lookup (Key.fromText key) obj
  case Aeson.fromJSON v of
    Aeson.Success a -> Just a
    Aeson.Error _ -> Nothing

-- | The periods of the @Forge.KESPersist.Persisted@ traces, in order.
persistedPeriods :: MonadIO m => FilePath -> m [Word64]
persistedPeriods logFile = mapMaybe (field "period") <$> logEntries logFile "Forge.KESPersist.Persisted"

-- | The number of blocks the node reports having forged.
forgedBlocks :: MonadIO m => FilePath -> m Int
forgedBlocks logFile = length <$> logEntries logFile "Forge.Loop.ForgedBlock"

-- | The KES evolutions reported by @Forge.StateInfo@ (endPeriod is the
-- current absolute period in that trace, so the evolution is its distance
-- from startPeriod), with the start period.
reportedEvolutions :: MonadIO m => FilePath -> m [(Word64, Word64)]
reportedEvolutions logFile =
  mapMaybe evolution <$> logEntries logFile "Forge.StateInfo.StateInfo"
  where
    evolution o = do
      start <- field "startPeriod" o
      current <- field "endPeriod" o
      pure (start, current - start)

-- | The reasons of the traces under a namespace (@reason@ field).
reasons :: MonadIO m => FilePath -> Text -> m [Text]
reasons logFile namespace = mapMaybe (field "reason") <$> logEntries logFile namespace

-- | Wait (up to the given number of seconds) until the log satisfies a
-- condition.
waitForLog :: (HasCallStack, MonadIO m, H.MonadTest m, H.MonadAssertion m) => Int -> String -> FilePath -> (FilePath -> IO Bool) -> m ()
waitForLog seconds what logFile cond = withFrozenCallStack $
  H.byDurationM 0.5 (fromIntegral seconds) what $ do
    ok <- H.evalIO (cond logFile)
    H.assert ok

-- | Wait for the next persisted period above the given one and return it.
waitForPersistAfter :: (HasCallStack, MonadIO m, H.MonadTest m, H.MonadAssertion m) => FilePath -> Word64 -> m Word64
waitForPersistAfter logFile period = withFrozenCallStack $ do
  waitForLog 40 ("Expected a KESPersisted trace after period " <> show period) logFile $
    fmap (any (> period)) . persistedPeriods
  maximum <$> persistedPeriods logFile

-- | Forging continues: at least two more blocks within the given time.
assertStillForging :: (HasCallStack, MonadIO m, H.MonadTest m, H.MonadAssertion m) => FilePath -> m ()
assertStillForging logFile = withFrozenCallStack $ do
  before <- forgedBlocks logFile
  waitForLog 40 ("Expected the node to forge more than " <> show before <> " blocks") logFile $
    fmap (>= before + 2) . forgedBlocks

-- * The key file

-- | Envelope type, period and raw key bytes of a KES signing key file.
readKeyFile :: (HasCallStack, MonadIO m, H.MonadTest m) => FilePath -> m (KESEnvelopeType, Maybe Word64, BS.ByteString)
readKeyFile path = withFrozenCallStack $ do
  contents <- H.evalIO $ BS.readFile path
  result <- H.evalIO $ BS.unsafeUseAsCStringLen contents $ \(ptr, len) ->
    readLockedKESEnvelope algorithmName (castPtr ptr) len
  envelope <- H.evalEither result
  key <- H.evalIO $ withMLockedForeignPtr (lkeKey envelope) $ \ptr ->
    BS.packCStringLen (ptr, lkeKeyLen envelope)
  H.evalIO $ finalizeLockedKESEnvelope envelope
  pure (lkeType envelope, lkePeriod envelope, key)

-- | Write a KES signing key file in the period-carrying envelope.
writeKeyFileWithPeriod :: MonadIO m => FilePath -> Word64 -> BS.ByteString -> m ()
writeKeyFileWithPeriod path period key = liftIO $
  BS.unsafeUseAsCStringLen key $ \(ptr, len) ->
    writeLockedKESEnvelope algorithmName (Just period) ptr len $ \json jsonLen ->
      BS.packCStringLen (castPtr json, jsonLen) >>= BS.writeFile path

algorithmName :: String
algorithmName = algorithmNameKES (Proxy @(KES StandardCrypto))

-- * Tests

-- | The file's period advances with every evolution, and a restart from the
-- persisted file resumes at the persisted evolution (or the clock's, when
-- the clock has moved on), with the old envelope type accepted at the first
-- start ('startPersistingTestnet' checks the file is of that type) and the
-- new one at every restart.
hprop_persist_evolved_kes_key_across_evolutions_and_restarts :: Property
hprop_persist_evolved_kes_key_across_evolutions_and_restarts =
  integrationRetryWorkspace 2 "kes-persist-restarts" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
    (runtime, spo0, work) <- startPersistingTestnet tempAbsBasePath'
    let keyFile = spoKesKeyFile work

    -- The file follows the evolutions.
    p1 <- waitForPersistAfter (nodeStdout spo0) 0
    (ty1, period1, _) <- readKeyFile keyFile
    ty1 === KESKeyWithPeriod
    period1 === Just p1
    p2 <- waitForPersistAfter (nodeStdout spo0) p1
    (_, period2, _) <- readKeyFile keyFile
    period2 === Just p2
    H.assert (p2 > p1)
    assertStillForging (nodeStdout spo0)

    -- Three restarts: right after a persist (so the restart straddles no
    -- boundary), then at whatever point the next ones land.
    let restartAndCheck spo expectAtLeast = do
          stopNode spo
          (_, persisted, _) <- readKeyFile keyFile
          H.note_ $ "Restarting from the file at period " <> show persisted
          spo' <- restartSpo runtime work spo
          let logFile = nodeStdout spo'
          -- The hot key is built at the persisted evolution and evolved to
          -- the clock's period on the first tick, never backwards.
          waitForLog 40 "Expected a Forge.StateInfo trace after the restart" logFile $
            fmap (not . null) . reportedEvolutions
          evolutions <- reportedEvolutions logFile
          (start, firstEvolution) <- case evolutions of
            e : _ -> pure e
            [] -> H.failure
          H.note_ $ "First reported evolution after the restart: " <> show firstEvolution
          persistedEvolution <- case persisted of
            Just p -> pure (p - start)
            Nothing -> H.failure
          H.assert (firstEvolution >= persistedEvolution)
          H.assert (firstEvolution >= expectAtLeast)
          -- No load error, forging and persisting go on.
          logLines <- lines <$> H.readFile logFile
          H.assert . not $ any ("KesKeyFileLoadError" `isInfixOf`) logLines
          H.assert . not $ any ("BlockForgingUpdateError" `isInfixOf`) logLines
          void $ waitForPersistAfter logFile (fromMaybe 0 persisted)
          assertStillForging logFile
          pure spo'
    spo1 <- restartAndCheck spo0 2
    spo2 <- restartAndCheck spo1 3
    spo3 <- restartAndCheck spo2 4
    stopNode spo3

-- | A key file whose period is ahead of the clock (a persisted file carried
-- to a machine whose clock is behind, or a key evolved by hand) loads, but
-- the node refuses to sign with it: KESKeyFromTheFuture, no blocks.
hprop_kes_key_file_ahead_of_clock_stops_forging :: Property
hprop_kes_key_file_ahead_of_clock_stops_forging =
  integrationRetryWorkspace 2 "kes-key-ahead-of-clock" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
    (runtime, spo0, work) <- startPersistingTestnet tempAbsBasePath'
    let keyFile = spoKesKeyFile work
    p <- waitForPersistAfter (nodeStdout spo0) 0
    evolutions <- reportedEvolutions (nodeStdout spo0)
    start <- case evolutions of
      (s, _) : _ -> pure s
      [] -> H.failure
    stopNode spo0

    -- Evolve the persisted key ten periods further and write it back with
    -- that period.
    (_, period, key) <- readKeyFile keyFile
    period === Just p
    KesSigningKey sk <- H.evalEither $ deserialiseFromRawBytes (AsSigningKey AsKesKey) key
    let ahead = 10
        evolve k t = unsoundPureUpdateKES () k (fromIntegral t)
        stepped = foldl' (\mk t -> mk >>= \k -> evolve k t) (Just sk) [p - start .. p - start + ahead - 1]
    sk' <- maybe H.failure pure stepped
    writeKeyFileWithPeriod keyFile (p + ahead) (serialiseToRawBytes (KesSigningKey sk'))
    (ty', period', _) <- readKeyFile keyFile
    ty' === KESKeyWithPeriod
    period' === Just (p + ahead)

    spo1 <- restartSpo runtime work spo0
    let logFile = nodeStdout spo1
    waitForLog 40 "Expected a KESKeyFromTheFuture forge state update error" logFile $
      fmap (any ("KESKeyFromTheFuture" `isInfixOf`) . lines) . readFile
    forged <- forgedBlocks logFile
    H.threadDelay 10_000_000
    forgedLater <- forgedBlocks logFile
    H.note_ $ "Blocks forged with the key from the future: " <> show forged <> ", 10 s later: " <> show forgedLater
    forgedLater === forged
    -- The file is left alone: nothing was persisted.
    persisted <- persistedPeriods logFile
    persisted === []
    (_, periodAfter, _) <- readKeyFile keyFile
    periodAfter === Just (p + ahead)
    stopNode spo1

-- | Every failure injection produces its warning trace and leaves forging
-- untouched: SIGHUP reloads racing the writer, an unwritable key directory,
-- a foreign key dropped into the path, a hard link to the key file, and a
-- symbolic link at the key path.
hprop_kes_persist_failure_injections_keep_forging :: Property
hprop_kes_persist_failure_injections_keep_forging =
  integrationRetryWorkspace 2 "kes-persist-failure-injections" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
    (runtime, spo0, work) <- startPersistingTestnet tempAbsBasePath'
    let keyFile = spoKesKeyFile work
        keyDir = takeDirectory keyFile
        logFile = nodeStdout spo0
    p0 <- waitForPersistAfter logFile 0

    -- SIGHUP right after each of three persists: the reload re-reads the
    -- file the writer just replaced (or is replacing) and installs the hook
    -- again. No reload error, forging goes on.
    pHup <- foldM' p0 [1 .. 3 :: Int] $ \p _ -> do
      H.evalIO $ reloadNodeConfiguration (nodeProcessHandle spo0)
      waitForPersistAfter logFile p
    logLines <- lines <$> H.readFile logFile
    H.assert . not $ any ("BlockForgingUpdateError" `isInfixOf`) logLines
    length (filter ("EnabledBlockForging" `isInfixOf`) logLines) === 4
    assertStillForging logFile

    -- Unwritable key directory: the write fails, the file keeps the last
    -- evolution, forging goes on. Then writable again: persisting resumes.
    (_, beforeReadOnly, _) <- readKeyFile keyFile
    H.evalIO $ setFileMode keyDir 0o500
    waitForLog 40 "Expected a KESPersistFailed trace" logFile $ \lf ->
      not . null <$> logEntries lf "Forge.KESPersist.Failed"
    H.evalIO $ setFileMode keyDir 0o700
    (_, afterReadOnly, _) <- readKeyFile keyFile
    afterReadOnly === beforeReadOnly
    assertStillForging logFile
    pFailed <- maximum <$> persistedPeriods logFile
    pResumed <- waitForPersistAfter logFile (max pHup pFailed)

    -- A foreign key dropped into the path is never overwritten; the real
    -- key put back from an older copy (taken one persist earlier) is
    -- overwritten again, with the backwards step traced.
    let savedKeyFile = work </> "kes.skey.saved"
    H.evalIO $ copyFile keyFile savedKeyFile
    (_, savedPeriod, _) <- readKeyFile savedKeyFile
    pResumed' <- waitForPersistAfter logFile pResumed
    foreignKey <- H.evalIO $ generateSigningKey AsKesKey
    H.evalIO (writeFileTextEnvelope (File keyFile) Nothing foreignKey) >>= H.evalEither
    waitForLog 40 "Expected a KESPersistSkipped trace for the foreign key" logFile $ \lf ->
      any ("different key" `Text.isInfixOf`) <$> reasons lf "Forge.KESPersist.Skipped"
    (tyForeign, _, _) <- readKeyFile keyFile
    tyForeign === KESKeyEvolutionZero
    assertStillForging logFile
    H.evalIO $ renameFile savedKeyFile keyFile
    pRestored <- waitForPersistAfter logFile pResumed'
    backwards <- logEntries logFile "Forge.KESPersist.PeriodWentBackwards"
    H.note_ $ "PeriodWentBackwards traces: " <> show backwards
    H.assert (not (null backwards))
    (_, periodRestored, _) <- readKeyFile keyFile
    periodRestored === Just pRestored
    H.assert (Just pRestored > savedPeriod)

    -- A hard link to the key file: the file is replaced, the link keeps the
    -- old contents, and the old inode is not zeroed.
    let linkFile = work </> "kes.skey.link"
    H.evalIO $ createLink keyFile linkFile
    linkedBefore <- H.evalIO $ BS.readFile linkFile
    pLinked <- waitForPersistAfter logFile pRestored
    waitForLog 40 "Expected a KESOrphanNotZeroed trace" logFile $ \lf ->
      not . null <$> logEntries lf "Forge.KESPersist.OrphanNotZeroed"
    H.evalIO (BS.readFile linkFile) >>= (=== linkedBefore)
    (_, periodLinked, _) <- readKeyFile keyFile
    periodLinked === Just pLinked
    H.evalIO $ removeFile linkFile
    assertStillForging logFile

    -- A symbolic link at the key path (after a restart, since the path is
    -- fixed at start-up): the key loads through the link, nothing is ever
    -- written through it, forging goes on.
    stopNode spo0
    (_, periodBeforeSymlink, _) <- readKeyFile keyFile
    let realKeyFile = keyDir </> "real.skey"
    H.evalIO $ renameFile keyFile realKeyFile
    H.evalIO $ createFileLink realKeyFile keyFile
    spo1 <- restartSpo runtime work spo0
    let logFile1 = nodeStdout spo1
    waitForLog 40 "Expected a KESPersistSkipped trace for the symbolic link" logFile1 $ \lf ->
      any ("symbolic link" `Text.isInfixOf`) <$> reasons lf "Forge.KESPersist.Skipped"
    (_, periodReal, _) <- readKeyFile realKeyFile
    periodReal === periodBeforeSymlink
    assertStillForging logFile1
    stopNode spo1
  where
    foldM' z xs f = go z xs
      where
        go acc [] = pure acc
        go acc (x : rest) = f acc x >>= \acc' -> go acc' rest

#ifndef UNIX
createLink :: FilePath -> FilePath -> IO ()
createLink _ _ = fail "hard links are not supported on this platform"

setFileMode :: FilePath -> Int -> IO ()
setFileMode _ _ = fail "file modes are not supported on this platform"
#endif
