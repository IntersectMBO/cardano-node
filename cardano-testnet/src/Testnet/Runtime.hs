{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Testnet.Runtime
  ( startNode
  , startLedgerNewEpochStateLogging
  , NodeStartFailure (..)
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.List (isInfixOf)
import qualified Data.List as List
import           GHC.Stack
import qualified GHC.Stack as GHC
import           Network.Socket (HostAddress, PortNumber)
import           Prettyprinter (unAnnotate)
import qualified System.Directory as IO
import           System.FilePath
import qualified System.IO as IO
import qualified System.Process as IO

import           Testnet.Filepath
import qualified Testnet.Ping as Ping
import           Testnet.Process.Run
import           Testnet.Types (NodeRuntime (NodeRuntime), TestnetRuntime (configurationFile),
                   poolSprockets, showIpv4Address)

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H

data NodeStartFailure
  = ProcessRelatedFailure ProcessError
  | ExecutableRelatedFailure ExecutableError
  | FileRelatedFailure IOException
  | NodeExecutableError (Doc Ann)
  | NodeAddressAlreadyInUseError (Doc Ann)
 -- | NodePortNotOpenError IOException
  | MaxSprocketLengthExceededError
  deriving Show

-- | Analyze @stderr@ contents and return the appropriate error. If the node didn't start because the address
-- was already in use, 'NodeAddressAlreadyInUse' is returned.
mkNodeNonEmptyStderrError
  :: String -- ^ @stderr@ contents
  -> NodeStartFailure
mkNodeNonEmptyStderrError stderr' = do
  if "Address already in use" `isInfixOf` stderr'
    then NodeAddressAlreadyInUseError $ pretty stderr'
    else NodeExecutableError $ pretty stderr'

instance Error NodeStartFailure where
  prettyError = \case
    ProcessRelatedFailure e -> "Cannot initiate process:" <+> pshow e
    ExecutableRelatedFailure e -> "Cannot run cardano-node executable" <+> pshow e
    FileRelatedFailure e -> "File error:" <+> prettyException e
    NodeExecutableError e -> "Cardano node process did not start:" <+> unAnnotate e
    NodeAddressAlreadyInUseError e -> "Cardano node process did not start - address already in use:" <+> unAnnotate e
    MaxSprocketLengthExceededError -> "Max sprocket length exceeded"

-- TODO: We probably want a check that this node has the necessary config files to run and
-- if it doesn't we fail hard.
-- | Start a node, creating file handles, sockets and temp-dirs.
--
-- If the port in the function argument was obtained using 'H.randomPort' which binds to the port first and then
-- closes it, on some operating systems, like MacOS, the port can get stuck in TIME_WAIT state for a
-- significant period. Unfortunately there is no Haskell API giving the ability to check that - this
-- means that the user of this function needs to retry on 'NodeAddressAlreadyInUseError' until this
-- function succeeds.
-- (see state diagram in https://www.rfc-editor.org/rfc/rfc793#section-3.2 p. 23.)
startNode
  :: HasCallStack
  => MonadResource m
  => MonadCatch m
  => MonadFail m
  => MonadTest m
  => TmpAbsolutePath
  -- ^ The temporary absolute path
  -> String
  -- ^ The name of the node
  -> HostAddress
  -- ^ Node IPv4 address
  -> PortNumber
  -- ^ Node port
  -> Int
  -- ^ Testnet magic
  -> [String]
  -- ^ The command --socket-path will be added automatically.
  -> ExceptT NodeStartFailure m NodeRuntime
startNode tp node ipv4 port testnetMagic nodeCmd = GHC.withFrozenCallStack $ do
  let tempBaseAbsPath = makeTmpBaseAbsPath tp
      socketDir = makeSocketDir tp
      logDir = makeLogDir tp

  liftIO $ createDirectoryIfMissingNew_ $ logDir </> node
  void . liftIO $ createSubdirectoryIfMissingNew tempBaseAbsPath (socketDir </> node)

  let nodeStdoutFile = logDir </> node </> "stdout.log"
      nodeStderrFile = logDir </> node </> "stderr.log"
      socketRelPath = socketDir </> node </> "sock"
      sprocket = Sprocket tempBaseAbsPath socketRelPath

  hNodeStdout <- retryOpenFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- retryOpenFile nodeStderrFile IO.ReadWriteMode

  -- Sometimes the handles are not getting properly closed when node fails to start. This results in
  -- operating system holding the file lock for longer than it's necessary. This in the end prevents retrying
  -- node start and acquiring a lock for the same stderr/stdout files again.
  closeHandlesOnError [hNodeStdout, hNodeStderr] $ do

    unless (List.length (H.sprocketArgumentName sprocket) <= H.maxSprocketArgumentNameLength) $
       left MaxSprocketLengthExceededError

    let socketAbsPath = H.sprocketSystemName sprocket

    nodeProcess
      <- firstExceptT ExecutableRelatedFailure
           $ hoistExceptT liftIO $ procNode $ mconcat
                         [ nodeCmd
                         , [ "--socket-path", H.sprocketArgumentName sprocket
                           , "--port", show port
                           , "--host-addr", showIpv4Address ipv4
                           ]
                         ]

    -- The port number if it is obtained using 'H.randomPort', it is firstly bound to and then closed. The closing
    -- and release in the operating system is done asynchronously and can be slow. Here we wait until the port
    -- is out of CLOSING state.
    H.note_ $ "Waiting for port " <> show port <> " to be available before starting node"
    H.assertM $ Ping.waitForPortClosed 30 0.1 port

    (Just stdIn, _, _, hProcess, _)
      <- firstExceptT ProcessRelatedFailure $ initiateProcess
            $ nodeProcess
               { IO.std_in = IO.CreatePipe, IO.std_out = IO.UseHandle hNodeStdout
               , IO.std_err = IO.UseHandle hNodeStderr
               , IO.cwd = Just tempBaseAbsPath
               }

    -- We force the evaluation of initiateProcess so we can be sure that
    -- the process has started. This allows us to read stderr in order
    -- to fail early on errors generated from the cardano-node binary.
    _ <- liftIO (IO.getPid hProcess)
      >>= hoistMaybe (NodeExecutableError $ "startNode:" <+> pretty node <+> "'s process did not start.")

    -- Wait for socket to be created
    eSprocketError <-
      Ping.waitForSprocket
        120  -- timeout
        0.2 -- check interval
        sprocket

    -- If we do have anything on stderr, fail.
    stdErrContents <- liftIO $ IO.readFile nodeStderrFile
    unless (null stdErrContents) $
      throwError $ mkNodeNonEmptyStderrError stdErrContents

    -- No stderr and no socket? Fail.
    firstExceptT
      (\ioex ->
        NodeExecutableError . hsep $
          ["Socket", pretty socketAbsPath, "was not created after 120 seconds. There was no output on stderr. Exception:", prettyException ioex])
      $ hoistEither eSprocketError
      
    -- Ping node and fail on error
    Ping.pingNode (fromIntegral testnetMagic) sprocket
       >>= (firstExceptT (NodeExecutableError . ("Ping error:" <+>) . prettyError) . hoistEither)

    pure $ NodeRuntime node ipv4 port sprocket stdIn nodeStdoutFile nodeStderrFile hProcess
  where
    -- close provided list of handles when 'ExceptT' throws an error
    closeHandlesOnError :: MonadIO m => [IO.Handle] -> ExceptT e m a -> ExceptT e m a
    closeHandlesOnError handles action =
      catchE action $ \e -> do
        liftIO $ mapM_ IO.hClose handles
        throwE e

    -- Sometimes even when we close the files manually, the operating system still holds the lock for some
    -- reason. This is most prominent on MacOS. Therefore, as a last resort, instead of
    -- failing the node startup procedure, we simply try to use a different file name for the logs, with
    -- the suffix @-n.log@ where @n@ is an attempt number.
    retryOpenFile :: MonadIO m
                  => MonadCatch m
                  => FilePath -- ^ path we're trying to open
                  -> IO.IOMode
                  -> ExceptT NodeStartFailure m IO.Handle
    retryOpenFile fullPath mode = go 0
      where
        go :: MonadIO m
           => MonadCatch m
           => Int
           -> ExceptT NodeStartFailure m IO.Handle
        go n = do
          let (path, extension) = splitExtension fullPath
              path' = if n > 0
                         then path <> "-" <> show n <> extension
                         else fullPath
          r <- fmap (first FileRelatedFailure) . try . liftIO $ IO.openFile path' mode
          case r of
            Right h -> pure h
            Left e
              -- give up after 1000 attempts
              | n >= 1000 -> throwE e
              | otherwise -> go (n + 1)



createDirectoryIfMissingNew :: HasCallStack => FilePath -> IO FilePath
createDirectoryIfMissingNew directory = GHC.withFrozenCallStack $ do
  IO.createDirectoryIfMissing True directory
  pure directory

createDirectoryIfMissingNew_ :: HasCallStack => FilePath -> IO ()
createDirectoryIfMissingNew_ directory = GHC.withFrozenCallStack $
  void $ createDirectoryIfMissingNew directory

createSubdirectoryIfMissingNew :: ()
  => HasCallStack
  => FilePath
  -> FilePath
  -> IO FilePath
createSubdirectoryIfMissingNew parent subdirectory = GHC.withFrozenCallStack $ do
  IO.createDirectoryIfMissing True $ parent </> subdirectory
  pure subdirectory

-- | Start ledger's new epoch state logging for the first node in the background.
-- Pretty JSON logs will be placed in:
-- 1. <tmp workspace directory>/logs/ledger-new-epoch-state.log
-- 2. <tmp workspace directory>/logs/ledger-new-epoch-state-diffs.log
-- NB: The diffs represent the the changes in the 'NewEpochState' between each
-- block or turn of the epoch. We have excluded the 'stashedAVVMAddresses'
-- field of 'NewEpochState' in the JSON rendering.
-- The logging thread will be cancelled when `MonadResource` releases all resources.
-- Idempotent.
startLedgerNewEpochStateLogging
  :: HasCallStack
  => MonadCatch m
  => MonadResource m
  => MonadTest m
  => TestnetRuntime
  -> FilePath -- ^ tmp workspace directory
  -> m ()
startLedgerNewEpochStateLogging testnetRuntime tmpWorkspace = withFrozenCallStack $ do
  let logDir = makeLogDir (TmpAbsolutePath tmpWorkspace)
      -- used as a lock to start only a single instance of epoch state logging
      logFile = logDir </> "ledger-epoch-state.log"
      diffFile = logDir </> "ledger-epoch-state-diffs.log"

  H.evalIO (IO.doesDirectoryExist logDir) >>= \case
    True -> pure ()
    False -> do
      H.note_ $ "Log directory does not exist: " <> logDir <> " - cannot start logging epoch states"
      H.failure

  H.evalIO (IO.doesFileExist logFile) >>= \case
    True -> do
      H.note_ $ "Epoch states logging to " <> logFile <> " is already started."
    False -> do
      H.evalIO $ appendFile logFile ""
      socketPath <- H.noteM $ H.sprocketSystemName <$> H.headM (poolSprockets testnetRuntime)

      _ <- H.asyncRegister_ . runExceptT $
        foldEpochState
          (configurationFile testnetRuntime)
          (Api.File socketPath)
          Api.QuickValidation
          (EpochNo maxBound)
          Nothing
          (handler logFile diffFile)

      H.note_ $ "Started logging epoch states to: " <> logFile <> "\nEpoch state diffs are logged to: " <> diffFile
  where
    handler :: FilePath -- ^ log file
            -> FilePath -- ^ diff file
            -> AnyNewEpochState
            -> SlotNo
            -> BlockNo
            -> StateT (Maybe AnyNewEpochState) IO ConditionResult
    handler outputFp diffFp anes@(AnyNewEpochState !sbe !nes) _ (BlockNo blockNo) = handleException $ do
      let prettyNes = shelleyBasedEraConstraints sbe (encodePretty nes)
          blockLabel = "#### BLOCK " <> show blockNo <> " ####"
      liftIO . BSC.appendFile outputFp $ BSC.unlines [BSC.pack blockLabel, prettyNes, ""]

      -- store epoch state for logging of differences
      mPrevEpochState <- get
      put (Just anes)
      forM_ mPrevEpochState $ \(AnyNewEpochState sbe' pnes) -> do
        let prettyPnes = shelleyBasedEraConstraints sbe' (encodePretty pnes)
            difference = calculateEpochStateDiff prettyPnes prettyNes
        liftIO . appendFile diffFp $ unlines [blockLabel, difference, ""]

      pure ConditionNotMet
      where
        -- | Handle all sync exceptions and log them into the log file. We don't want to fail the test just
        -- because logging has failed.
        handleException = handle $ \(e :: SomeException) -> do
          liftIO $ appendFile outputFp $ "Ledger new epoch logging failed - caught exception:\n"
            <> displayException e <> "\n"
          pure ConditionMet

calculateEpochStateDiff
  :: BSC.ByteString -- ^ Current epoch state
  -> BSC.ByteString -- ^ Following epoch state
  -> String
calculateEpochStateDiff current next =
  let diffResult = getGroupedDiff (BSC.unpack <$> BSC.lines current) (BSC.unpack <$> BSC.lines next)
  in if null diffResult
     then "No changes in epoch state"
     else ppDiff diffResult

instance (L.EraTxOut ledgerera, L.EraGov ledgerera) => ToJSON (L.NewEpochState ledgerera) where
  toJSON (L.NewEpochState nesEL nesBprev nesBCur nesEs nesRu nesPd _stashedAvvm) =
    object
      [ "currentEpoch" .= nesEL
      , "priorBlocks" .= nesBprev
      , "currentEpochBlocks" .= nesBCur
      , "currentEpochState" .= nesEs
      , "rewardUpdate" .= nesRu
      , "currentStakeDistribution" .= nesPd
      ]

