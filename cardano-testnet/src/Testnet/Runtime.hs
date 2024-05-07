{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Runtime
  ( startNode
  , startLedgerNewEpochStateLogging
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import           Prelude

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Resource
import qualified Data.List as List
import           Data.Text (Text, unpack)
import           GHC.Stack
import qualified GHC.Stack as GHC
import           Network.Socket (PortNumber)
import           Prettyprinter (unAnnotate)
import qualified System.Directory as IO
import           System.FilePath
import qualified System.IO as IO
import qualified System.Process as IO

import           Testnet.Filepath
import qualified Testnet.Ping as Ping
import           Testnet.Process.Run
import           Testnet.Property.Util (runInBackground)
import           Testnet.Types hiding (testnetMagic)

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H
import qualified Hedgehog.Extras.Test.Base as H

data NodeStartFailure
  = ProcessRelatedFailure ProcessError
  | ExecutableRelatedFailure ExecutableError
  | FileRelatedFailure IOException
  | NodeExecutableError (Doc Ann)
 -- | NodePortNotOpenError IOException
  | MaxSprocketLengthExceededError
  deriving Show

instance Error NodeStartFailure where
  prettyError = \case
    ProcessRelatedFailure e -> "Cannot initiate process:" <+> pshow e
    ExecutableRelatedFailure e -> "Cannot run cardano-node executable" <+> pshow e
    FileRelatedFailure e -> "File error:" <+> prettyException e
    NodeExecutableError e -> "Cardano node process did not start:" <+> unAnnotate e
    MaxSprocketLengthExceededError -> "Max sprocket length exceeded"

-- TODO: We probably want a check that this node has the necessary config files to run and
-- if it doesn't we fail hard.
-- | Start a node, creating file handles, sockets and temp-dirs.
startNode
  :: HasCallStack
  => MonadResource m
  => MonadCatch m
  => MonadFail m
  => TmpAbsolutePath
  -- ^ The temporary absolute path
  -> String
  -- ^ The name of the node
  -> Text
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

  hNodeStdout <- handleIOExceptionsWith FileRelatedFailure . liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- handleIOExceptionsWith FileRelatedFailure . liftIO $ IO.openFile nodeStderrFile IO.ReadWriteMode

  unless (List.length (H.sprocketArgumentName sprocket) <= H.maxSprocketArgumentNameLength) $
     left MaxSprocketLengthExceededError

  let socketAbsPath = H.sprocketSystemName sprocket

  nodeProcess
    <- firstExceptT ExecutableRelatedFailure
         $ hoistExceptT liftIO $ procNode $ mconcat
                       [ nodeCmd
                       , [ "--socket-path", H.sprocketArgumentName sprocket
                         , "--port", show port
                         , "--host-addr", unpack ipv4
                         ]
                       ]

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
      30  -- timeout
      0.2 -- check interval
      sprocket

  -- If we do have anything on stderr, fail.
  stdErrContents <- liftIO $ IO.readFile nodeStderrFile
  unless (null stdErrContents)
    $ left . NodeExecutableError $ pretty stdErrContents

  -- No stderr and no socket? Fail.
  firstExceptT
    (\ioex ->
      NodeExecutableError . hsep $
        ["Socket", pretty socketAbsPath, "was not created after 30 seconds. There was no output on stderr. Exception:", prettyException ioex])
    $ hoistEither eSprocketError

  -- Ping node and fail on error
  Ping.pingNode (fromIntegral testnetMagic) sprocket
    >>= (firstExceptT (NodeExecutableError . ("Ping error:" <+>) . prettyError) . hoistEither)

  pure $ NodeRuntime node ipv4 port sprocket stdIn nodeStdoutFile nodeStderrFile hProcess

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
-- Logs will be placed in <tmp workspace directory>/logs/ledger-new-epoch-state.log
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
      _ <- runInBackground . runExceptT $
        foldEpochState
          (configurationFile testnetRuntime)
          (Api.File socketPath)
          Api.QuickValidation
          (EpochNo maxBound)
          ()
          (\epochState _ _ -> handler logFile epochState)
      H.note_ $ "Started logging epoch states to: " <> logFile
  where
    handler :: FilePath -> AnyNewEpochState -> StateT () IO LedgerStateCondition
    handler outputFp anyNewEpochState = handleException . liftIO $ do
      appendFile outputFp $ "#### BLOCK ####" <> "\n"
      appendFile outputFp $ show anyNewEpochState <> "\n"
      pure ConditionNotMet
      where
        -- | Handle all sync exceptions and log them into the log file. We don't want to fail the test just
        -- because logging has failed.
        handleException = handle $ \(e :: SomeException) -> do
          liftIO $ appendFile outputFp $ "Ledger new epoch logging failed - caught exception:\n"
            <> displayException e <> "\n"
          pure ConditionMet


