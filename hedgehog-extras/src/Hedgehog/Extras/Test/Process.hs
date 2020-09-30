{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Test.Process
  ( createProcess
  , execFlex
  , procFlex
  , getProjectBase
  , waitForProcess
  , maybeWaitForProcess
  , getPid
  , waitSecondsForProcess
  ) where

import           Control.Monad
import           Control.Monad.Catch hiding (catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (MonadResource, ReleaseKey, register)
import           Data.Aeson (eitherDecode)
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe (Maybe (..))
import           Data.Semigroup ((<>))
import           Data.String (String)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Cli (argQuote)
import           Hedgehog.Extras.Internal.Plan
import           Hedgehog.Extras.Stock.IO.Process (TimedOut (..))
import           Prelude (error)
import           System.Exit (ExitCode)
import           System.FilePath.Posix ((</>))
import           System.IO (Handle)
import           System.Process (CmdSpec (..), CreateProcess (..), Pid, ProcessHandle)
import           Text.Show

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Process as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.IO.Unsafe as IO
import qualified System.Process as IO

planJsonFile :: String
planJsonFile = IO.unsafePerformIO $ do
  maybeBuildDir <- liftIO $ IO.lookupEnv "CABAL_BUILDDIR"
  case maybeBuildDir of
    Just buildDir -> return $ ".." </> buildDir </> "cache/plan.json"
    Nothing -> return "../dist-newstyle/cache/plan.json"
{-# NOINLINE planJsonFile #-}

exeSuffix :: String
exeSuffix = if OS.isWin32 then ".exe" else ""

-- | Create a process returning handles to stdin, stdout, and stderr as well as the process handle.
createProcess
  :: (MonadTest m, MonadResource m, HasCallStack)
  => CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, ReleaseKey)
createProcess cp = GHC.withFrozenCallStack $ do
  H.annotate $ "CWD: " <> show (IO.cwd cp)
  case IO.cmdspec cp of
    RawCommand cmd args -> H.annotate $ "Command line: " <> cmd <> " " <> L.unwords args
    ShellCommand cmd -> H.annotate $ "Command line: " <> cmd
  (mhStdin, mhStdout, mhStderr, hProcess) <- H.evalIO $ IO.createProcess cp
  releaseKey <- register $ IO.cleanupProcess (mhStdin, mhStdout, mhStderr, hProcess)

  return (mhStdin, mhStdout, mhStderr, hProcess, releaseKey)

-- | Get the process ID.
getPid
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ProcessHandle
  -> m (Maybe Pid)
getPid hProcess = GHC.withFrozenCallStack . H.evalIO $ IO.getPid hProcess

-- | Create a process returning its stdout.
--
-- Being a 'flex' function means that the environment determines how the process is launched.
--
-- When running in a nix environment, the 'envBin' argument describes the environment variable
-- that defines the binary to use to launch the process.
--
-- When running outside a nix environment, the `pkgBin` describes the name of the binary
-- to launch via cabal exec.
execFlex
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => String
  -> String
  -> [String]
  -> m String
execFlex pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  cp <- procFlex pkgBin envBin arguments
  H.annotate . ("Command: " <>) $ case IO.cmdspec cp of
    IO.ShellCommand cmd -> cmd
    IO.RawCommand cmd args -> cmd <> " " <> L.unwords args
  (exitResult, stdout, stderr) <- H.evalIO $ IO.readCreateProcessWithExitCode cp ""
  case exitResult of
    IO.ExitFailure exitCode -> H.failMessage GHC.callStack . L.unlines $
      [ "Process exited with non-zero exit-code"
      , "━━━━ command ━━━━"
      , pkgBin <> " " <> L.unwords (fmap argQuote arguments)
      , "━━━━ stdout ━━━━"
      , stdout
      , "━━━━ stderr ━━━━"
      , stderr
      , "━━━━ exit code ━━━━"
      , show @Int exitCode
      ]
    IO.ExitSuccess -> return stdout

-- | Wait for process to exit.
waitForProcess
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ProcessHandle
  -> m ExitCode
waitForProcess hProcess = GHC.withFrozenCallStack $
  H.evalIO $ IO.waitForProcess hProcess

-- | Wait for process to exit or return 'Nothing' if interrupted by an asynchronous exception.
maybeWaitForProcess
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ProcessHandle
  -> m (Maybe ExitCode)
maybeWaitForProcess hProcess = GHC.withFrozenCallStack $
  H.evalIO $ IO.maybeWaitForProcess hProcess

-- | Wait a maximum of 'seconds' secons for process to exit.
waitSecondsForProcess
  :: (MonadTest m, MonadIO m, HasCallStack)
  => Int
  -> ProcessHandle
  -> m (Either TimedOut ExitCode)
waitSecondsForProcess seconds hProcess = GHC.withFrozenCallStack $ do
  result <- H.evalIO $ IO.waitSecondsForProcess seconds hProcess
  case result of
    Left TimedOut -> do
      H.annotate "Timed out waiting for process to exit"
      return (Left TimedOut)
    Right maybeExitCode -> do
      case maybeExitCode of
        Nothing -> H.failMessage GHC.callStack "No exit code for process"
        Just exitCode -> do
          H.annotate $ "Process exited " <> show exitCode
          return (Right exitCode)

-- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
-- corresponding to the executable and an argument list.
--
-- The actual executable will be found by consulting the "plan.json" generated by cabal.  It
-- is assumed that the proused ject has already been configured and the executable has been
-- built.
procDist
  :: (MonadTest m, MonadIO m)
  => String
  -- ^ Package name
  -> [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procDist pkg arguments = do
  contents <- H.evalIO . LBS.readFile $ planJsonFile

  case eitherDecode contents of
    Right plan -> case L.filter matching (plan & installPlan) of
      (component:_) -> case component & binFile of
        Just bin -> return $ IO.proc (T.unpack bin <> exeSuffix) arguments
        Nothing -> error $ "missing bin-file in: " <> show component
      [] -> error $ "Cannot find exe:" <> pkg <> " in plan"
    Left message -> error $ "Cannot decode plan: " <> message
  where matching :: Component -> Bool
        matching component = case componentName component of
          Just name -> name == "exe:" <> T.pack pkg
          Nothing -> False

-- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
-- corresponding to the executable, an environment variable pointing to the executable,
-- and an argument list.
--
-- The actual executable used will the one specified by the environment variable, but if
-- the environment variable is not defined, it will be found instead by consulting the
-- "plan.json" generated by cabal.  It is assumed that the project has already been
-- configured and the executable has been built.
procFlex
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procFlex pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
  maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return $ IO.proc envBin arguments
    Nothing -> procDist pkg arguments

-- | Compute the project base.  This will be based on either the "CARDANO_NODE_SRC"
-- environment variable or the parent directory.  Both should point to the
-- root directory of the Github project checkout.
getProjectBase
  :: (MonadTest m, MonadIO m)
  => m String
getProjectBase = do
  maybeNodeSrc <- liftIO $ IO.lookupEnv "CARDANO_NODE_SRC"
  case maybeNodeSrc of
    Just path -> return path
    Nothing -> do
      atBase <- liftIO $ IO.doesFileExist "cabal.project"
      if atBase
        then return "."
        else return ".."
