{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chairman.Process
  ( createProcess
  , execFlex
  , getProjectBase
  , procCli
  , procNode
  , execCli
  , waitForProcess
  ) where

import           Chairman.Base (Integration)
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ReleaseKey, register)
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Int
import           Data.Maybe (Maybe (..))
import           Data.Monoid
import           Data.Semigroup ((<>))
import           Data.String (String)
import           GHC.Stack (CallStack, HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           System.Exit (ExitCode)
import           System.IO (Handle)
import           System.Process (CmdSpec (..), CreateProcess (..), ProcessHandle)
import           Text.Show

import qualified Data.List as L
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.Process as IO

-- | Format argument for a shell CLI command.
--
-- This includes automatically embedding string in double quotes if necessary, including any necessary escaping.
--
-- Note, this function does not cover all the edge cases for shell processing, so avoid use in production code.
argQuote :: String -> String
argQuote arg = if ' ' `L.elem` arg || '"' `L.elem` arg || '$' `L.elem` arg
  then "\"" <> escape arg <> "\""
  else arg
  where escape :: String -> String
        escape ('"':xs) = '\\':'"':escape xs
        escape ('\\':xs) = '\\':'\\':escape xs
        escape ('\n':xs) = '\\':'n':escape xs
        escape ('\r':xs) = '\\':'r':escape xs
        escape ('\t':xs) = '\\':'t':escape xs
        escape ('$':xs) = '\\':'$':escape xs
        escape (x:xs) = x:escape xs
        escape "" = ""

-- | Create a process returning handles to stdin, stdout, and stderr as well as the process handle.
createProcess :: HasCallStack
  => CreateProcess
  -> Integration (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, ReleaseKey)
createProcess cp = GHC.withFrozenCallStack $ do
  case IO.cmdspec cp of
    RawCommand cmd args -> H.annotate $ "Command line: " <> cmd <> " " <> L.unwords args
    ShellCommand cmd -> H.annotate $ "Command line: " <> cmd
  (mhStdin, mhStdout, mhStderr, hProcess) <- H.evalM . liftIO $ IO.createProcess cp
  releaseKey <- register $ IO.cleanupProcess (mhStdin, mhStdout, mhStderr, hProcess)
  return (mhStdin, mhStdout, mhStderr, hProcess, releaseKey)



-- | Create a process returning its stdout.
--
-- Being a 'flex' function means that the environment determines how the process is launched.
--
-- When running in a nix environment, the 'envBin' argument describes the environment variable
-- that defines the binary to use to launch the process.
--
-- When running outside a nix environment, the `pkgBin` describes the name of the binary
-- to launch via cabal exec.
execFlex :: HasCallStack
  => String
  -> String
  -> [String]
  -> Integration String
execFlex pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  maybeEnvBin <- liftIO $ IO.lookupEnv envBin
  (actualBin, actualArguments) <- case maybeEnvBin of
    Just envBin' -> return (envBin', arguments)
    Nothing -> return ("cabal", ("exec":"--":pkgBin:arguments))
  H.annotate $ "Command: " <> actualBin <> " " <> L.unwords actualArguments
  (exitResult, stdout, stderr) <- H.evalM . liftIO $ IO.readProcessWithExitCode actualBin actualArguments ""
  case exitResult of
    IO.ExitFailure exitCode -> failWithCustom GHC.callStack Nothing . L.unlines $
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

-- | Run cardano-cli, returning the stdout
execCli :: HasCallStack => [String] -> Integration String
execCli = GHC.withFrozenCallStack $ execFlex "cardano-cli" "CARDANO_CLI"

waitForProcess :: HasCallStack
  => ProcessHandle
  -> Integration (Maybe ExitCode)
waitForProcess hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> return Nothing

procFlex
  :: HasCallStack
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> Integration CreateProcess
  -- ^ Captured stdout
procFlex pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
  maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return $ IO.proc envBin arguments
    Nothing -> return $ IO.proc "cabal" ("exec":"--":pkg:arguments)

procCli
  :: HasCallStack
  => [String]
  -- ^ Arguments to the CLI command
  -> Integration CreateProcess
  -- ^ Captured stdout
procCli = procFlex "cardano-cli" "CARDANO_CLI"

procNode
  :: HasCallStack
  => [String]
  -- ^ Arguments to the CLI command
  -> Integration CreateProcess
  -- ^ Captured stdout
procNode = procFlex "cardano-node" "CARDANO_NODE"

getProjectBase :: Integration String
getProjectBase = do
  maybeNodeSrc <- liftIO $ IO.lookupEnv "CARDANO_NODE_SRC"
  case maybeNodeSrc of
    Just path -> return path
    Nothing -> return ".."

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg =
  liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)
