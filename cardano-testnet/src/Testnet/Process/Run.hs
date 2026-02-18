{-# LANGUAGE RankNTypes #-}

module Testnet.Process.Run
  ( bashPath
  , execCli
  , execCli_
  , execCli'
  , execCliAny
  , execCreateScriptContext
  , execCreateScriptContext'
  , execCliStdoutToJson
  , initiateProcess
  , procCli
  , procNode
  , procSubmitApi
  , procChairman
  , mkExecConfig
  , mkExecConfigOffline
  , ProcessError(..)
  , addEnvVarsToConfig
  ) where

import           Prelude

import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import           Data.Maybe
import           Data.Monoid (Last (..))
import           Data.String (fromString)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import qualified System.Environment as IO
import           System.Exit (ExitCode)
import           System.IO
import qualified System.IO.Unsafe as IO
import qualified System.Process as IO
import           System.Process

import           Testnet.Process.RunIO (liftIOAnnotated)

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import           Hedgehog.Extras.Test.Process (ExecConfig)
import qualified Hedgehog.Internal.Property as H

-- | Path to the bash executable.  This is used on Windows so that the caller can supply a Windows
-- path to the bash executable because there is no reliable way to invoke bash without the full
-- Windows path from Haskell.
bashPath :: FilePath
bashPath = IO.unsafePerformIO $ do
  mValue <- IO.lookupEnv "BASH_PATH"
  case mValue of
    Just "" -> return "bash"
    Just value -> return value
    Nothing -> return "bash"

{-# NOINLINE bashPath #-}

-- | Run cardano-cli, returning the stdout
execCli
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m String
execCli = GHC.withFrozenCallStack $ H.execFlex "cardano-cli" "CARDANO_CLI"

-- | Run cardano-cli, discarding return value
execCli_
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m ()
execCli_ = GHC.withFrozenCallStack $ void . execCli

-- | Run cardano-cli, returning the stdout
execCli'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> [String]
  -> m String
execCli' execConfig = GHC.withFrozenCallStack $ H.execFlex' execConfig "cardano-cli" "CARDANO_CLI"

-- | Run cardano-cli, returning the exit code, the stdout, and the stderr.
-- Contrary to other functions from this module, this function doesn't fail the test
-- if the call fails. So if you want to test something negative, this is the function to use.
execCliAny
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> [String]
  -> m (ExitCode, String, String) -- ^ The exit code of the call, stdout, stderr.
execCliAny execConfig = GHC.withFrozenCallStack $ H.execFlexAny' execConfig "cardano-cli" "CARDANO_CLI"

-- | Run create-script-context, returning the stdout.
execCreateScriptContext
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m String
execCreateScriptContext =
  GHC.withFrozenCallStack $ H.execFlex "create-script-context" "CREATE_SCRIPT_CONTEXT"

-- | Run create-script-context, returning the stdout.
execCreateScriptContext'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> [String]
  -> m String
execCreateScriptContext' execConfig =
  GHC.withFrozenCallStack $ H.execFlex' execConfig "create-script-context" "CREATE_SCRIPT_CONTEXT"

-- | Call a command of the CLI that returns JSON to stdout. Then parse it,
-- and deserialize it to a Haskell value. Fail the test if a step fails.
-- If your CLI command doesn't support
-- returning JSON to stdout, and needs going through a file instead, probably
-- you should add a similar function to this one.
execCliStdoutToJson :: ()
  => (HasCallStack, Aeson.FromJSON a, MonadTest m, MonadCatch m, MonadIO m)
  => ExecConfig -- ^ The configuration with which to call the CLI
  -> [String] -- ^ The CLI command to execute
  -> m a
execCliStdoutToJson execConfig cmd = GHC.withFrozenCallStack $ do
  result <- execCli' execConfig cmd
  H.note_ result
  H.leftFail . Aeson.eitherDecode $ fromString result

-- | Create a 'CreateProcess' describing how to start the cardano-cli process
-- and an argument list.
procCli
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procCli = GHC.withFrozenCallStack $ H.procFlex "cardano-cli" "CARDANO_CLI"

-- | Create a 'CreateProcess' describing how to start the cardano-node process
-- and an argument list.
procNode
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procNode = GHC.withFrozenCallStack $ H.procFlex "cardano-node" "CARDANO_NODE"

-- | Create a 'CreateProcess' describing how to start the cardano-submit-api process
-- and an argument list.
procSubmitApi
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procSubmitApi = GHC.withFrozenCallStack $ H.procFlex "cardano-submit-api" "CARDANO_SUBMIT_API"

-- | Create a 'CreateProcess' describing how to start the cardano-node-chairman process
-- and an argument list.
procChairman
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procChairman = GHC.withFrozenCallStack $ H.procFlex "cardano-node-chairman" "CARDANO_NODE_CHAIRMAN" . ("run":)

mkExecConfig :: ()
  => MonadTest m
  => MonadIO m
  => FilePath
  -> IO.Sprocket
  -> Int -- ^ Network id
  -> m ExecConfig
mkExecConfig tempBaseAbsPath sprocket networkId = do
  env' <- H.evalIO IO.getEnvironment

  return H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName sprocket)
      , ("CARDANO_NODE_NETWORK_ID", show networkId)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env'
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

-- | Adds environment variables to an 'ExecConfig' that may already
-- have some environment variables set. This is done by prepending the new
-- environment variables to the existing ones.
addEnvVarsToConfig :: H.ExecConfig -> [(String, String)] -> H.ExecConfig
addEnvVarsToConfig execConfig newEnvVars =
  execConfig { H.execConfigEnv = Last $ Just $ newEnvVars <> prevEnvVars }
  where
    prevEnvVars :: [(String, String)]
    prevEnvVars = fromMaybe [] . getLast $ H.execConfigEnv execConfig

-- | Creates an 'ExecConfig' that can be used to run a process offline.
-- e.g cardano-cli without a node running.
mkExecConfigOffline :: ()
  => MonadTest m
  => MonadIO m
  => FilePath
  -> m ExecConfig
mkExecConfigOffline tempBaseAbsPath   = do
  env' <- H.evalIO IO.getEnvironment

  return H.ExecConfig
    { H.execConfigEnv = Last $ Just
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      env'
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }


data ProcessError
  = ProcessIOException IOException
  | ResourceException ResourceCleanupException
  deriving Show

initiateProcess
  :: forall m. MonadCatch m
  => MonadResource m
  => CreateProcess
  -> ExceptT ProcessError m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, ReleaseKey)
initiateProcess cp = do
  (mhStdin, mhStdout, mhStderr, hProcess)
    <- handlesExceptT resourceAndIOExceptionHandlers . liftIOAnnotated $ IO.createProcess cp

  releaseKey <- handlesExceptT resourceAndIOExceptionHandlers
                  . register $ IO.cleanupProcess (mhStdin, mhStdout, mhStderr, hProcess)
  return (mhStdin, mhStdout, mhStderr, hProcess, releaseKey)

-- We can throw an IOException from createProcess or an ResourceCleanupException from the ResourceT monad
resourceAndIOExceptionHandlers :: Applicative m => [Handler m ProcessError]
resourceAndIOExceptionHandlers = [ Handler $ pure . ProcessIOException
                                 , Handler $ pure . ResourceException
                                 ]

