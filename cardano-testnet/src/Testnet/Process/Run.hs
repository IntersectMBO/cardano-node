module Testnet.Process.Run
  ( bashPath
  , execCli
  , execCli_
  , execCli'
  , execCliAny
  , execCreateScriptContext
  , execCreateScriptContext'
  , initiateProcess
  , procCli
  , procNode
  , procSubmitApi
  , procChairman
  , mkExecConfig
  , ProcessError(..)
  , ExecutableError(..)
  ) where

import           Prelude

import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Function
import qualified Data.List as List
import           Data.Monoid (Last (..))
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import qualified System.Directory as IO
import qualified System.Environment as IO
import           System.Exit (ExitCode)
import           System.FilePath
import           System.IO
import qualified System.IO.Unsafe as IO
import qualified System.Process as IO
import           System.Process

import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Plan (Component (..), Plan (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import           Hedgehog.Extras.Test.Base
import           Hedgehog.Extras.Test.Process (ExecConfig)
import qualified Hedgehog.Extras.Test.Process as H
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
  -> m (ExitCode, String, String) -- ^ The exit code of the call, stdoud, stderr.
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
  :: [String]
  -- ^ Arguments to the CLI command
  -> ExceptT ExecutableError IO CreateProcess
  -- ^ Captured stdout
procNode = GHC.withFrozenCallStack $ procFlexNew "cardano-node" "CARDANO_NODE"

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

  noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName sprocket)
      , ("CARDANO_NODE_NETWORK_ID", show networkId)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env'
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }


data ProcessError
  = ProcessIOException IOException
  | ResourceException ResourceCleanupException
  deriving Show

initiateProcess
  :: CreateProcess
  -> ExceptT ProcessError (ResourceT IO) (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, ReleaseKey)
initiateProcess cp = do

  (mhStdin, mhStdout, mhStderr, hProcess)
    <- handlesExceptT resourceAndIOExceptionHandlers . lift $ IO.createProcess cp

  releaseKey <- handlesExceptT resourceAndIOExceptionHandlers
                  . register $ IO.cleanupProcess (mhStdin, mhStdout, mhStderr, hProcess)

  return (mhStdin, mhStdout, mhStderr, hProcess, releaseKey)

-- We can throw an IOException from createProcess or an ResourceCleanupException from the ResourceT monad
resourceAndIOExceptionHandlers :: [Handler (ResourceT IO) ProcessError]
resourceAndIOExceptionHandlers = [ Handler $ return . ProcessIOException
                                 , Handler $ return . ResourceException
                                 ]


procFlexNew
  :: String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> ExceptT ExecutableError IO CreateProcess
  -- ^ Captured stdout
procFlexNew = procFlexNew' H.defaultExecConfig

procFlexNew'
  :: H.ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> ExceptT ExecutableError IO CreateProcess
  -- ^ Captured stdout
procFlexNew' execConfig pkg binaryEnv arguments = GHC.withFrozenCallStack $ do
  bin <- binFlexNew pkg binaryEnv
  return (IO.proc bin arguments)
    { IO.env = getLast $ H.execConfigEnv execConfig
    , IO.cwd = getLast $ H.execConfigCwd execConfig
    -- this allows sending signals to the created processes, without killing the test-suite process
    , IO.create_group = True
    }

-- | Compute the path to the binary given a package name or an environment variable override.
binFlexNew
  :: String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> ExceptT ExecutableError IO FilePath
  -- ^ Path to executable
binFlexNew pkg binaryEnv = do
  maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return envBin
    Nothing -> binDist pkg

-- | Find the nearest plan.json going upwards from the current directory.
findDefaultPlanJsonFile :: IO FilePath
findDefaultPlanJsonFile = IO.getCurrentDirectory >>= go
  where go :: FilePath -> IO FilePath
        go d = do
          let file = d </> "dist-newstyle/cache/plan.json"
          exists <- IO.doesFileExist file
          if exists
            then return file
            else do
              let parent = takeDirectory d
              if parent == d
                then return "dist-newstyle/cache/plan.json"
                else go parent


-- | Discover the location of the plan.json file.
planJsonFile :: IO FilePath
planJsonFile = do
  maybeBuildDir <- liftIO $ IO.lookupEnv "CABAL_BUILDDIR"
  case maybeBuildDir of
    Just buildDir -> return $ ".." </> buildDir </> "cache/plan.json"
    Nothing -> findDefaultPlanJsonFile
{-# NOINLINE planJsonFile #-}

data ExecutableError
  = CannotDecodePlanJSON FilePath String
  | RetrievePlanJsonFailure IOException
  | ReadFileFailure IOException
  | MissingExecutable FilePath String
  deriving Show


-- | Consult the "plan.json" generated by cabal to get the path to the executable corresponding.
-- to a haskell package.  It is assumed that the project has already been configured and the
-- executable has been built.
binDist
  :: String
  -- ^ Package name
  -> ExceptT ExecutableError IO FilePath
  -- ^ Path to executable
binDist pkg = do
  pJsonFp <- handleIOExceptT RetrievePlanJsonFailure planJsonFile
  contents <- handleIOExceptT ReadFileFailure $ LBS.readFile pJsonFp

  case Aeson.eitherDecode contents of
    Right plan -> case List.filter matching (plan & installPlan) of
      (component:_) -> case component & binFile of
        Just bin -> return $ addExeSuffix (Text.unpack bin)
        Nothing -> left $ MissingExecutable pJsonFp $ "missing bin-file in: " <> show component
      [] -> error $ "Cannot find exe:" <> pkg <> " in plan"
    Left message -> left $ CannotDecodePlanJSON pJsonFp $ "Cannot decode plan: " <> message
  where matching :: Component -> Bool
        matching component = case componentName component of
          Just name -> name == Text.pack ("exe:" <> pkg)
          Nothing -> False

addExeSuffix :: String -> String
addExeSuffix s = if ".exe" `List.isSuffixOf` s
  then s
  else s <> exeSuffix

exeSuffix :: String
exeSuffix = if OS.isWin32 then ".exe" else ""
