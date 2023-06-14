module Testnet.Process.Run
  ( bashPath
  , execCli
  , execCli_
  , execCli'
  , execCreateScriptContext
  , execCreateScriptContext'
  , procCli
  , procNode
  , procSubmitApi
  , procChairman
  , mkExecConfig
  ) where

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Test.Process (ExecConfig)
import           System.Process (CreateProcess)

import           Data.Monoid (Last (..))
import qualified GHC.Stack as GHC
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import           Hedgehog.Extras.Test.Base
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

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
execCli_ = void . execCli

-- | Run cardano-cli, returning the stdout
execCli'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> [String]
  -> m String
execCli' execConfig = GHC.withFrozenCallStack $ H.execFlex' execConfig "cardano-cli" "CARDANO_CLI"

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
  -> m ExecConfig
mkExecConfig tempBaseAbsPath sprocket = do
  env <- H.evalIO IO.getEnvironment

  noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName sprocket)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }
