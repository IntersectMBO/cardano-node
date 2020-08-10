{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Common.Process
  ( createProcess
  , getProjectBase
  , procNode
  , interruptProcessGroupOf
  , waitForProcess
  ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Function
import           Data.Maybe (Maybe (..))
import           Data.Semigroup ((<>))
import           Data.String (String)
import           GHC.Stack (HasCallStack)
import           System.Exit (ExitCode)
import           System.IO (Handle, IO)
import           System.Process (CmdSpec (..), CreateProcess (..), ProcessHandle)

import qualified Data.List as L
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified System.Environment as IO
import qualified System.Process as IO

createProcess :: HasCallStack
  => CreateProcess
  -> H.PropertyT IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = GHC.withFrozenCallStack $ do
  case IO.cmdspec cp of
    RawCommand cmd args -> H.annotate $ "Command line: " <> cmd <> " " <> L.unwords args
    ShellCommand cmd -> H.annotate $ "Command line: " <> cmd
  H.evalM . liftIO $ IO.createProcess cp

interruptProcessGroupOf :: HasCallStack
  => ProcessHandle
  -> H.PropertyT IO ()
interruptProcessGroupOf hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ IO.interruptProcessGroupOf hProcess

waitForProcess :: HasCallStack
  => ProcessHandle
  -> H.PropertyT IO (Maybe ExitCode)
waitForProcess hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> return Nothing

procNode
  :: [String]
  -- ^ Arguments to the CLI command
  -> H.PropertyT IO CreateProcess
  -- ^ Captured stdout
procNode arguments = do
  maybeCardanoCli <- liftIO $ IO.lookupEnv "CARDANO_NODE"
  cp <- case maybeCardanoCli of
    Just cardanoCli -> return $ IO.proc cardanoCli arguments
    Nothing -> return $ IO.proc "cabal" ("exec":"--":"cardano-node":arguments)
  return $ cp
    { IO.create_group = True
    , IO.std_in = IO.CreatePipe
    }

getProjectBase :: H.PropertyT IO String
getProjectBase = do
  maybeCardanoCli <- liftIO $ IO.lookupEnv "CARDANO_NODE_SRC"
  case maybeCardanoCli of
    Just path -> return path
    Nothing -> return ".."
