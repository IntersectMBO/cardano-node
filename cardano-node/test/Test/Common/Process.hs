module Test.Common.Process
  ( createProcess
  , interruptProcessGroupOf
  , waitForProcess
  ) where

import           Control.Monad.IO.Class
import           Data.Function ((.))
import           Data.Function (($))
import           Data.Maybe (Maybe (..))
import           GHC.Stack (HasCallStack)
import           System.Exit (ExitCode)
import           System.IO (Handle, IO)
import           System.Process (CreateProcess (..), ProcessHandle)

import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified System.Process as IO

createProcess :: HasCallStack
  => CreateProcess
  -> H.PropertyT IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ IO.createProcess cp

interruptProcessGroupOf :: HasCallStack
  => ProcessHandle
  -> H.PropertyT IO ()
interruptProcessGroupOf hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ IO.interruptProcessGroupOf hProcess

waitForProcess :: HasCallStack
  => ProcessHandle
  -> H.PropertyT IO ExitCode
waitForProcess hProcess = GHC.withFrozenCallStack $ do
  H.evalM . liftIO $ IO.waitForProcess hProcess
