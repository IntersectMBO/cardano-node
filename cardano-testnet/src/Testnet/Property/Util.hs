{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Property.Util
  ( integration
  , integrationRetryWorkspace
  , integrationWorkspace
  , isLinux
  , runInBackground

  , decodeEraUTxO
  ) where

import           Cardano.Api

import           Control.Concurrent.Async
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import           GHC.Stack
import           Network.Mux.Trace
import qualified System.Environment as IO
import           System.Info (os)
import qualified System.IO.Unsafe as IO

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property (MonadTest)


disableRetries :: Bool
disableRetries = IO.unsafePerformIO $ do
  mValue <- IO.lookupEnv "DISABLE_RETRIES"
  return $ mValue == Just "1"
{-# NOINLINE disableRetries #-}

-- TODO: Document what an Integration is
integration :: HasCallStack => H.Integration () -> H.Property
integration f = withFrozenCallStack $ H.withTests 1 $ H.propertyOnce f

-- | The 'FilePath' in '(FilePath -> H.Integration ())' is the work space directory.
-- This is created (and returned) via 'H.workspace'.
integrationRetryWorkspace :: HasCallStack => Int -> FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationRetryWorkspace n workspaceName f = withFrozenCallStack $
  if disableRetries
    then
      integration $
        H.runFinallies $ H.workspace (workspaceName <> "-no-retries") f
    else
      integration $ H.retry n $ \i ->
        H.runFinallies $ H.workspace (workspaceName <> "-" <> show i) f

-- | The 'FilePath' in '(FilePath -> H.Integration ())' is the work space directory.
-- This is created (and returned) via 'H.workspace'.
integrationWorkspace :: HasCallStack => FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationWorkspace workspaceName f = withFrozenCallStack $
  integration $ H.runFinallies $ H.workspace workspaceName f

isLinux :: Bool
isLinux = os == "linux"

-- | Runs an action in background, and registers cleanup to `MonadResource m`
-- Concurrency is tricky in the 'ResourceT' monad. See the "Concurrency" section of
-- https://www.fpcomplete.com/blog/understanding-resourcet/.
runInBackground :: MonadTest m
                => MonadResource m
                => IO ()
                -> IO a
                -> m ()
runInBackground runOnException act  =
  void . H.evalIO
       $ runResourceT
       -- We don't 'wait' because this "background process" may not terminate.
       -- If we 'wait' and it doesn't terminate, 'ResourceT' will not kill it
       -- and the test will hang indefinitely.
       -- Not waiting isn't a problem because this "background process"
       -- is meant to run indefinitely and will be cleaned up by
       -- 'ResourceT' when the test ends or fails.
       -- We use 'asyncWithUnmask' because our logging thread is terminated via an exception.
       -- In order to avoid competing for a file handle we must catch the exception which signals
       -- the logging file is no longer being written to and we can now run the desired additional IO action we
       -- want (runOnException). Attempting to share the 'FileHandle' and use concurrency primitives was not fruitful
       -- and the section "Other ways to abuse ResourceT" in https://www.fpcomplete.com/blog/understanding-resourcet/
       -- confirms this is problematic in 'ResourceT'.
       $ resourceForkWith (\_ -> do r <- H.asyncWithUnmask (\restore -> restore act `E.onException` runOnException)
                                    linkOnly ignoreException r
                          ) $ return ()
 where
  ignoreException  :: E.SomeException -> Bool
  ignoreException e =
    case E.fromException e of
     Just (MuxError errType _) ->
       case errType of
         MuxBearerClosed -> False
         -- This is expected as the background thread is killed.
         -- However we do want to be made aware about other
         -- exceptions.
         _ -> True
     _ -> False

decodeEraUTxO :: (IsShelleyBasedEra era, MonadTest m) => ShelleyBasedEra era -> Aeson.Value -> m (UTxO era)
decodeEraUTxO _ = H.jsonErrorFail . Aeson.fromJSON

