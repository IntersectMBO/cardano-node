{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Property.Util
  ( integration
  , integrationRetryWorkspace
  , integrationWorkspace
  , isLinux

  , decodeEraUTxO
  ) where

import           Cardano.Api

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Control.Retry as R
import qualified Data.Aeson as Aeson
import           GHC.Stack
import qualified System.Directory as IO
import qualified System.Environment as IO
import           System.FilePath ((</>))
import           System.Info (os)
import qualified System.IO as IO
import qualified System.IO.Temp as IO
import qualified System.IO.Unsafe as IO

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.CallStack as H
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
        H.runFinallies $ workspace (workspaceName <> "-no-retries") f
    else
      integration $ H.retry n $ \i ->
        H.runFinallies $ workspace (workspaceName <> "-" <> show i) f

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
-- TODO: this is a version which retries deleting of a workspace on exception - upstream to hedgehog-extras
workspace
  :: MonadTest m
  => HasCallStack
  => MonadResource m
  => FilePath
  -> (FilePath -> m ())
  -> m ()
workspace prefixPath f = withFrozenCallStack $ do
  systemTemp <- H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  ws <- H.evalIO $ IO.createTempDirectory systemTemp $ prefixPath <> "-test"
  H.annotate $ "Workspace: " <> ws
  H.evalIO $ IO.writeFile (ws </> "module") H.callerModuleName
  f ws
  when (os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    -- try to delete the directory 5 times, 100ms apart
    let retryPolicy = R.constantDelay 100_000 <> R.limitRetries 10
        -- retry only on IOExceptions
        ioExH _ = Handler $ \(_ :: IOException) -> pure True
    -- For some reason, the temporary directory removal sometimes fails.
    -- Lets wrap this in MonadResource try multiple times before we fail.
    void
      . register
      . R.recovering retryPolicy [ioExH]
      . const
      $ IO.removePathForcibly ws

-- | The 'FilePath' in '(FilePath -> H.Integration ())' is the work space directory.
-- This is created (and returned) via 'H.workspace'.
integrationWorkspace :: HasCallStack => FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationWorkspace workspaceName f = withFrozenCallStack $
  integration $ H.runFinallies $ workspace workspaceName f

isLinux :: Bool
isLinux = os == "linux"

decodeEraUTxO :: (IsShelleyBasedEra era, MonadTest m) => ShelleyBasedEra era -> Aeson.Value -> m (UTxO era)
decodeEraUTxO _ = H.jsonErrorFail . Aeson.fromJSON

