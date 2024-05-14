{-# LANGUAGE DataKinds #-}
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

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import           GHC.Stack
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
-- The argument forces IO monad to prevent leaking of `MonadResource` to the child thread
runInBackground :: MonadTest m
                => MonadResource m
                => MonadCatch m
                => IO a
                -> m ()
runInBackground act = void . H.evalM $ allocate (H.async act) cleanUp
  where
    cleanUp :: H.Async a -> IO ()
    cleanUp a = H.cancel a >> void (H.link a)

decodeEraUTxO :: (IsShelleyBasedEra era, MonadTest m) => ShelleyBasedEra era -> Aeson.Value -> m (UTxO era)
decodeEraUTxO _ = H.jsonErrorFail . Aeson.fromJSON

