{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Property.Utils
  ( integration
  , integrationRetryWorkspace
  , integrationWorkspace
  , isLinux
  , runInBackground

  -- ** Parsers
  , pMaxLovelaceSupply

  -- ** Genesis
  , getByronGenesisHash
  , getShelleyGenesisHash

  , convertToEraString
  , decodeEraUTxO
  ) where

import           Cardano.Api

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Key
import           Data.Aeson.KeyMap hiding (map)
import qualified Data.ByteString as BS
import           Data.Char (toLower)
import           Data.Text (Text)
import           Data.Word
import           GHC.Stack
import qualified GHC.Stack as GHC
import           Options.Applicative
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
integration f = GHC.withFrozenCallStack $ H.withTests 1 $ H.propertyOnce f

-- | The 'FilePath' in '(FilePath -> H.Integration ())' is the work space directory.
-- This is created (and returned) via 'H.workspace'.
integrationRetryWorkspace :: HasCallStack => Int -> FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationRetryWorkspace n workspaceName f = GHC.withFrozenCallStack $
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
integrationWorkspace workspaceName f = GHC.withFrozenCallStack $
  integration $ H.runFinallies $ H.workspace workspaceName f

isLinux :: Bool
isLinux = os == "linux"


-- Parsers

pMaxLovelaceSupply :: Parser Word64
pMaxLovelaceSupply =
  option auto
      (   long "max-lovelace-supply"
      <>  help "Max lovelace supply that your testnet starts with."
      <>  metavar "WORD64"
      <>  showDefault
      <>  value 10_020_000_000
      )

-- * Generate hashes for genesis.json files

getByronGenesisHash :: (H.MonadTest m, MonadIO m) => FilePath -> m (KeyMap Aeson.Value)
getByronGenesisHash path = do
  e <- runExceptT $ readGenesisData path
  (_, genesisHash) <- H.leftFail e
  let genesisHash' = unGenesisHash genesisHash
  pure . singleton "ByronGenesisHash" $ toJSON genesisHash'

getShelleyGenesisHash :: (H.MonadTest m, MonadIO m) => FilePath -> Text -> m (KeyMap Aeson.Value)
getShelleyGenesisHash path key = do
  content <- H.evalIO  $ BS.readFile path
  let genesisHash = Crypto.hashWith id content :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString
  pure . singleton (fromText key) $ toJSON genesisHash

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

convertToEraString :: AnyCardanoEra -> String
convertToEraString = map toLower . docToString . pretty

decodeEraUTxO :: (IsShelleyBasedEra era, MonadTest m) => ShelleyBasedEra era -> Aeson.Value -> m (UTxO era)
decodeEraUTxO _ = H.jsonErrorFail . Aeson.fromJSON
