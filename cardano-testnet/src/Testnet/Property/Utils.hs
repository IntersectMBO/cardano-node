{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Property.Utils
  ( integration
  , integrationRetryWorkspace
  , integrationWorkspace
  , isLinux

  , QueryTipOutput(..)
  , queryTip
  , waitUntilEpoch

  -- ** Parsers
  , pMaxLovelaceSupply

  -- ** Genesis
  , getByronGenesisHash
  , getShelleyGenesisHash
  ) where

import           Cardano.Api

import           Cardano.CLI.Shelley.Output
import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Key
import           Data.Aeson.KeyMap
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Word
import           GHC.Stack
import qualified GHC.Stack as GHC
import           Options.Applicative
import           System.Directory (doesFileExist, removeFile)
import           System.Info (os)

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Testnet.Process.Run as H

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import           Hedgehog.Extras.Test.Process (ExecConfig)
import           Hedgehog.Internal.Property (MonadTest)
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

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


-- | Submit the desired epoch to wait to.
waitUntilEpoch
  :: (MonadCatch m, MonadIO m, MonadTest m)
  => FilePath
  -- ^ Output file
  -> Int
  -- ^ Testnet magic
  -> ExecConfig
  -> EpochNo
  -- ^ Desired epoch
  -> m EpochNo
waitUntilEpoch fp testnetMagic execConfig desiredEpoch = do
  exists <- H.evalIO $ doesFileExist fp
  when exists $ H.evalIO $ removeFile fp

  void $ H.execCli' execConfig
    [ "query",  "tip"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", fp
    ]

  tipJSON <- H.leftFailM $ H.readJsonFile fp
  tip <- H.noteShowM $ H.jsonErrorFail $ fromJSON @QueryTipLocalStateOutput tipJSON
  case mEpoch tip of
    Nothing ->
      H.failMessage
        callStack "waitUntilEpoch: cardano-cli query tip returned Nothing for EpochNo"
    Just currEpoch ->
      if currEpoch >= desiredEpoch
      then return currEpoch
      else do H.threadDelay 10_000_000
              waitUntilEpoch fp testnetMagic execConfig desiredEpoch

queryTip
  :: (MonadCatch m, MonadIO m, MonadTest m)
  => QueryTipOutput
  -- ^ Output file
  -> Int
  -- ^ Testnet magic
  -> ExecConfig
  -> m QueryTipLocalStateOutput
queryTip (QueryTipOutput fp) testnetMag execConfig = do
  exists <- H.evalIO $ doesFileExist fp
  when exists $ H.evalIO $ removeFile fp

  void $ H.execCli' execConfig
    [ "query",  "tip"
    , "--testnet-magic", show @Int testnetMag
    , "--out-file", fp
    ]

  tipJSON <- H.leftFailM $ H.readJsonFile fp
  H.noteShowM $ H.jsonErrorFail $ fromJSON @QueryTipLocalStateOutput tipJSON

newtype QueryTipOutput = QueryTipOutput { unQueryTipOutput :: FilePath}


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
