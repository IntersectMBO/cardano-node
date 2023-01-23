{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Utils
  ( QueryTipOutput(..)
  , queryTip
  , waitUntilEpoch

  -- ** Parsers
  , pMaxLovelaceSupply
  ) where

import           Cardano.Api
import           Prelude

import           Cardano.CLI.Shelley.Output
import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Word
import           GHC.Stack
import           Options.Applicative
import           System.Directory (doesFileExist, removeFile)

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import           Hedgehog.Extras.Test.Process (ExecConfig)
import           Hedgehog.Internal.Property (MonadTest)

import           Testnet.Cardano (CardanoTestnetOptions (..), defaultTestnetOptions)
import qualified Testnet.Util.Process as H

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
  exists <- liftIO $ doesFileExist fp
  when exists $ liftIO $ removeFile fp

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
      else do liftIO $ threadDelay 10_000_000
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
  exists <- liftIO $ doesFileExist fp
  when exists $ liftIO $ removeFile fp

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
      <>  value (cardanoMaxSupply defaultTestnetOptions)
      )
