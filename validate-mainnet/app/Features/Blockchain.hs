{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Features.Blockchain
  ( BlockchainLayer(..)
  , genesisConfig
  , createBlockchainFeature
  , configEpochFileDir
  )
where

import Cardano.Prelude

import Formatting (build, sformat)

import Cardano.Chain.Block
  ( ChainValidationState
  , initialChainValidationState
  )
import Cardano.Chain.Common (parseReqNetworkMag)
import Cardano.Chain.Epoch.Validation (EpochError, validateEpochFile)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Shell.Constants.Types (CardanoConfiguration(..), Core(..), Genesis(..))
import Cardano.Shell.Features.Logging (LoggingLayer(..))
import Cardano.Shell.Types
  (ApplicationEnvironment(..), CardanoEnvironment, CardanoFeature(..))

import Test.Cardano.Mirror (mainnetEpochFiles)


-- | `BlockchainLayer` provides a window of sorts that
-- enables us to access values from various 'MVar's
-- that hold the results of specific applications.
newtype BlockchainLayer = BlockchainLayer
  { chainValidationStatus
      :: forall m
       . MonadIO m
      => m (Maybe (Either EpochError ChainValidationState))
  }

data BlockchainConfiguration = BlockchainConfiguration
  { configEpochFileDir :: FilePath
  , genesisConfig      :: Genesis.Config
  }

cleanup :: forall m . MonadIO m => m ()
cleanup = pure ()

-- TODO: This should get the epoch files from a location specified in the config,
-- but for now we use `cardano-mainnet-mirror`.
init
  :: forall m
   . MonadIO m
  => BlockchainConfiguration
  -> ApplicationEnvironment
  -> LoggingLayer
  -> ChainValidationState
  -> MVar (Either EpochError ChainValidationState)
  -> m ()
init config appEnv ll initialCVS cvsVar = do
  files <- case appEnv of
    Development -> take 10 <$> liftIO mainnetEpochFiles
    Production  -> liftIO mainnetEpochFiles

  -- Validate epoch files.
  result <- liftIO . runExceptT $ foldM
    (validateEpochFile (genesisConfig config) ll)
    initialCVS
    files

  liftIO $ putMVar cvsVar result


createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> ApplicationEnvironment
  -> LoggingLayer
  -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature _ cc appEnv loggingLayer = do

  -- Construct `Config` using mainnet-genesis.json
  let mainnetGenFilepath = geSrc . coGenesis $ ccCore cc
  let reqNetworkMagic = parseReqNetworkMag . coRequiresNetworkMagic $ ccCore cc

  config <- either (panic . sformat build) identity
    <$> runExceptT
          (Genesis.mkConfigFromFile reqNetworkMagic mainnetGenFilepath Nothing)

  let
    blockchainConf =
      BlockchainConfiguration "cardano-mainnet-mirror/epochs" config

  -- Create initial `ChainValidationState`.
  initCVS <- either (panic . show) identity
    <$> runExceptT (initialChainValidationState config)

  -- Create MVar that will hold the result of the bulk chain validation.
  cvsVar <- newEmptyMVar

  -- Create Blockchain feature
  let
    bcFeature = CardanoFeature
      { featureName     = "Blockchain"
      -- `featureStart` is the logic to be executed of a given feature.
      , featureStart    = init blockchainConf appEnv loggingLayer initCVS cvsVar
      , featureShutdown = cleanup
      }

  -- Create `BlockchainLayer` which allows us to see the status of
  -- the blockchain feature.
  let
    bcLayer =
      BlockchainLayer
        { chainValidationStatus = liftIO $ tryReadMVar cvsVar}
  pure (bcLayer, bcFeature)
