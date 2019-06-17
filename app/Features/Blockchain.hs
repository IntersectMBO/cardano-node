{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Features.Blockchain
  ( BlockchainLayer(..)
  , BlockchainConfig(..)
  , createBlockchainFeature
  )
where

import           Cardano.Prelude

import           Cardano.Shell.Constants.Types  (CardanoConfiguration (..))
import           Cardano.Shell.Features.Logging (LoggingLayer (..))
import           Cardano.Shell.Types            (ApplicationEnvironment (..),
                                                 CardanoEnvironment,
                                                 CardanoFeature (..))
import           CLI                            (CLI)
import           Run                            (runNode)


newtype BlockchainLayer = BlockchainLayer
  { bcHandleSimpleNode :: CLI -> LoggingLayer -> IO () }


cleanup :: forall m . MonadIO m => m ()
cleanup = pure ()

newtype BlockchainConfig = BlockchainConfig { parsedConfig :: CLI }

init
  :: forall m
   . MonadIO m
  => BlockchainConfig
  -> ApplicationEnvironment
  -> BlockchainLayer
  -> LoggingLayer
  -> m ()
init config appEnv bcl ll = do
  _ <- case appEnv of
    Development -> pure $ "Development"
    Production  -> pure $ "Production"
    -- It seems in validate mainnet we used this to
    -- check for the result in an MVAR
    -- we are not looking for a result here. Therefore
    -- I think this init function does not make sense at the moment.
  return ()

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> BlockchainConfig
  -> ApplicationEnvironment
  -> LoggingLayer
  -> ExceptT Text IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature _ cc nodeConf appEnv loggingLayer = do

  let bcLayer = BlockchainLayer $ runNode

  let
    nFeature = CardanoFeature
      { featureName     = "Simple Node"
      , featureStart    = init nodeConf appEnv bcLayer loggingLayer
      , featureShutdown = cleanup
      }
  pure (bcLayer, nFeature)
