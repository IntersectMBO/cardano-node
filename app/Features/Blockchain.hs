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

newtype BlockchainLayer = BlockchainLayer { dummy :: forall m . MonadIO m =>  m () }

{-
newtype BlockchainLayer = BlockchainLayer
  { bcHandleSimpleNode :: forall blk. RunDemo blk => DemoProtocol blk -> CLI -> TopologyInfo -> IO () }
-}

cleanup :: forall m . MonadIO m => m ()
cleanup = pure ()

-- This needs to basically be populated with the results from the opt-applicative parser
newtype BlockchainConfig = BlockchainConfig {dummyConfig :: Text}
{-
-- TODO: I think this type should eventually be removed because we should
-- be getting the configuration from CardanoConfiguration
newtype BlockchainConfig = BlockchainConfig { cfg :: CLI, topology :: TopologyInfo}
-}

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
  {-
  --TODO: Need to also pass in logging here, i.e modify handleSimpleNode
  bcHandleSimpleNode demoProtocol (cfg config) (cfg topology)
  -}
  return ()

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> ApplicationEnvironment
  -> LoggingLayer
   -- TODO: Should be a typed error not Text
  -> ExceptT Text IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature _ cc appEnv loggingLayer = do
    --TODO: Get topology from cc

  let nodeConf = BlockchainConfig "DummyConfig"

  let bcLayer   = BlockchainLayer $ return ()
  {-
  let bcLayer = BlockchainLayer $ handleSimpleNode
  -}

  let
    nFeature = CardanoFeature
      { featureName     = "Simple Node"
      , featureStart    = init nodeConf appEnv bcLayer loggingLayer
      , featureShutdown = cleanup
      }
  pure (bcLayer, nFeature)
