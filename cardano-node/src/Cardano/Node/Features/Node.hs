{-# LANGUAGE RankNTypes #-}


module Cardano.Node.Features.Node
  ( NodeLayer(..)
  , createNodeFeature
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types (CardanoConfiguration (..),
                                       CardanoEnvironment (..))
import           Cardano.Config.Logging (LoggingLayer (..),)
import           Cardano.Node.Run
import           Cardano.Shell.Types (CardanoFeature (..))
import           Cardano.Tracing.Tracers (TraceOptions(..))


-------------------------------------------------------------------------------
-- Layer
-------------------------------------------------------------------------------

data NodeLayer = NodeLayer
    { nlRunNode   :: forall m. MonadIO m => m ()
    }

-------------------------------------------------------------------------------
-- Node Feature
-------------------------------------------------------------------------------


createNodeFeature
  :: LoggingLayer
  -> NodeArgs
  -> TraceOptions
  -> CardanoEnvironment
  -> CardanoConfiguration
  -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer nodeCLI traceOptions cardanoEnvironment cardanoConfiguration = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional
    -- configuration from, it could be from the filesystem, so
    -- we give him the most flexible/powerful context, @IO@.

    -- Construct the node layer
    nodeLayer <- createNodeLayer
                   cardanoEnvironment
                   loggingLayer
                   cardanoConfiguration
                   nodeCLI
                   traceOptions

    -- Construct the cardano feature
    let cardanoFeature :: CardanoFeature
        cardanoFeature =
         CardanoFeature
           { featureName = "NodeFeature"
           , featureStart = void $ pure nodeLayer
           , featureShutdown = pure ()
           }

    pure (nodeLayer, cardanoFeature)
  where
    createNodeLayer :: CardanoEnvironment -> LoggingLayer -> CardanoConfiguration -> NodeArgs -> TraceOptions -> IO NodeLayer
    createNodeLayer _ logLayer cc nodeArgs traceCLI = do
        pure $ NodeLayer {nlRunNode = liftIO $ runNode nodeArgs logLayer traceCLI cc}
