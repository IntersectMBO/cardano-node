{-# LANGUAGE RankNTypes #-}


module Cardano.Node.Features.Node
  ( NodeLayer(..)
  , createNodeFeature
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types (CardanoEnvironment,
                                       CardanoEnvironment,
                                       NodeProtocolMode (..))
import           Cardano.Config.Logging (LoggingLayer (..),)
import           Cardano.Node.Run
import           Cardano.Shell.Types (CardanoFeature (..))

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
  -> CardanoEnvironment
  -> NodeProtocolMode
  -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer _ npm = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional
    -- configuration from, it could be from the filesystem, so
    -- we give him the most flexible/powerful context, @IO@.

    -- Construct the node layer
    let nodeLayer = NodeLayer {
                        nlRunNode = liftIO $ runNode loggingLayer npm
                      }

    -- Construct the cardano feature
    let cardanoFeature :: CardanoFeature
        cardanoFeature =
         CardanoFeature
           { featureName = "NodeFeature"
           , featureStart = void $ pure nodeLayer
           , featureShutdown = pure ()
           }

    pure (nodeLayer, cardanoFeature)
