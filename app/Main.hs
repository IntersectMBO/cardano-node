{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main (main) where

import           Data.Semigroup ((<>))
import           Prelude hiding ((.))

import           Cardano.Prelude
import           Cardano.Shell.Constants.Types (CardanoConfiguration (..))
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Presets (mainnetConfiguration)
import           Cardano.Shell.Types (ApplicationEnvironment (Development),
                                      CardanoApplication (..),
                                      CardanoEnvironment, CardanoFeature (..),
                                      CardanoFeatureInit (..),
                                      NoDependency (..),
                                      initializeCardanoEnvironment)

import           CLI
import           Run

main :: IO ()
main = do

    let cardanoConfiguration = mainnetConfiguration
    cardanoEnvironment <- initializeCardanoEnvironment

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures cardanoConfiguration cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures Development cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: CardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures cardanoConfiguration cardanoEnvironment = do

    (nodeLayer, nodeFeature) <- createNodeFeature NoDependency cardanoEnvironment cardanoConfiguration

    -- Here we return all the features.
    let allCardanoFeatures :: [CardanoFeature]
        allCardanoFeatures =
            [ nodeFeature
            ]

    pure (allCardanoFeatures, nodeLayer)

--------------------------------
-- Layer
--------------------------------

data NodeLayer = NodeLayer
    { nlRunNode   :: forall m. MonadIO m => m ()
    }

--------------------------------
-- Node Feature
--------------------------------

-- type NodeCardanoFeature = CardanoFeatureInit LoggingLayer Text NodeLayer
type NodeCardanoFeature = CardanoFeatureInit NoDependency CLI NodeLayer


createNodeFeature :: NoDependency -> CardanoEnvironment -> CardanoConfiguration -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer cardanoEnvironment cardanoConfiguration = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.
    cli <- execParser opts

    -- we construct the layer
    nodeLayer <- (featureInit nodeCardanoFeatureInit) cardanoEnvironment loggingLayer cardanoConfiguration cli

    -- we construct the cardano feature
    let cardanoFeature = nodeCardanoFeature nodeCardanoFeatureInit nodeLayer

    -- we return both
    pure (nodeLayer, cardanoFeature)
  where
    opts = info (parseCLI <**> helper)
      ( fullDesc
     <> progDesc "Run a node with the chain-following protocol hooked in."
     )


nodeCardanoFeatureInit :: NodeCardanoFeature
nodeCardanoFeatureInit = CardanoFeatureInit
    { featureType    = "NodeFeature"
    , featureInit    = featureStart'
    , featureCleanup = featureCleanup'
    }
  where
    featureStart' :: CardanoEnvironment -> NoDependency -> CardanoConfiguration -> CLI -> IO NodeLayer
    featureStart' _ _ _ cli = pure $ NodeLayer {nlRunNode = liftIO $ runNode cli}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()


nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer = CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = nlRunNode nodeLayer
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }
