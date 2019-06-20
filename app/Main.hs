{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Main (main) where

import           Data.Semigroup ((<>))

import           Options.Applicative

import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Constants.Types (CardanoConfiguration (..))
import           Cardano.Shell.Features.Logging (CLIarguments (..),
                                                 LoggingLayer (..),
                                                 createLoggingFeature, parse)
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Presets (mainnetConfiguration)
import           Cardano.Shell.Types (ApplicationEnvironment (Development),
                                      CardanoApplication (..),
                                      CardanoEnvironment, CardanoFeature (..),
                                      CardanoFeatureInit (..),
                                      initializeCardanoEnvironment)

import           CLI
import           Run


-- | The product type of all command line arguments
data ArgParser = ArgParser !CLIarguments !CLI

-- | The product parser for all the CLI arguments.
commandLineParser :: Parser ArgParser
commandLineParser = ArgParser
    <$> parse
    <*> parseCLI

-- | Top level parser with info.
opts :: ParserInfo ArgParser
opts = info (commandLineParser <**> helper)
    ( fullDesc
    <> progDesc "Cardano demo node."
    <> header "Demo node to run." )

-- | Main function.
main :: IO ()
main = do

    let cardanoConfiguration = mainnetConfiguration
    cardanoEnvironment  <- initializeCardanoEnvironment

    logConfig           <- execParser opts

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig cardanoConfiguration cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures Development cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: ArgParser -> CardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (ArgParser logCli cli) cardanoConfiguration cardanoEnvironment = do

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment cardanoConfiguration logCli
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer cli cardanoEnvironment cardanoConfiguration

    -- Here we return all the features.
    let allCardanoFeatures :: [CardanoFeature]
        allCardanoFeatures =
            [ loggingFeature
            , nodeFeature
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
type NodeCardanoFeature = CardanoFeatureInit LoggingLayer CLI NodeLayer


createNodeFeature :: LoggingLayer -> CLI -> CardanoEnvironment -> CardanoConfiguration -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer cli cardanoEnvironment cardanoConfiguration = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.

    -- we construct the layer
    nodeLayer <- (featureInit nodeCardanoFeatureInit) cardanoEnvironment loggingLayer cardanoConfiguration cli

    -- we construct the cardano feature
    let cardanoFeature = nodeCardanoFeature nodeCardanoFeatureInit nodeLayer

    -- we return both
    pure (nodeLayer, cardanoFeature)

nodeCardanoFeatureInit :: NodeCardanoFeature
nodeCardanoFeatureInit = CardanoFeatureInit
    { featureType    = "NodeFeature"
    , featureInit    = featureStart'
    , featureCleanup = featureCleanup'
    }
  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> CardanoConfiguration -> CLI -> IO NodeLayer
    featureStart' _ loggingLayer _ cli = do
        tr <- (llAppendName loggingLayer) "node" (llBasicTrace loggingLayer)
        pure $ NodeLayer {nlRunNode = liftIO $ runNode cli tr}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()


nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer = CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = pure ()
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }
