{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main (main) where

import           Options.Applicative

import           Cardano.Shell.Features.Logging (LoggingCLIArguments (..),
                                                 LoggingLayer (..),
                                                 createLoggingFeature, loggingParser)
import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Configuration.Lib (finaliseCardanoConfiguration)
import           Cardano.Shell.Constants.Types (CardanoConfiguration (..))
import           Cardano.Shell.Lib (GeneralException (..), runCardanoApplicationWithFeatures)
import           Cardano.Shell.Constants.PartialTypes (PartialCardanoConfiguration (..),
                                                       PartialCore (..))
import           Cardano.Shell.Presets (mainnetConfiguration)
import           Cardano.Shell.Types (ApplicationEnvironment (Development),
                                      CardanoApplication (..),
                                      CardanoEnvironment, CardanoFeature (..),
                                      CardanoFeatureInit (..),
                                      initializeCardanoEnvironment)
import           Cardano.Prelude (throwIO)

import           CLI
import           Run

-- | The product type of all command line arguments
data ArgParser = ArgParser !LoggingCLIArguments !CLI

-- | The product parser for all the CLI arguments.
--
commandLineParser :: Parser ArgParser
commandLineParser = ArgParser
    <$> loggingParser
    <*> parseCLI

-- | Top level parser with info.
--
opts :: ParserInfo ArgParser
opts = info (commandLineParser <**> helper)
  ( fullDesc
  <> progDesc "Cardano wallet node."
  <> header "Demo client to run.")


-- TODO move this to `cardano-shell` and use it in `cardano-node` as well.
-- Better than a partial pattern match.
--
data PartialConfigError = PartialConfigError Text
  deriving (Eq, Show, Typeable)

instance Exception PartialConfigError

-- | Main function.
main :: IO ()
main = do

    cardanoConfiguration <-
      case finaliseCardanoConfiguration mainnetConfiguration of
        Right cc -> return cc
        Left err -> throwIO (PartialConfigError err)
    cardanoEnvironment  <- initializeCardanoEnvironment

    logConfig           <- execParser opts

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig cardanoConfiguration cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures Development cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: ArgParser -> PartialCardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (ArgParser logCli cli) partialConfig cardanoEnvironment = do

    finalConfig <- either (throwIO . ConfigurationError) pure $
          finaliseCardanoConfiguration $
          -- Here we perform merging of layers of configuration, but for now, only in a trivial way,
          -- just for Cardano.Shell.Constants.Types.Genesis.
          -- We expect this process to become generic at some point.
          partialConfig { pccCore =
                          flip fmap (pccCore partialConfig) $
                          \x -> x { pcoGenesis = (<>) <$> pcoGenesis x <*> cliGenesis cli }}

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig logCli
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer cli cardanoEnvironment finalConfig

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
    featureStart' _ loggingLayer cc cli = do
        let tr = llAppendName loggingLayer "wallet" (llBasicTrace loggingLayer)
        pure $ NodeLayer {nlRunNode = liftIO $ runClient cli tr cc}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()


nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer = CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = pure ()
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }
