{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Cardano.Prelude hiding (option)

import           Control.Tracer
import           Options.Applicative

import           Cardano.BM.Data.LogItem
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),
                                      CardanoFeatureInit (..))
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract (NumCoreNodes (..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Partial (PartialCardanoConfiguration (..))
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Types (CardanoConfiguration (..),
                                                   CardanoEnvironment (..))
import           Cardano.Config.Logging (LoggingCLIArguments (..),
                                                LoggingLayer (..),
                                                createLoggingFeature
                                                )
import           Cardano.Common.Parsers (loggingParser, parseCoreNodeId)
import           Cardano.Common.Parsers
import           Cardano.Wallet.Run

-- | The product type of all command line arguments
data ArgParser = ArgParser !LoggingCLIArguments !CLI

-- | The product parser for all the CLI arguments.
--
commandLineParser :: Parser ArgParser
commandLineParser = ArgParser
    <$> loggingParser
    <*> parseWalletCLI

parseWalletCLI :: Parser CLI
parseWalletCLI = CLI
    <$> parseCoreNodeId
    <*> parseNumCoreNodes
    <*> parseProtocol
    <*> parseCommonCLI
    <*> parseCommonCLIAdvanced

parseNumCoreNodes :: Parser NumCoreNodes
parseNumCoreNodes =
    option (fmap NumCoreNodes auto) (
            long "num-core-nodes"
         <> short 'm'
         <> metavar "NUM-CORE-NODES"
         <> help "The number of core nodes"
    )

-- | Top level parser with info.
--
opts :: ParserInfo ArgParser
opts = info (commandLineParser <**> helper)
  ( fullDesc
  <> progDesc "Cardano wallet node."
  <> header "Demo client to run.")

-- | Main function.
main :: IO ()
main = do

    let cardanoEnvironment = NoEnvironment

    logConfig           <- execParser opts

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig mainnetConfiguration cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: ArgParser -> PartialCardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (ArgParser logCli cli) partialConfig cardanoEnvironment = do
    finalConfig <-
      case mkConfiguration partialConfig (cliCommon cli) (cliCommonAdv cli) of
        Left err -> throwIO err
        Right x -> pure x

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

type NodeCardanoFeature = CardanoFeatureInit CardanoEnvironment LoggingLayer CardanoConfiguration CLI NodeLayer


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
        let tr :: MonadIO m => Tracer m (Cardano.BM.Data.LogItem.LogObject Text)
            tr = llAppendName loggingLayer "wallet" (llBasicTrace loggingLayer)
        pure $ NodeLayer {nlRunNode = liftIO $ runClient cli tr cc}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()


nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer = CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = pure ()
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }
