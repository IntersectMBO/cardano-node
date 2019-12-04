{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)

import           Control.Tracer
import           Options.Applicative

import           Cardano.BM.Data.LogItem
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Types (CardanoEnvironment (..))
import           Cardano.Config.Logging (LoggingCLIArguments (..),
                                                LoggingLayer (..),
                                                createLoggingFeature
                                                )
import           Cardano.Common.Parsers (loggingParser, nodeCliParser)
import           Cardano.Wallet.Run

-- | The product type of all command line arguments
data ArgParser = ArgParser !LoggingCLIArguments !WalletCLI

-- | The product parser for all the WalletCLI arguments.
--
commandLineParser :: Parser ArgParser
commandLineParser = ArgParser
    <$> loggingParser
    <*> parseWalletCLI

parseWalletCLI :: Parser WalletCLI
parseWalletCLI = WalletCLI
    <$> nodeCliParser
    <*> parseGenesisHash

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

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: ArgParser  -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (ArgParser _ cli) cardanoEnvironment = do
    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment $ waNodeCli cli
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer cli cardanoEnvironment

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

createNodeFeature :: LoggingLayer -> WalletCLI -> CardanoEnvironment -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer cli cardanoEnvironment = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.

    -- we construct the layer
    nodeLayer <- featureStart' cardanoEnvironment loggingLayer cli

    -- we construct the cardano feature
    let cardanoFeature = CardanoFeature
                           { featureName       = "NodeFeature"
                           , featureStart      = pure ()
                           , featureShutdown   = pure ()
                           }

    -- we return both
    pure (nodeLayer, cardanoFeature)

  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> WalletCLI -> IO NodeLayer
    featureStart' _ loggingLayer' walletCli = do
        let tr :: MonadIO m => Tracer m (Cardano.BM.Data.LogItem.LogObject Text)
            tr = llAppendName loggingLayer "wallet" (llBasicTrace loggingLayer')
        pure $ NodeLayer {nlRunNode = liftIO $ runClient walletCli tr}
