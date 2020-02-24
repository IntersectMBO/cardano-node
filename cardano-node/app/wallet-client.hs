{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)

import           Options.Applicative

import           Cardano.BM.Trace
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..))

import           Cardano.Common.Parsers (parseConfigFile, parseGenesisFile, parseSocketPath)
import           Cardano.Config.CommonCLI
import           Cardano.Config.Types ( CardanoEnvironment (..), ConfigYamlFilePath (..)
                                      , DelegationCertFile(..), SigningKeyFile (..))
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Wallet.Logging (WalletCLI(..), createLoggingFeatureWallet)
import           Cardano.Wallet.Run

-- | Main function.
main :: IO ()
main = do

    let cardanoEnvironment = NoEnvironment

    logConfig           <- execParser opts

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)

-- | Top level parser with info.
--
opts :: ParserInfo WalletCLI
opts = info (parseWalletCLI <**> helper)
  ( fullDesc
  <> progDesc "Cardano wallet node."
  <> header "Demo client to run.")

parseWalletCLI :: Parser WalletCLI
parseWalletCLI = WalletCLI
    <$> (ConfigYamlFilePath <$> parseConfigFile)
    <*> optional (DelegationCertFile <$> parseDelegationCert)
    <*> parseGenesisHash
    <*> parseGenesisFile "genesis-json"
    <*> optional (SigningKeyFile <$> parseSigningKey)
    <*> parseSocketPath "Path to a cardano-node socket."

initializeAllFeatures :: WalletCLI  -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures wCli cardanoEnvironment = do
    (loggingLayer, loggingFeature) <- createLoggingFeatureWallet cardanoEnvironment wCli
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer wCli cardanoEnvironment

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
                           , featureStart      = void $ pure nodeLayer
                           , featureShutdown   = pure ()
                           }

    -- we return both
    pure (nodeLayer, cardanoFeature)

  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> WalletCLI -> IO NodeLayer
    featureStart' _ loggingLayer' walletCli = do
        let tr :: MonadIO m => Trace m Text
            tr = llAppendName loggingLayer "wallet" (llBasicTrace loggingLayer')
        pure $ NodeLayer {nlRunNode = liftIO $ runClient walletCli tr}
