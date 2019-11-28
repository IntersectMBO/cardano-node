module Cardano.Wallet.Logging
  ( WalletCLI(..)
  , createLoggingFeatureWallet
  ) where

import           Cardano.Prelude

import           Cardano.Shell.Types (CardanoFeature (..))

import           Cardano.Config.Types ( CardanoEnvironment, ConfigYamlFilePath(..)
                                      , DelegationCertFile , GenesisFile, NodeConfiguration(..)
                                      , SigningKeyFile, SocketPath, parseNodeConfiguration)
import           Cardano.Config.Logging ( LoggingLayer (..), loggingCardanoFeatureInit
                                        , loggingCLIConfiguration)

createLoggingFeatureWallet
  :: CardanoEnvironment -> WalletCLI -> IO (LoggingLayer, CardanoFeature)
createLoggingFeatureWallet _ wCli = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional
    -- configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.
    --
    -- Currently we parse outside the features since we want to have a complete
    -- parser for __every feature__.
    nc <- parseNodeConfiguration . unConfigPath $ waConfig wCli
    let logConfigFp = if ncLoggingSwitch nc then Just . unConfigPath $ waConfig wCli else Nothing

    (disabled', loggingConfiguration) <- loggingCLIConfiguration
                                           logConfigFp
                                           (ncLogMetrics nc)

    -- we construct the layer
    (loggingLayer, cleanUpLogging) <- loggingCardanoFeatureInit disabled' loggingConfiguration


    -- we construct the cardano feature
    let cardanoFeature = CardanoFeature
                            { featureName = "LoggingMonitoringFeature"
                            , featureStart = liftIO . void $ pure loggingLayer
                            , featureShutdown = liftIO $ cleanUpLogging loggingLayer
                            }

    -- we return both
    pure (loggingLayer, cardanoFeature)

data WalletCLI = WalletCLI
  { waConfig :: !ConfigYamlFilePath
  , waDelegCertFile :: !(Maybe DelegationCertFile)
  , waGenesisHash :: !Text
  , waGenesisFile :: !GenesisFile
  , waSignKeyFile :: !(Maybe SigningKeyFile)
  , waSocketFile :: !SocketPath
  -- Socket Directory. Will be changed in
  -- an upcoming PR to an actual socket filepath.
  }
