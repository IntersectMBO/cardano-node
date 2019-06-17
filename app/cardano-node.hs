{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Cardano.Prelude
import           Cardano.Shell.Features.Logging (LoggingLayer (..), Trace,
                                                 createLoggingFeature)
import           Cardano.Shell.Lib              (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Presets          (mainnetConfiguration)
import           Cardano.Shell.Presets          (devConfiguration)
import           Cardano.Shell.Types            (ApplicationEnvironment (..),
                                                 CardanoApplication (..),
                                                 initializeCardanoEnvironment)
import           Features.Blockchain            (BlockchainConfig (..),
                                                 BlockchainLayer (..),
                                                 createBlockchainFeature)

import           CLI                            (execParser, fullDesc, helper,
                                                 info, parseCLI, progDesc)
import           Parser                         (parseFunc)

main :: IO ()
main = do

    -- Parse command line arguments
    cli <- execParser opts
    let blcConfig = BlockchainConfig cli

    let cardanoConfiguration = devConfiguration
    cardanoEnvironment <- initializeCardanoEnvironment

    --Features 'NodeApp' will use.
    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment cardanoConfiguration
    blockchainFeature  <- runExceptT $ createBlockchainFeature cardanoEnvironment cardanoConfiguration blcConfig Production loggingLayer

    case blockchainFeature  of
      Left err -> print err
      Right (nodeLayer, blockchainFeature ) ->
        runCardanoApplicationWithFeatures Production [blockchainFeature , loggingFeature] . CardanoApplication $ nodeApp loggingLayer nodeLayer blcConfig
  where
   opts = info (parseCLI <**> helper) (fullDesc <> progDesc "Run a node with the chain-following protocol hooked in.")

nodeApp :: LoggingLayer -> BlockchainLayer -> BlockchainConfig -> IO ()
nodeApp ll bcl bcc = do
  mainTrace <- llAppendName ll "node" (llBasicTrace ll)

  -- Run node
  shellRunNode mainTrace bcc bcl ll

shellRunNode :: Trace IO Text -> BlockchainConfig -> BlockchainLayer -> LoggingLayer -> IO ()
shellRunNode logTrace bcc bcl ll = do
    logNotice logTrace "Begin shellRunNode..."
    (bcHandleSimpleNode bcl) (parsedConfig bcc) (ll)
  where
    logNotice :: Trace IO Text -> Text -> IO ()
    logNotice = llLogNotice ll

    nodeLoop :: Trace IO Text -> IO ()
    nodeLoop logTrace = logNotice logTrace "In nodeLoop function!"
