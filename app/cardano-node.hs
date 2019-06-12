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

import           Parser                         (parseFunc)

main :: IO ()
main = do
  -- The topology comes from a json file at the moment
  -- TODO: Move json file to cardano-node
  -- For now we get out configuration from the commandline
  -- I believe we would want to get the entire configuration from
  -- CardanoConfiguration
  {-
  cli <- execParser opts

  Populate BlockchainConfig here via parser
  then pass it to nodeApp
  -}
  let dummyBlockchainConfig = BlockchainConfig "dummy"

  let cardanoConfiguration = devConfiguration
  cardanoEnvironment <- initializeCardanoEnvironment

  --Features 'NodeApp' will use.
  (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment cardanoConfiguration
  blockchainFeature  <- runExceptT $ createBlockchainFeature cardanoEnvironment cardanoConfiguration Production loggingLayer

  case blockchainFeature  of
    Left err -> print err
    Right (nodeLayer, blockchainFeature ) ->
      runCardanoApplicationWithFeatures Production [blockchainFeature , loggingFeature] . CardanoApplication $ nodeApp loggingLayer nodeLayer dummyBlockchainConfig


nodeApp :: LoggingLayer -> BlockchainLayer -> BlockchainConfig -> IO ()
nodeApp ll bcl bcc = do
  mainTrace <- llAppendName ll "node" (llBasicTrace ll)

  -- Run node
  shellRunNode mainTrace bcc bcl ll

shellRunNode :: Trace IO Text -> BlockchainConfig -> BlockchainLayer -> LoggingLayer -> IO ()
shellRunNode logTrace bcc bcl ll = do
    logNotice logTrace "Begin shellRunNode..."
    -- bcHandleSimpleNode bcl $ demoProtocol (cfg bcc) (topology bcc)
  where
    logNotice :: Trace IO Text -> Text -> IO ()
    logNotice = llLogNotice ll

    nodeLoop :: Trace IO Text -> IO ()
    nodeLoop logTrace = logNotice logTrace "In nodeLoop function!"
