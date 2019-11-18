{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),)

import           Cardano.Common.Help
import           Cardano.Common.Parsers
import           Cardano.Config.CommonCLI (parseCommonCLIAdvanced)
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Config.Types
import           Cardano.Config.Partial
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Topology
                 ( NodeAddress (..), NodeHostAddress (..)
                 , TopologyInfo)
import           Cardano.Node.Features.Node

main :: IO ()
main = do
    cli <- Opt.execParser opts

    (features, nodeLayer) <- initializeAllFeatures cli env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      opts :: Opt.ParserInfo NodeCLI
      opts =
        Opt.info (nodeCliParser
                    <**> helperBrief "help" "Show this help text" nodeCliHelpMain
                    <**> helperBrief "help-tracing" "Show help for tracing options" cliHelpTracing
                    <**> helperBrief "help-advanced" "Show help for advanced options" cliHelpAdvanced
                 )

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

      helperBrief :: String -> String -> String -> Parser (a -> a)
      helperBrief l d helpText = Opt.abortOption (Opt.InfoMsg helpText) $ mconcat
        [ Opt.long l
        , Opt.help d ]

      nodeCliHelpMain :: String
      nodeCliHelpMain = renderHelpDoc 80 $
        parserHelpHeader "cardano-node" nodeCliParser
        <$$> ""
        <$$> parserHelpOptions nodeCliParser

      cliHelpTracing :: String
      cliHelpTracing = renderHelpDoc 80 $
        "Additional tracing options:"
        <$$> ""
        <$$> parserHelpOptions cliTracingParser

      cliHelpAdvanced :: String
      cliHelpAdvanced = renderHelpDoc 80 $
        "Advanced options:"
        <$$> ""
        <$$> parserHelpOptions parseCommonCLIAdvanced


initializeAllFeatures
  :: NodeCLI
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures nCli@(NodeCLI _ _ ncFp _)
                       cardanoEnvironment = do

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment nCli

    nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
    (nodeLayer   , nodeFeature)    <-
      createNodeFeature
        loggingLayer
        cardanoEnvironment
        nodeConfig
        nCli

    pure ([ loggingFeature
          , nodeFeature
          ] :: [CardanoFeature]
         , nodeLayer)
