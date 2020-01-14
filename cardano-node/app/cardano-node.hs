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
                                      CardanoFeature (..))

import           Cardano.Common.Help
import           Cardano.Common.TopHandler
import           Cardano.Common.Parsers
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Config.Types
import           Cardano.Node.Features.Node

main :: IO ()
main = toplevelExceptionHandler $ do

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


initializeAllFeatures
  :: NodeCLI
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures nCli@NodeCLI { configFp = ncFp }
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
