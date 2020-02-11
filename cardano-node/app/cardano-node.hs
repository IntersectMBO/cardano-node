{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)
import           Data.Text (pack)

import           Data.Semigroup ((<>))
import           Data.Version (showVersion)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import           Paths_cardano_node (version)

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

    cli <- Opt.customExecParser p opts

    (features, nodeLayer) <- initializeAllFeatures cli env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      opts :: Opt.ParserInfo NodeProtocolMode
      opts =
        Opt.info (nodeProtocolModeParser
                    <**> helperBrief "help" "Show this help text" nodeCliHelpMain)

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

      helperBrief :: String -> String -> String -> Parser (a -> a)
      helperBrief l d helpText = Opt.abortOption (Opt.InfoMsg helpText) $ mconcat
        [ Opt.long l
        , Opt.help d ]

      nodeCliHelpMain :: String
      nodeCliHelpMain = renderHelpDoc 80 $
        parserHelpHeader "cardano-node" nodeProtocolModeParser
        <$$> ""
        <$$> parserHelpOptions nodeProtocolModeParser


initializeAllFeatures
  :: NodeProtocolMode
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures npm cardanoEnvironment = do
  (loggingLayer, loggingFeature) <- createLoggingFeature
                                      (pack $ showVersion version)
                                      cardanoEnvironment
                                      npm
  (nodeLayer   , nodeFeature)    <- createNodeFeature
                                      loggingLayer
                                      cardanoEnvironment
                                      npm

  pure ([loggingFeature, nodeFeature] :: [CardanoFeature], nodeLayer)
