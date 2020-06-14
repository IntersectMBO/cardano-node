{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)
import qualified Data.Text as Text

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

import           Data.Version (showVersion)
import           Paths_cardano_node (version)
import           System.Info (arch, compilerName, compilerVersion, os)
import           Cardano.Config.GitRev (gitRev)

import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..))

import           Cardano.Common.Help
import           Cardano.Config.TopHandler
import           Cardano.Config.Parsers
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Config.Types
import           Cardano.Node.Features.Node

main :: IO ()
main = toplevelExceptionHandler $ do

    cmd <- Opt.customExecParser p opts

    case cmd of
      RunCmd args -> runRunCommand args
      VersionCmd  -> runVersionCommand

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      opts :: Opt.ParserInfo Command
      opts =
        Opt.info (fmap RunCmd nodeCLIParser <|> parseVersionCmd
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
        parserHelpHeader "cardano-node" nodeCLIParser
        <$$> ""
        <$$> parserHelpOptions nodeCLIParser


data Command = RunCmd NodeCLI
             | VersionCmd

-- Yes! A --version flag or version command. Either guess is right!
parseVersionCmd :: Parser Command
parseVersionCmd =
      Opt.subparser
        (mconcat
         [ Opt.commandGroup "Miscellaneous commands"
         , Opt.metavar "version"
         , Opt.hidden
         , command'
           "version"
           "Show the cardano-node version"
           (pure VersionCmd)
         ]
        )
  <|> Opt.flag' VersionCmd
        (  Opt.long "version"
        <> Opt.help "Show the cardano-node version"
        <> Opt.hidden
        )

runVersionCommand :: IO ()
runVersionCommand =
    putTextLn $ mconcat
      [ "cardano-node ", renderVersion version
      , " - ", Text.pack os, "-", Text.pack arch
      , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
      , "\ngit rev ", gitRev
      ]
  where
    renderVersion = Text.pack . showVersion


runRunCommand :: NodeCLI -> IO ()
runRunCommand cli = do
    (features, nodeLayer) <- initializeAllFeatures cli env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode


initializeAllFeatures
  :: NodeCLI
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures npm cardanoEnvironment = do

  eitherFeatures <- runExceptT $ createLoggingFeature
                      (Text.pack (showVersion version))
                      cardanoEnvironment
                      npm

  (loggingLayer, loggingFeature) <- case eitherFeatures of
                                      Left err -> putTextLn (show err) >> exitFailure
                                      Right res -> return res

  (nodeLayer, nodeFeature) <- createNodeFeature
                                loggingLayer
                                cardanoEnvironment
                                npm

  pure ([loggingFeature, nodeFeature] :: [CardanoFeature], nodeLayer)
