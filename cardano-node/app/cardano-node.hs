{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import qualified Cardano.Crypto.Init as Crypto
import           Cardano.Git.Rev (gitRev)
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration)
import           Cardano.Node.Handlers.TopLevel
import           Cardano.Node.Parsers (nodeCLIParser, parserHelpHeader, parserHelpOptions,
                   renderHelpDoc)
import           Cardano.Node.Run (runNode)
import           Cardano.Node.Tracing.Documentation (TraceDocumentationCmd (..),
                   parseTraceDocumentationCmd, runTraceDocumentationCmd)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative
import qualified Options.Applicative as Opt
import           Options.Applicative.Help ((<$$>))
import           System.Info (arch, compilerName, compilerVersion, os)

import           Paths_cardano_node (version)

main :: IO ()
main = do
  Crypto.cryptoInit

  toplevelExceptionHandler $ do
    cmd <- Opt.customExecParser p opts

    case cmd of
      RunCmd args -> runNode args
      TraceDocumentation tdc -> runTraceDocumentationCmd tdc
      VersionCmd  -> runVersionCommand

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      opts :: Opt.ParserInfo Command
      opts =
        Opt.info (fmap RunCmd nodeCLIParser
                  <|> fmap TraceDocumentation parseTraceDocumentationCmd
                  <|> parseVersionCmd
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


data Command = RunCmd PartialNodeConfiguration
             | TraceDocumentation TraceDocumentationCmd
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
    Text.putStrLn $ mconcat
      [ "cardano-node ", renderVersion version
      , " - ", Text.pack os, "-", Text.pack arch
      , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
      , "\ngit rev ", gitRev
      ]
  where
    renderVersion = Text.pack . showVersion


command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [ progDesc descr ])
    , metavar c
    ]
