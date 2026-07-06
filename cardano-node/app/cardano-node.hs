{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Cardano.Configuration as CC
import qualified Cardano.Crypto.Init as Crypto
import           Cardano.Git.Rev (gitRev)
import           Cardano.Node.Handlers.TopLevel
import           Cardano.Node.Run (runNode)
import           Cardano.Node.Tracing.Documentation (TraceDocumentationCmd (..),
                   parseTraceDocumentationCmd, runTraceDocumentationCmd)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative
import qualified Options.Applicative as Opt
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
        Opt.info (fmap RunCmd nodeRunParser
                  <|> fmap TraceDocumentation parseTraceDocumentationCmd
                  <|> parseVersionCmd
                  <**> helper)

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

-- | The node's CLI, parsed by @cardano-config@, under the @run@ subcommand.
nodeRunParser :: Parser CC.CliArgs
nodeRunParser = Opt.subparser $ command' "run" "Run the node." CC.parseCliArgs

data Command = RunCmd CC.CliArgs
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
      , "\ngit rev ", $(gitRev)
      ]
  where
    renderVersion = Text.pack . showVersion


command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [ progDesc descr ])
    , metavar c
    ]
