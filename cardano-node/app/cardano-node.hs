{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

import qualified Cardano.Crypto.Init as Crypto
import           Cardano.Git.Rev (gitRev)
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration(..))
import           Cardano.Node.Handlers.TopLevel
import           Cardano.Node.Parsers (nodeCLIParser, parserHelpHeader, parserHelpOptions,
                   renderHelpDoc)
import           Cardano.Node.Run (runNode)
import           Cardano.Node.Tracing.Documentation (TraceDocumentationCmd (..),
                   parseTraceDocumentationCmd, runTraceDocumentationCmd)

import           Data.Monoid (Last (getLast))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative
import qualified Options.Applicative as Opt
import           Options.Applicative.Help ((<$$>))
import           System.Info (arch, compilerName, compilerVersion, os)
import           System.IO (hPutStrLn, stderr)

import           Paths_cardano_node (version)

import GHC.Conc

main :: IO ()
main = do
  Crypto.cryptoInit

  myThreadId >>= flip labelThread "main"

  toplevelExceptionHandler $ do
    cmd <- Opt.customExecParser p opts

    case cmd of
      RunCmd args -> do
        warnIfSet args pncMaybeMempoolCapacityOverride "mempool-capacity-override" "MempoolCapacityBytesOverride"
        warnIfSet args pncNumOfDiskSnapshots "num-of-disk-snapshots" "NumOfDiskSnapshots"
        warnIfSet args pncSnapshotInterval "snapshot-interval" "SnapshotInterval"
        runNode args
      TraceDocumentation tdc -> runTraceDocumentationCmd tdc
      VersionCmd  -> runVersionCommand

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      warnIfSet :: PartialNodeConfiguration -> (PartialNodeConfiguration -> Last a) -> String -> String -> IO ()
      warnIfSet args f name key =
          maybe
            (pure ())
            (\_ -> hPutStrLn stderr $ "WARNING: Option --" ++ name ++ " was set via CLI flags.\
            \ This CLI flag will be removed in upcoming node releases.\
            \ Please, set this configuration option in the configuration file instead with key " ++ key ++ ".")
        $ getLast
        $ f args

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
