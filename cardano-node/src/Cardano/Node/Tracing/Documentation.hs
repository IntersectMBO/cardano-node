{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Tracing.Documentation
  ( TraceDocumentationCmd (..)
  , parseTraceDocumentationCmd
  , runTraceDocumentationCmd
  , docTracers
  , DocRun (..)
  , docTracersFirstPhase
  ) where

import           Cardano.Git.Rev (gitRev)
import           Cardano.Node.Orphans ()
import           Cardano.Node.Queries ()
import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Registry
import           Cardano.Node.Tracing.Tracers (buildNodeTracers)
import           Cardano.Node.Tracing.Tracers.HasIssuer ()
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import           Control.Monad (forM_, void)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as T
import           Data.Time (getZonedTime)
import           Data.Version (showVersion)
import qualified Options.Applicative as Opt
import           System.IO

import           Hermod.Tracing
import           Hermod.Tracing.DocuGenerator (DocTracer, docTracer, docTracerDatapoint,
                   docuResultsToMetricsHelptext, docuResultsToNamespaces, docuResultsToText)
import           Paths_cardano_node (version)


data TraceDocumentationCmd
  = TraceDocumentationCmd
    { tdcConfigFile   :: FilePath
    , tdcOutput       :: FilePath
    , tdMetricsHelp   :: Maybe FilePath
    , tdNamespaceList :: Maybe FilePath
    }

parseTraceDocumentationCmd :: Opt.Parser TraceDocumentationCmd
parseTraceDocumentationCmd =
  Opt.subparser
    (mconcat
     [ Opt.commandGroup "Miscellaneous commands"
     , Opt.metavar "trace-documentation"
     , Opt.hidden
     , Opt.command "trace-documentation" $
       Opt.info
         (TraceDocumentationCmd
           <$> Opt.strOption
               ( Opt.long "config"
                 <> Opt.metavar "FILE"
                 <> Opt.help "Configuration file for the cardano-node"
               )
           <*> Opt.strOption
               ( Opt.long "output-file"
                 <> Opt.metavar "FILE"
                 <> Opt.help "Generated documentation output file (Markdown)"
               )
           <*> Opt.optional (Opt.strOption
                ( Opt.long "output-metric-help"
                  <> Opt.metavar "FILE"
                  <> Opt.help "Metrics helptext file for cardano-tracer (JSON)"
                )
              )
           <*> Opt.optional (Opt.strOption
                ( Opt.long "output-namespace-list"
                  <> Opt.metavar "FILE"
                  <> Opt.help "Namespace list file (text)"
                )
              )
           Opt.<**> Opt.helper)
       $ mconcat [ Opt.progDesc "Generate the trace documentation" ]
     ]
    )

runTraceDocumentationCmd
  :: TraceDocumentationCmd
  -> IO ()
runTraceDocumentationCmd TraceDocumentationCmd{..} = do
  docTracers tdcConfigFile tdcOutput tdMetricsHelp tdNamespaceList

docTracers ::
  FilePath
  -> FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> IO ()
docTracers configFileName outputFileName mbMetricsHelpFilename mbNamespaceList = do
    run <- docTracersFirstPhase (Just configFileName)
    docTracersSecondPhase outputFileName mbMetricsHelpFilename mbNamespaceList run

-- | The documentation of every trace the node declares.
data DocRun = DocRun
  { drDocTracer  :: DocTracer
  , drNamespaces :: [([Text], [Text])]
    -- ^ The namespaces the configuration consistency check knows.
  , drConfig     :: TraceConfig
  }

-- | Build the node's tracers exactly as the node does, with backends that only
-- answer documentation requests, then configure and document them.
docTracersFirstPhase :: Maybe FilePath -> IO DocRun
docTracersFirstPhase condConfigFileName = do
    trConfig <- case condConfigFileName of
                  Just fn -> readConfigurationWithDefault (FromFile fn) defaultCardanoConfig
                  Nothing -> pure defaultCardanoConfig
    registry <- newRegistry ForDocumentation Backends
      { bkStdout    = docTracer (Stdout MachineFormat)
      , bkForward   = docTracer Forwarder
      , bkEKG       = Just (docTracer EKGBackend)
      , bkDataPoint = docTracerDatapoint DatapointBackend
      }
    void (buildNodeTracers @(CardanoBlock StandardCrypto) registry)
    entries <- registered registry
    configReflection <- emptyConfigReflection
    configureAll configReflection trConfig entries
    docs <- documentAll entries
    pure DocRun
      { drDocTracer  = docs
      , drNamespaces = namespacesOf entries
      , drConfig     = trConfig
      }

docTracersSecondPhase ::
     FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> DocRun
  -> IO ()
docTracersSecondPhase outputFileName mbMetricsHelpFilename mbNamespaceList DocRun{..} = do
    let text = docuResultsToText drDocTracer drConfig
    time <- getZonedTime
    let stamp = "Generated at "
             <> pack (show time)
             <> ", git commit hash "
             <> $(gitRev)
             <> ", node version "
             <> pack (showVersion version) <> "\n"
    doWrite outputFileName (text <> stamp)
    forM_ mbMetricsHelpFilename $ \f ->
       doWrite f (docuResultsToMetricsHelptext drDocTracer)
    forM_ mbNamespaceList $ \f ->
       doWrite f (docuResultsToNamespaces drDocTracer)
  where
    doWrite outfile text =
      withFile outfile WriteMode $ \handle ->
        hSetEncoding handle utf8 >> T.hPutStr handle text
