{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Analyze.CLI
  ( parseAnalyzerParams
  ) where

import           Cardano.Tracer.Analyze.Types

import           Options.Applicative

parseAnalyzerParams :: Parser AnalyzerParams
parseAnalyzerParams = AnalyzerParams <$>
    strOption (
         long "trace1"
      <> short '1'
      <> metavar "FILEPATH"
      <> help "First file with trace messages"
      <> completer (bashCompleter "file")
      )
    <*> strOption (
         long "trace2"
      <> short '2'
      <> metavar "FILEPATH"
      <> help "Second file with trace messages"
      <> completer (bashCompleter "file")
      )
    <*> switch (
         long "isHuman"
      <> short 'u'
      <> help "Analyze in human format."
      )
