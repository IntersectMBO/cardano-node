{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Semigroup ((<>))
import qualified Options.Applicative as Opt

import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..))

import           Cardano.Config.Types
import           Cardano.Common.Parsers (nodeCliParser)
import           Cardano.Tracing.TraceAcceptor (runTraceAcceptor)

main :: IO ()
main = do
  nCli <- Opt.customExecParser pref opts
  (,) loggingLayer
      loggingFeature <- createLoggingFeature env nCli

  let cardanoApplication =
        CardanoApplication . liftIO $ runTraceAcceptor loggingLayer

  runCardanoApplicationWithFeatures [loggingFeature] cardanoApplication
  where
    env :: CardanoEnvironment
    env = NoEnvironment

    pref :: Opt.ParserPrefs
    pref = Opt.prefs Opt.showHelpOnEmpty

    opts :: Opt.ParserInfo NodeCLI
    opts =
      Opt.info (nodeCliParser <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "trace-acceptor - utility to support a variety of key\
          \ operations (genesis generation, migration,\
          \ pretty-printing..) for different system generations."
        )
