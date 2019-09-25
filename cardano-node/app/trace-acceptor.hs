{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Logging (LoggingCLIArguments (..),
                                                createLoggingFeature
                                                )
import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Partial
import           Cardano.Config.Types
import           Cardano.Common.Parsers
import           Cardano.Tracing.TraceAcceptor (runTraceAcceptor)

main :: IO ()
main = do
  (,,) commonCLI
       commonCLIAdv
       loggingCLI <- Opt.customExecParser pref opts
  finalConfig <- case mkConfiguration pcc commonCLI commonCLIAdv of
                   Left err -> throwIO err
                   Right x -> pure x
  (,) loggingLayer
      loggingFeature <- createLoggingFeature env finalConfig loggingCLI

  let cardanoApplication =
        CardanoApplication . liftIO $ runTraceAcceptor loggingLayer
        
  runCardanoApplicationWithFeatures [loggingFeature] cardanoApplication
  where
    pcc :: PartialCardanoConfiguration
    pcc = mainnetConfiguration

    env :: CardanoEnvironment
    env = NoEnvironment

    pref :: Opt.ParserPrefs
    pref = Opt.prefs Opt.showHelpOnEmpty

    opts :: Opt.ParserInfo (CommonCLI, CommonCLIAdvanced, LoggingCLIArguments)
    opts =
      Opt.info (cliParser <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "trace-acceptor - utility to support a variety of key\
          \ operations (genesis generation, migration,\
          \ pretty-printing..) for different system generations."
        )

    cliParser :: Parser (CommonCLI, CommonCLIAdvanced, LoggingCLIArguments)
    cliParser = (,,)
      <$> parseCommonCLI
      <*> parseCommonCLIAdvanced
      <*> loggingParser
