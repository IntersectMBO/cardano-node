{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Options.Applicative as Opt
import           Options.Applicative (Parser, ParserInfo, ParserPrefs,
                                      showHelpOnEmpty)
import           System.Exit (exitFailure)

import           Cardano.CLI.Parsers
import           Cardano.CLI.Run
import           Cardano.Common.TopHandler
import           Cardano.Common.Parsers
import           Cardano.Config.Logging (createLoggingFeatureCLI)
import           Cardano.Config.Protocol (Protocol)
import           Cardano.Config.Types (CardanoEnvironment (..))
import           Cardano.Crypto (RequiresNetworkMagic(..))
import           Cardano.Shell.Types (CardanoFeature (..))
import qualified Ouroboros.Consensus.BlockchainTime as Consensus

main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts
  -- Initialize logging layer. Particularly, we need it for benchmarking (command 'generate-txs').
  let cardanoConfiguration :: PartialCardanoConfiguration
      cardanoConfiguration = mainnetConfiguration
      cardanoEnvironment :: CardanoEnvironment
      cardanoEnvironment = NoEnvironment

  cmdRes <- runExceptT . runCommand $ mainCommand co

  case cmdRes of
    Right _ -> pure ()
    Left err -> do putStrLn $ renderCliError err
                   exitFailure
  where
    pref :: ParserPrefs
    pref = Opt.prefs showHelpOnEmpty

    opts :: ParserInfo CLI
    opts =
      Opt.info (parseClientCommand <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "cardano-cli - utility to support a variety of key\
          \ operations (genesis generation, migration,\
          \ pretty-printing..) for different system generations."
        )

    renderCliError :: CliError -> String
    renderCliError = show

data CLI = CLI { mainCommand :: ClientCommand }

parseClientCommand :: Parser ClientCommand
parseClientCommand =
  CLI <$> parseGenesisRelatedValues
      <|> parseKeyRelatedValues
      <|> parseDelegationRelatedValues
      <|> parseTxRelatedValues
