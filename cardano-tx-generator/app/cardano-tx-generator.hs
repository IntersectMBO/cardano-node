{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Monad.Trans.Except.Extra
                   ( runExceptT )
import           Options.Applicative
                   ( ParserInfo
                   , customExecParser, fullDesc, header
                   , helper, info, prefs, showHelpOnEmpty
                   )
import           System.Exit
                   (exitFailure)

import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
                   ( GenerateTxs
                   , parseCommand
                   )
import           Cardano.Benchmarking.GeneratorTx.CLI.Run
                   ( runCommand )

main :: IO ()
main = do
  generateTxs <- customExecParser (prefs showHelpOnEmpty) txGenInfo
  runExceptT (runCommand generateTxs) >>= \case
    Right _  -> pure ()
    Left err -> print err >> exitFailure
 where
  txGenInfo :: ParserInfo GenerateTxs
  txGenInfo =
    info (parseCommand <**> helper)
         (fullDesc <> header "cardano-tx-generator - the transaction generator for cardano node.")
