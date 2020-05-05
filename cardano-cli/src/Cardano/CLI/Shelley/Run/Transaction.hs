{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( runTransactionCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)


runTransactionCmd :: TransactionCmd -> ExceptT CliError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee out -> runTxBuildRaw txins txouts ttl fee out
    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw :: [TxIn] -> [TxOut] -> SlotNo -> Lovelace -> TxBodyFile -> ExceptT CliError IO ()
runTxBuildRaw txins txouts ttl amount (TxBodyFile fpath) =
  firstExceptT CardanoApiError
    . newExceptT
    . writeTxUnsigned fpath
    $ buildShelleyTransaction txins txouts ttl amount
