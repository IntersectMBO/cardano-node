{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( runTransactionCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api hiding (readSigningKey)
import           Cardano.Config.Shelley.ColdKeys (KeyRole (..), readSigningKey)
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)


runTransactionCmd :: TransactionCmd -> ExceptT CliError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee out ->
      runTxBuildRaw txins txouts ttl fee out
    TxSign txinfile skfiles mNetwork txoutfile ->
      runTxSign txinfile skfiles (maybe Mainnet Testnet mNetwork) txoutfile

    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw :: [TxIn] -> [TxOut] -> SlotNo -> Lovelace -> TxBodyFile -> ExceptT CliError IO ()
runTxBuildRaw txins txouts ttl amount (TxBodyFile fpath) =
  firstExceptT CardanoApiError
    . newExceptT
    . writeTxUnsigned fpath
    $ buildShelleyTransaction txins txouts ttl amount


runTxSign :: TxBodyFile -> [SigningKeyFile] -> Network -> TxFile -> ExceptT CliError IO ()
runTxSign (TxBodyFile infile) skfiles  network (TxFile outfile) = do
    txu <- firstExceptT CardanoApiError . newExceptT $ readTxUnsigned infile
    sks <- readSigningKeyFiles skfiles
    firstExceptT CardanoApiError
      . newExceptT
      . writeTxSigned outfile
      $ signTransaction txu network sks


-- TODO : This is nuts. The 'cardano-api' and 'cardano-config' packages both have functions
-- for reading/writing keys, but they are incompatible.
-- The 'config' version just operates on Shelley only 'SignKey's, but 'api' operates on
-- 'SigningKey's which have a Byron and a Shelley constructor.
readSigningKeyFiles :: [SigningKeyFile] -> ExceptT CliError IO [SigningKey]
readSigningKeyFiles files =
  newExceptT $ do
    xs <- mapM (runExceptT . readSigningKey GenesisUTxOKey . unSigningKeyFile) files
    case partitionEithers xs of
      (e:_, _) -> pure $ Left (KeyCliError e)
      ([], ys) -> pure $ Right (map SigningKeyShelley ys)
