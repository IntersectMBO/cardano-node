
module Cardano.CLI.Shelley.Run
  ( runShelleyClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)

import           Cardano.CLI.Errors (CliError(..))
import           Cardano.CLI.Shelley.Parsers

import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Shelley.Run.Transaction
import           Cardano.CLI.Shelley.Run.Node
import           Cardano.CLI.Shelley.Run.Pool
import           Cardano.CLI.Shelley.Run.Query
                                         -- Block, System, DevOps
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.TextView


--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyClientCommand (AddressCmd      cmd) = runAddressCmd      cmd
runShelleyClientCommand (StakeAddressCmd cmd) = runStakeAddressCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = runNodeCmd         cmd
runShelleyClientCommand (PoolCmd         cmd) = runPoolCmd         cmd
runShelleyClientCommand (QueryCmd        cmd) = runQueryCmd        cmd
runShelleyClientCommand (BlockCmd        cmd) = runBlockCmd        cmd
runShelleyClientCommand (SystemCmd       cmd) = runSystemCmd       cmd
runShelleyClientCommand (DevOpsCmd       cmd) = runDevOpsCmd       cmd
runShelleyClientCommand (GenesisCmd      cmd) = runGenesisCmd      cmd
runShelleyClientCommand (TextViewCmd     cmd) = runTextViewCmd     cmd


--TODO: if you fill any of these in, move them into their own modules first!

runBlockCmd :: BlockCmd -> ExceptT CliError IO ()
runBlockCmd cmd = liftIO $ putStrLn $ "TODO: runBlockCmd: " ++ show cmd

runSystemCmd:: SystemCmd -> ExceptT CliError IO ()
runSystemCmd cmd = liftIO $ putStrLn $ "TODO: runSystemCmd: " ++ show cmd

runDevOpsCmd :: DevOpsCmd -> ExceptT CliError IO ()
runDevOpsCmd cmd = liftIO $ putStrLn $ "TODO: runDevOpsCmd: " ++ show cmd

