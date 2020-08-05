module Cardano.CLI.Shelley.Run
  ( ShelleyClientCmdError
  , renderShelleyClientCmdError
  , runShelleyClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.CLI.Shelley.Parsers

import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.Governance
import           Cardano.CLI.Shelley.Run.Key
import           Cardano.CLI.Shelley.Run.Node
import           Cardano.CLI.Shelley.Run.Pool
import           Cardano.CLI.Shelley.Run.Query
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Shelley.Run.Transaction
                                         -- Block, System, DevOps
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.TextView

data ShelleyClientCmdError
  = ShelleyCmdAddressError !ShelleyAddressCmdError
  | ShelleyCmdGenesisError !ShelleyGenesisCmdError
  | ShelleyCmdGovernanceError !ShelleyGovernanceCmdError
  | ShelleyCmdNodeError !ShelleyNodeCmdError
  | ShelleyCmdPoolError !ShelleyPoolCmdError
  | ShelleyCmdStakeAddressError !ShelleyStakeAddressCmdError
  | ShelleyCmdTextViewError !ShelleyTextViewFileError
  | ShelleyCmdTransactionError !ShelleyTxCmdError
  | ShelleyCmdQueryError !ShelleyQueryCmdError
  | ShelleyCmdKeyError !ShelleyKeyCmdError
  deriving Show

-- Identity monad is used to avoid boilerplate
renderShelleyClientCmdError :: ShelleyCommand -> ShelleyClientCmdError -> Identity Text
renderShelleyClientCmdError cmd err = do
  cmdError <- case err of
                ShelleyCmdAddressError addrCmdErr ->
                  return $ renderShelleyAddressCmdError addrCmdErr
                ShelleyCmdGenesisError genesisCmdErr ->
                  return $ renderShelleyGenesisCmdError genesisCmdErr
                ShelleyCmdGovernanceError govCmdErr ->
                  return $ renderShelleyGovernanceError govCmdErr
                ShelleyCmdNodeError nodeCmdErr ->
                  return $ renderShelleyNodeCmdError nodeCmdErr
                ShelleyCmdPoolError poolCmdErr ->
                  return $ renderShelleyPoolCmdError poolCmdErr
                ShelleyCmdStakeAddressError stakeAddrCmdErr ->
                  return $ renderShelleyStakeAddressCmdError stakeAddrCmdErr
                ShelleyCmdTextViewError txtViewErr ->
                  return $ renderShelleyTextViewFileError txtViewErr
                ShelleyCmdTransactionError txErr ->
                  return $ renderShelleyTxCmdError txErr
                ShelleyCmdQueryError queryErr ->
                  return $ renderShelleyQueryCmdError queryErr
                ShelleyCmdKeyError keyErr ->
                  return $ renderShelleyKeyCmdError keyErr

  return $ "Shelley command failed: "
         <> renderShelleyCommand cmd
         <> "  Error: "
         <> cmdError


--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT ShelleyClientCmdError IO ()
runShelleyClientCommand (AddressCmd      cmd) = firstExceptT ShelleyCmdAddressError $ runAddressCmd cmd
runShelleyClientCommand (StakeAddressCmd cmd) = firstExceptT ShelleyCmdStakeAddressError $ runStakeAddressCmd cmd
runShelleyClientCommand (KeyCmd          cmd) = firstExceptT ShelleyCmdKeyError $ runKeyCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = firstExceptT ShelleyCmdTransactionError $ runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = firstExceptT ShelleyCmdNodeError $ runNodeCmd cmd
runShelleyClientCommand (PoolCmd         cmd) = firstExceptT ShelleyCmdPoolError $ runPoolCmd cmd
runShelleyClientCommand (QueryCmd        cmd) = firstExceptT ShelleyCmdQueryError $ runQueryCmd cmd
runShelleyClientCommand (GovernanceCmd   cmd) = firstExceptT ShelleyCmdGovernanceError $ runGovernanceCmd cmd
runShelleyClientCommand (GenesisCmd      cmd) = firstExceptT ShelleyCmdGenesisError $ runGenesisCmd cmd
runShelleyClientCommand (TextViewCmd     cmd) = firstExceptT ShelleyCmdTextViewError $ runTextViewCmd cmd
