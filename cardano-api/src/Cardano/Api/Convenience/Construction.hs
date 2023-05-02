-- | Convenience transaction construction functions
--
module Cardano.Api.Convenience.Construction (
    constructBalancedTx,

    -- * Misc
    TxInsExistError(..),
    ScriptLockedTxInsError(..),
    notScriptLockedTxIns,
    renderNotScriptLockedTxInsError,
    renderTxInsExistError,
    txInsExistInUTxO,

  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Prettyprinter as PP

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Fees
import           Cardano.Api.Pretty
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Tx
import           Cardano.Api.TxBody

-- | Construct a balanced transaction.
-- See Cardano.Api.Convenience.Query.queryStateForBalancedTx for a
-- convenient way of querying the node to get the required arguements
-- for constructBalancedTx.
constructBalancedTx
  :: IsShelleyBasedEra era
  => TxBodyContent BuildTx era
  -> AddressInEra era -- ^ Change address
  -> Maybe Word       -- ^ Override key witnesses
  -> UTxO era         -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> ProtocolParameters
  -> LedgerEpochInfo
  -> SystemStart
  -> Set PoolId       -- ^ The set of registered stake pools
  -> [ShelleyWitnessSigningKey]
  -> Either TxBodyErrorAutoBalance (Tx era)
constructBalancedTx txbodcontent changeAddr mOverrideWits utxo pparams
                    ledgerEpochInfo systemStart stakePools shelleyWitSigningKeys = do
  BalancedTxBody _ txbody _txBalanceOutput _fee
    <- makeTransactionBodyAutoBalance
         systemStart ledgerEpochInfo
         pparams stakePools utxo txbodcontent
         changeAddr mOverrideWits

  let keyWits = map (makeShelleyKeyWitness txbody) shelleyWitSigningKeys
  return $ makeSignedTransaction keyWits txbody

data TxInsExistError
  = TxInsDoNotExist [TxIn]
  | EmptyUTxO

renderTxInsExistError :: TxInsExistError -> Doc Ann
renderTxInsExistError EmptyUTxO =
  "The UTxO is empty"
renderTxInsExistError (TxInsDoNotExist txins) =
  PP.vsep
    [ "The following tx input(s) were not present in the UTxO:"
    , PP.indent 2 $ PP.vsep $ map pretty txins
    ]

txInsExistInUTxO :: [TxIn] -> UTxO era -> Either TxInsExistError ()
txInsExistInUTxO ins (UTxO utxo)
  | null utxo = Left EmptyUTxO
  | otherwise = do
      let utxoIns = Map.keys utxo
          occursInUtxo = [ txin | txin <- ins, txin `elem` utxoIns ]
      if length occursInUtxo == length ins
      then return ()
      else Left . TxInsDoNotExist $ ins List.\\ occursInUtxo

newtype ScriptLockedTxInsError = ScriptLockedTxIns [TxIn]

renderNotScriptLockedTxInsError :: ScriptLockedTxInsError -> Doc Ann
renderNotScriptLockedTxInsError (ScriptLockedTxIns txins) =
  "The followings tx inputs were expected to be key witnessed but are actually script witnessed: " <>
  PP.prettyList (map renderTxIn txins)

notScriptLockedTxIns :: [TxIn] -> UTxO era -> Either ScriptLockedTxInsError ()
notScriptLockedTxIns collTxIns (UTxO utxo) = do
  let onlyCollateralUTxOs = Map.restrictKeys utxo $ Set.fromList collTxIns
      scriptLockedTxIns =
        filter (\(_, TxOut aInEra _ _ _) -> not $ isKeyAddress aInEra ) $ Map.assocs onlyCollateralUTxOs
  if null scriptLockedTxIns
  then return ()
  else Left . ScriptLockedTxIns $ map fst scriptLockedTxIns


