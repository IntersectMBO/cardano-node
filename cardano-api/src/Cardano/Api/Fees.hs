{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Fee calculation
--
module Cardano.Api.Fees (
    transactionFee,
    estimateTransactionFee,
    
    makeTransactionBodyAutoBalance,
    evaluateTransactionExecutionUnits,
    evaluateTransactionFee,
    evaluateTransactionBalance,
  ) where

import           Prelude

import qualified Data.ByteString as BS
import           Data.Bifunctor (bimap, first)
import qualified Data.Array as Array
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Records (HasField (..))
import           Numeric.Natural
import           Data.Sequence.Strict (StrictSeq)

import           Control.Monad.Trans.Except

import qualified Cardano.Binary as CBOR
import           Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)

import qualified Cardano.Chain.Common as Byron

import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era  as Ledger
import qualified Cardano.Ledger.Era  as Ledger.Era (Crypto)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Crypto as Ledger

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Script
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.Value

-- Temporarily, until they're moved to the ledger API:
import           Cardano.Ledger.Alonzo.PParams (PParams'(..))
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Ledger
                   (evaluateTransactionExecutionUnits,
                    evaluateTransactionBalance,
                    evaluateTransactionFee,
                  --evaluateMinLovelaceOutput, --TODO use this
                    ScriptFailure)
import           Shelley.Spec.Ledger.PParams (PParams'(..))

import qualified Shelley.Spec.Ledger.TxBody as Shelley

import qualified Cardano.Ledger.Mary.Value as Mary

import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo

import qualified Ouroboros.Consensus.HardFork.History as Consensus


-- ----------------------------------------------------------------------------
-- Transaction fees
--

-- | For a concrete fully-constructed transaction, determine the minimum fee
-- that it needs to pay.
--
-- This function is simple, but if you are doing input selection then you
-- probably want to consider estimateTransactionFee.
--
transactionFee :: forall era.
                  IsShelleyBasedEra era
               => Natural -- ^ The fixed tx fee
               -> Natural -- ^ The tx fee per byte
               -> Tx era
               -> Lovelace
transactionFee txFeeFixed txFeePerByte (ShelleyTx _ tx) =
    Lovelace (a * sz + b)
  where
    a  = toInteger txFeePerByte
    b  = toInteger txFeeFixed
    sz = getField @"txsize" tx

--TODO: This could be made to work for Byron txs too, but in the Byron case
-- the per-byte is non-integral. We would need different parameters. e.g. a
-- new data type for fee params, Byron vs Shelley
transactionFee _ _ (ByronTx _) =
    case shelleyBasedEra :: ShelleyBasedEra ByronEra of {}


-- | This can estimate what the transaction fee will be, based on a starting
-- base transaction, plus the numbers of the additional components of the
-- transaction that may be added.
--
-- So for example with wallet coin selection, the base transaction should
-- contain all the things not subject to coin selection (such as script inputs,
-- metadata, withdrawals, certs etc)
--
estimateTransactionFee :: forall era.
                          IsShelleyBasedEra era
                       => NetworkId
                       -> Natural -- ^ The fixed tx fee
                       -> Natural -- ^ The tx fee per byte
                       -> Tx era
                       -> Int -- ^ The number of extra UTxO transaction inputs
                       -> Int -- ^ The number of extra transaction outputs
                       -> Int -- ^ The number of extra Shelley key witnesses
                       -> Int -- ^ The number of extra Byron key witnesses
                       -> Lovelace
estimateTransactionFee nw txFeeFixed txFeePerByte (ShelleyTx era tx) =
    let Lovelace baseFee = transactionFee txFeeFixed txFeePerByte (ShelleyTx era tx)
     in \nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->

        --TODO: this is fragile. Move something like this to the ledger and
        -- make it robust, based on the txsize calculation.
        let extraBytes :: Int
            extraBytes = nInputs               * sizeInput
                       + nOutputs              * sizeOutput
                       + nByronKeyWitnesses    * sizeByronKeyWitnesses
                       + nShelleyKeyWitnesses  * sizeShelleyKeyWitnesses

         in Lovelace (baseFee + toInteger txFeePerByte * toInteger extraBytes)
  where
    sizeInput               = smallArray + uint + hashObj
    sizeOutput              = smallArray + uint + address
    sizeByronKeyWitnesses   = smallArray + keyObj + sigObj + ccodeObj + attrsObj
    sizeShelleyKeyWitnesses = smallArray + keyObj + sigObj

    smallArray  = 1
    uint        = 5

    hashObj     = 2 + hashLen
    hashLen     = 32

    keyObj      = 2 + keyLen
    keyLen      = 32

    sigObj      = 2 + sigLen
    sigLen      = 64

    ccodeObj    = 2 + ccodeLen
    ccodeLen    = 32

    address     = 2 + addrHeader + 2 * addrHashLen
    addrHeader  = 1
    addrHashLen = 28

    attrsObj    = 2 + BS.length attributes
    attributes  = CBOR.serialize' $
                    Byron.mkAttributes Byron.AddrAttributes {
                      Byron.aaVKDerivationPath = Nothing,
                      Byron.aaNetworkMagic     = toByronNetworkMagic nw
                    }

--TODO: This can be made to work for Byron txs too. Do that: fill in this case
-- and remove the IsShelleyBasedEra constraint.
estimateTransactionFee _ _ _ (ByronTx _) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}


-- ----------------------------------------------------------------------------
-- Script execution units
--


-- Steps:
-- 1. evaluate all the scripts to get the exec units, update with ex units
-- 2. figure out the overall min fees
-- 3. update tx with fees
-- 4. balance the transaction and update tx change output

makeTransactionBodyAutoBalance :: IsShelleyBasedEra era   --TODO eliminate
                               => SystemStart
                               -> EraHistory era
                               -> ProtocolParameters
                               -> Set PoolId
                               -> UTxO era
                               -> TxBodyContent BuildTx era
                               -> AddressInEra era
                               -> Either () (TxBody era)
makeTransactionBodyAutoBalance systemstart history pparams
                            poolids utxo txbodycontent changeaddr = do
    txbody0 <- first (const ()) $
               makeTransactionBody txbodycontent

    exUnitsMap <- first (const ()) $
                  evaluateTransactionExecutionUnits
                    systemstart history
                    pparams utxo
                    txbody0
    exUnitsMap' <- traverse (first (const ())) exUnitsMap

    let txbodycontent1 = substituteExecutionUnits exUnitsMap' txbodycontent
    txbody1 <- first (const ()) $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee  = TxFeeExplicit undefined 0,
                 txOuts = TxOut changeaddr
                                (lovelaceToTxOutValue 0)
                                TxOutDatumHashNone
                        : txOuts txbodycontent
                 --TODO: think about the size of the change output
                 -- 1,2,4 or 8 bytes?
               }

    let fee = evaluateTransactionFee pparams txbody1
    txbody2 <- first (const ()) $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee = TxFeeExplicit undefined fee
               }

    let balance = evaluateTransactionBalance pparams poolids utxo txbody2
    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    case balance of
      TxOutAdaOnly _ _ -> return ()
        --TODO: do the same negative and minUTxOValue checks
      TxOutValue _ v   ->
        case valueToLovelace v of
          Nothing -> Left () -- TODO: non-ada assets not balanced
          Just c
            | c < 0 -> Left () --TODO: not enough inputs to cover outputs
              -- check the change is over the min utxo threshold
            | c < minUTxOValue -> Left () --TODO: not enough inputs to cover change min utxo
            | otherwise -> return ()
            where
              minUTxOValue = undefined

    --TODO: we could add the extra fee for the CBOR encoding of the change,
    -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.

    txbody3 <- first (const ()) $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent {
                 txFee  = TxFeeExplicit undefined fee,
                 txOuts = TxOut changeaddr balance TxOutDatumHashNone
                        : txOuts txbodycontent
               }

    return txbody3


substituteExecutionUnits :: Map ScriptWitnessIndex ExecutionUnits
                         -> TxBodyContent BuildTx era
                         -> TxBodyContent BuildTx era
substituteExecutionUnits exUnitsMap =
    mapTxScriptWitnesses f
  where
    f :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> ScriptWitness witctx era
    f _   wit@SimpleScriptWitness{} = wit
    f idx wit@(PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing      -> wit
        Just exunits -> PlutusScriptWitness langInEra version script
                                            datum redeemer exunits


data ScriptFailure = ScriptFailure
data PastHorizonException = PastHorizonException

-- | Run all the scripts in a transaction and return the execution units needed
-- for each use of each script. The total execution units for the transaction
-- is the sum of these.
--
evaluateTransactionExecutionUnits
  :: forall era.
     SystemStart
  -> EraHistory era
  -> ProtocolParameters
  -> UTxO era
  -> TxBody era
  -> Either PastHorizonException --TODO: wrap error
            (Map ScriptWitnessIndex (Either ScriptFailure ExecutionUnits))
evaluateTransactionExecutionUnits systemstart history pparams utxo txbody =
    case makeSignedTransaction [] txbody of
      ByronTx {}                 -> Right Map.empty
      ShelleyTx era tx' ->
        case era of
          ShelleyBasedEraShelley -> Right Map.empty
          ShelleyBasedEraAllegra -> Right Map.empty
          ShelleyBasedEraMary    -> Right Map.empty
          ShelleyBasedEraAlonzo  -> evalAlonzo era tx'

  where
    evalAlonzo :: forall ledgerera.
                  ShelleyLedgerEra era ~ ledgerera
               => Ledger.TxBody    ledgerera ~ Alonzo.TxBody ledgerera
               => Ledger.Witnesses ledgerera ~ Alonzo.TxWitness ledgerera
               => LedgerEraConstraints ledgerera
               => LedgerAlonzoConstraints ledgerera
               => ShelleyBasedEra era
               -> Ledger.Tx ledgerera
               -> Either PastHorizonException --TODO: wrap error
                         (Map ScriptWitnessIndex
                              (Either ScriptFailure ExecutionUnits))
    evalAlonzo era tx =
      case Ledger.evaluateTransactionExecutionUnits
             tx
             (toLedgerUTxO era utxo)
             (toLedgerEpochInfo history)
             systemstart
             (toAlonzoCostModels (protocolParamCostModels pparams))
        of Left  _     -> Left PastHorizonException
           Right exmap -> Right (fromLedgerScriptExUnitsMap exmap)

    toLedgerEpochInfo :: EraHistory era -> EpochInfo (Either PastHorizonException)
    toLedgerEpochInfo (EraHistory _ interpreter) =
        hoistEpochInfo (first fromConsensusPastHorizonException . runExcept) $
          Consensus.interpreterToEpochInfo interpreter

    toAlonzoCostModels :: Map AnyPlutusScriptVersion CostModel
                       -> Array.Array Alonzo.Language Alonzo.CostModel
    toAlonzoCostModels costmodels =
      Array.array
        (minBound, maxBound)
        [ (toAlonzoLanguage lang, toAlonzoCostModel costmodel)
        | (lang, costmodel) <- Map.toList costmodels ]

    fromConsensusPastHorizonException :: Consensus.PastHorizonException
                                      -> PastHorizonException
    fromConsensusPastHorizonException _ = PastHorizonException --TODO

    fromLedgerScriptFailure :: Ledger.ScriptFailure Ledger.StandardCrypto
                            -> ScriptFailure
    fromLedgerScriptFailure _ = ScriptFailure --TODO

    fromLedgerScriptExUnitsMap
      :: Map Alonzo.RdmrPtr (Either (Ledger.ScriptFailure Ledger.StandardCrypto)
                                    Alonzo.ExUnits)
      -> Map ScriptWitnessIndex (Either ScriptFailure ExecutionUnits)
    fromLedgerScriptExUnitsMap exmap =
      Map.fromList
        [ (fromAlonzoRdmrPtr rdmrptr,
           bimap fromLedgerScriptFailure fromAlonzoExUnits exunitsOrFailure)
        | (rdmrptr, exunitsOrFailure) <- Map.toList exmap ]

evaluateTransactionFee :: forall era ledgerera.
                          ShelleyLedgerEra era ~ ledgerera
                       => IsShelleyBasedEra era
                       => ProtocolParameters
                       -> TxBody era
                       -> Lovelace
evaluateTransactionFee pparams txbody =
    case makeSignedTransaction [] txbody of
      ByronTx{} -> case shelleyBasedEra :: ShelleyBasedEra era of {}
      --TODO: we could actually support Byron here, it'd be different but simpler

      ShelleyTx era tx' ->
        case era of
          ShelleyBasedEraAlonzo ->
            evalAlonzo era (toTxInBlock tx')
          _ -> error "TODO: evaluateTransactionFee can support pre-Alonzo eras with a different (simpler) code path"
  where
    numberOfKeyWitnesses :: Word
    numberOfKeyWitnesses = error "TODO: evaluateTransactionFee need to compute the number of key witnesses we need, this may require more help from the ledger to find all the places that need it"

    --TODO: this conversion can be eliminted once the ledger function is adjusted
    -- to take a Ledger.Tx rather than a Ledger.TxInBlock
    toTxInBlock :: Ledger.Tx ledgerera -> Ledger.TxInBlock ledgerera
    toTxInBlock = error "TODO: Ledger.evaluateTransactionFee is going to be modified so that it takes a Tx not a TxInBlock"

    evalAlonzo :: forall tx.
                  Ledger.TxInBlock ledgerera ~ tx ledgerera --TODO: this can be simplified
               => Ledger.Witnesses ledgerera ~ Alonzo.TxWitness ledgerera
               => LedgerEraConstraints ledgerera
               => LedgerPParamsConstraints ledgerera
               => LedgerAlonzoConstraints ledgerera
               => ShelleyBasedEra era
               -> Ledger.TxInBlock ledgerera
               -> Lovelace
    evalAlonzo era tx =
      fromShelleyLovelace $
        Ledger.evaluateTransactionFee
          (toLedgerPParams era pparams)
          tx
          numberOfKeyWitnesses

--data TxBalanceError = TxBalanceErrorMissingTxIns [TxIn]
--                    | TxBalanceErrorInvalidProtocolParameters

-- | Compute the total balance of the proposed transaction. Ultimately a valid
-- transaction must be fully balanced: that is have a total value of zero.
--
-- Finding the (non-zero) balance of partially constructed transaction is
-- useful for adjusting a transaction to be fully balanced.
--
evaluateTransactionBalance :: forall era.
                              IsShelleyBasedEra era
                           => ProtocolParameters
                           -> Set PoolId
                           -> UTxO era
                           -> TxBody era
                           -> TxOutValue era
evaluateTransactionBalance _ _ _ (ByronTxBody _) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}
    --TODO: we could actually support Byron here, it'd be different but simpler

evaluateTransactionBalance pparams poolids utxo
                           (ShelleyTxBody era txbody _ _ _) =
    withLedgerConstraints era evalAdaOnly evalMultiAsset
  where
    isNewPool :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> Bool
    isNewPool kh = StakePoolKeyHash kh `Set.notMember` poolids

    evalMultiAsset :: forall ledgerera.
                      ShelleyLedgerEra era ~ ledgerera
                   => LedgerEraConstraints ledgerera
                   => LedgerMultiAssetConstraints ledgerera
                   => LedgerPParamsConstraints ledgerera
                   => LedgerTxBodyConstraints ledgerera
                   => MultiAssetSupportedInEra era
                   -> TxOutValue era
    evalMultiAsset evidence =
      TxOutValue evidence . fromMaryValue $
         Ledger.evaluateTransactionBalance
           (toLedgerPParams era pparams)
           (toLedgerUTxO era utxo)
           isNewPool
           txbody

    evalAdaOnly =
      error "TODO: evalAdaOnly case in evaluateTransactionBalance"

    -- Conjur up all the necessary class instances and evidence
    withLedgerConstraints
      :: ShelleyLedgerEra era ~ ledgerera
      => ShelleyBasedEra era
      -> (   LedgerEraConstraints ledgerera
          => LedgerAdaOnlyConstraints ledgerera
          => LedgerPParamsConstraints ledgerera
          => LedgerTxBodyConstraints ledgerera
          => OnlyAdaSupportedInEra era
          -> a)
      -> (   LedgerEraConstraints ledgerera
          => LedgerMultiAssetConstraints ledgerera
          => LedgerPParamsConstraints ledgerera
          => LedgerTxBodyConstraints ledgerera
          => MultiAssetSupportedInEra era
          -> a)
      -> a
    withLedgerConstraints ShelleyBasedEraShelley f _ = f AdaOnlyInShelleyEra
    withLedgerConstraints ShelleyBasedEraAllegra f _ = f AdaOnlyInAllegraEra
    withLedgerConstraints ShelleyBasedEraMary    _ f = f MultiAssetInMaryEra
    withLedgerConstraints ShelleyBasedEraAlonzo  _ f = f MultiAssetInAlonzoEra

type LedgerEraConstraints ledgerera =
       ( Ledger.Era ledgerera
       , Ledger.Era.Crypto ledgerera ~ Ledger.StandardCrypto
       )

type LedgerAdaOnlyConstraints ledgerera =
         Ledger.Value ledgerera ~ Ledger.Coin

type LedgerMultiAssetConstraints ledgerera =
       ( Ledger.Value ledgerera ~ Mary.Value Ledger.StandardCrypto
       , HasField "mint" (Ledger.TxBody ledgerera) (Ledger.Value ledgerera)
       )

type LedgerPParamsConstraints ledgerera =
       ( HasField "_minfeeA"     (Ledger.PParams ledgerera) Natural
       , HasField "_minfeeB"     (Ledger.PParams ledgerera) Natural
       , HasField "_keyDeposit"  (Ledger.PParams ledgerera) Ledger.Coin
       , HasField "_poolDeposit" (Ledger.PParams ledgerera) Ledger.Coin
       )

type LedgerTxBodyConstraints ledgerera =
       ( HasField "certs" (Ledger.TxBody ledgerera)
                          (StrictSeq (Shelley.DCert Ledger.StandardCrypto))
       , HasField "inputs" (Ledger.TxBody ledgerera)
                           (Set (Shelley.TxIn Ledger.StandardCrypto))
       , HasField "wdrls" (Ledger.TxBody ledgerera) (Shelley.Wdrl Ledger.StandardCrypto)
       )

type LedgerAlonzoConstraints ledgerera =
       ( LedgerMultiAssetConstraints ledgerera
       , Ledger.TxBody    ledgerera ~ Alonzo.TxBody    ledgerera
       , Ledger.Witnesses ledgerera ~ Alonzo.TxWitness ledgerera
       , Ledger.Script    ledgerera ~ Alonzo.Script    ledgerera
       , Ledger.TxOut     ledgerera ~ Alonzo.TxOut     ledgerera
       , HasField "_prices" (Ledger.PParams ledgerera) Alonzo.Prices
       , HasField "witnesses" (Ledger.TxInBlock ledgerera) (Alonzo.TxWitness ledgerera)
       )


