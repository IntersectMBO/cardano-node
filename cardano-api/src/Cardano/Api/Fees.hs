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
    BalanceTxBodyError(..),
    transactionFee,
    estimateTransactionFee,
    makeTransactionBodyAutoBalance,
    evaluateTransactionExecutionUnits,
    evaluateTransactionFee,
    evaluateTransactionBalance,

     -- * Error rendering
    renderPlutusFailure,
    renderTxExecutionUnitsError,
  ) where

import           Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Array as Array
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence.Strict (StrictSeq (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Records (HasField (..))
import           Numeric.Natural

import           Control.Monad.Trans.Except

import qualified Cardano.Binary as CBOR
import           Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)

import qualified Cardano.Chain.Common as Byron

import qualified Cardano.Ledger.Alonzo.Tools as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Era as Ledger.Era (Crypto)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Tx as Ledger

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Script
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.Value

-- Temporarily, until they're moved to the ledger API:
import           Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Shelley.Spec.Ledger.API.Wallet as Ledger (evaluateTransactionBalance,
                   evaluateTransactionFee)
import           Shelley.Spec.Ledger.PParams (PParams' (..))

import qualified Shelley.Spec.Ledger.API as Shelley

import qualified Cardano.Ledger.Mary.Value as Mary

import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo

import qualified Ouroboros.Consensus.HardFork.History as Consensus

--TODO: Expose through ledger-specs
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusCore.Pretty as Plutus

{- HLINT ignore "Redundant return" -}

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

data BalanceTxBodyError era =
       BalanceTxBodyErr (TxBodyError era)
     | BalanceScriptFailure ScriptFailure
     | BalanceMoreInputsNeeded Lovelace Lovelace Aeson.Value
     | BalanceMinUTxONotMet
     | BalanceByronEraNotSupported
     | BalanceMinUTxOPParamNotFound
     | BalanceCostPerWordPParamNotFound
     | BalanceTxExecUnits TxExecutionUnitsError
  deriving Show

instance Error (BalanceTxBodyError era) where
  displayError (BalanceTxBodyErr e) =
    "Transaction balance body error: " <> displayError e
  displayError (BalanceScriptFailure sFailure) =
    show $ renderPlutusFailure sFailure
  displayError (BalanceMoreInputsNeeded currentBalance fee utxoVal ) =
    "Transaction balance needs more inputs. Current balance: " <> show currentBalance <>
    "\n" <> "Fee :" <> show fee <> "\n" <> "UTxOVal: " <> show utxoVal

  displayError BalanceMinUTxONotMet =
    "Transaction balance minimum UTxO not met"
  displayError BalanceByronEraNotSupported =
    "Transaction balance Byron era not supported"
  displayError BalanceMinUTxOPParamNotFound =
    "Transaction balance min UTxO protocol parameter not found"
  displayError BalanceCostPerWordPParamNotFound =
    "Transaction balance cost per word protocol parameter not found"
  displayError (BalanceTxExecUnits txExecErr) =
    show $ renderTxExecutionUnitsError txExecErr

-- Steps:
-- 1. evaluate all the scripts to get the exec units, update with ex units
-- 2. figure out the overall min fees
-- 3. update tx with fees
-- 4. balance the transaction and update tx change output

makeTransactionBodyAutoBalance :: ShelleyBasedEra era
                               -> EraInMode era mode
                               -> SystemStart
                               -> EraHistory mode
                               -> ProtocolParameters
                               -> Set PoolId
                               -> UTxO era
                               -> TxBodyContent BuildTx era
                               -> AddressInEra era
                               -> Either (BalanceTxBodyError era)
                                         (TxBody era, Map ScriptWitnessIndex ExecutionUnits, Lovelace)
makeTransactionBodyAutoBalance sbe eraInMode systemstart history pparams
                               poolids utxo txbodycontent changeaddr = do
    txbody0 <- first BalanceTxBodyErr $
                 obtainIsCardanoEraConstraint sbe $ makeTransactionBody txbodycontent

    exUnitsMap <- first BalanceTxExecUnits $
                    evaluateTransactionExecutionUnits
                      eraInMode
                      systemstart history
                      pparams utxo
                      txbody0

    exUnitsMap' <- traverse (first BalanceScriptFailure) exUnitsMap

    let txbodycontent1 = substituteExecutionUnits exUnitsMap' txbodycontent

    explicitInE <- first (const BalanceByronEraNotSupported)
                     $ txFeesExplicitInEra (shelleyBasedToCardanoEra sbe)

    -- Insert change address and set tx fee to 0
    txbodyWithExecUnitsZeroFeeAndChangeAddress <- obtainIsEra sbe $ first BalanceTxBodyErr $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee  = TxFeeExplicit explicitInE 0,
                 txOuts = TxOut changeaddr
                                (lovelaceToTxOutValue 0)
                                TxOutDatumHashNone
                        : txOuts txbodycontent
                 --TODO: think about the size of the change output
                 -- 1,2,4 or 8 bytes?
               }

    let txfee = obtainIsCardanoEraConstraint sbe
                  $ obtainLedgerEra sbe $ evaluateTransactionFee sbe pparams txbodyWithExecUnitsZeroFeeAndChangeAddress
    txbody2WithFee <- first BalanceTxBodyErr $ -- TODO: impossible to fail now
                obtainIsEra sbe $ makeTransactionBody txbodycontent1 {
                 txFee = TxFeeExplicit explicitInE txfee
               }

    let balance = obtainCLI sbe $ evaluateTransactionBalance sbe pparams poolids utxo txbody2WithFee
    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    minUTxOValue <- getMinUTxOValue pparams
    case balance of
      TxOutAdaOnly _ l -> obtainIsEra sbe $ balanceCheck minUTxOValue l txfee utxo
      TxOutValue _ v   ->
        case valueToLovelace v of
          Nothing -> Left $ error "TODO: non-ada assets not balanced"
          Just c -> obtainIsEra sbe $ balanceCheck minUTxOValue c txfee utxo

    --TODO: we could add the extra fee for the CBOR encoding of the change,
    -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.

    txbody3 <- first BalanceTxBodyErr $ -- TODO: impossible to fail now
               obtainIsEra sbe $ makeTransactionBody txbodycontent {
                 txFee  = TxFeeExplicit explicitInE txfee,
                 txOuts = TxOut changeaddr balance TxOutDatumHashNone
                        : txOuts txbodycontent
               }

    return (txbody3, exUnitsMap', txfee)
 where
   obtainIsEra
     :: ShelleyBasedEra era
     -> (( IsCardanoEra era
       ) => a) -> a
   obtainIsEra ShelleyBasedEraShelley f = f
   obtainIsEra ShelleyBasedEraAllegra f = f
   obtainIsEra ShelleyBasedEraMary    f = f
   obtainIsEra ShelleyBasedEraAlonzo  f = f

   obtainCLI
     :: ShelleyBasedEra era
     -> (( Shelley.CLI (ShelleyLedgerEra era)
       ) => a) -> a
   obtainCLI ShelleyBasedEraShelley f = f
   obtainCLI ShelleyBasedEraAllegra f = f
   obtainCLI ShelleyBasedEraMary    f = f
   obtainCLI ShelleyBasedEraAlonzo  f = f


   obtainIsCardanoEraConstraint
     :: ShelleyBasedEra era
     -> (( IsCardanoEra era
         , Shelley.CLI (ShelleyLedgerEra era)
       ) => a) -> a
   obtainIsCardanoEraConstraint ShelleyBasedEraShelley f = f
   obtainIsCardanoEraConstraint ShelleyBasedEraAllegra f = f
   obtainIsCardanoEraConstraint ShelleyBasedEraMary    f = f
   obtainIsCardanoEraConstraint ShelleyBasedEraAlonzo  f = f

   obtainLedgerEra
     :: ShelleyBasedEra era
     -> (( Ledger.Era (ShelleyLedgerEra era)
         , CBOR.ToCBOR (Ledger.AuxiliaryData (ShelleyLedgerEra era))
         , CBOR.ToCBOR (Ledger.TxBody (ShelleyLedgerEra era))
         , CBOR.ToCBOR (Ledger.Witnesses (ShelleyLedgerEra era))
         , HasField "certs" (Ledger.TxBody (ShelleyLedgerEra era))
             (StrictSeq (Shelley.DCert Ledger.StandardCrypto))
         ) => a) -> a
   obtainLedgerEra ShelleyBasedEraShelley f = f
   obtainLedgerEra ShelleyBasedEraAllegra f = f
   obtainLedgerEra ShelleyBasedEraMary    f = f
   obtainLedgerEra ShelleyBasedEraAlonzo  f = f

   balanceCheck :: IsCardanoEra era => Lovelace -> Lovelace -> Lovelace -> UTxO era -> Either (BalanceTxBodyError era) ()
   balanceCheck minUTxOValue balance fee utxo'
    | balance < 0 = Left $ BalanceMoreInputsNeeded balance fee (Aeson.toJSON utxo')
      -- check the change is over the min utxo threshold
    | balance < minUTxOValue = Left BalanceMinUTxONotMet
    | otherwise = return ()

   getMinUTxOValue :: ProtocolParameters ->  Either (BalanceTxBodyError era) Lovelace
   getMinUTxOValue pparams' =
     case sbe of
       ShelleyBasedEraShelley -> minUTxOHelper pparams'
       ShelleyBasedEraAllegra -> minUTxOHelper pparams'
       ShelleyBasedEraMary -> minUTxOHelper pparams'
       ShelleyBasedEraAlonzo -> case protocolParamUTxOCostPerWord pparams' of
                                  Just minUtxo -> Right minUtxo
                                  Nothing -> Left BalanceCostPerWordPParamNotFound

   minUTxOHelper :: ProtocolParameters -> Either (BalanceTxBodyError era) Lovelace
   minUTxOHelper pparams' = case protocolParamMinUTxOValue pparams' of
                             Just minUtxo -> Right minUtxo
                             Nothing -> Left BalanceMinUTxOPParamNotFound

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

data ScriptFailure
  = UnnecessaryRedeemer ScriptWitnessIndex
  | MissingScript ScriptWitnessIndex
  | MissingDatum (Hash ScriptData)
  | ValidationFailed Plutus.EvaluationError
  | MissingSpendingTxIn TxIn
  | SpendingTxInMissingDatumHash TxIn
  | IncompatibleBudget Plutus.ExBudget
  deriving Show

renderPlutusFailure :: ScriptFailure -> Text
renderPlutusFailure (UnnecessaryRedeemer index) =
  "Unnecessary redeemer at: " <> renderScriptWitnessIndex index
renderPlutusFailure (MissingScript index) =
  "Missing Plutus script at: " <> renderScriptWitnessIndex index
renderPlutusFailure (MissingDatum dHash) =
  "Missing datum with hash: " <> serialiseToRawBytesHexText dHash
renderPlutusFailure (ValidationFailed evalErr) =
  "Plutus script validation failed: " <> Plutus.render (Plutus.pretty evalErr)
renderPlutusFailure (MissingSpendingTxIn txin) =
  "Missing Plutus spending txin: " <> renderTxIn txin
renderPlutusFailure (SpendingTxInMissingDatumHash txin) =
  "Plutus spending txin is missing a datum hash: " <> renderTxIn txin
renderPlutusFailure (IncompatibleBudget exBudget) =
   "Incompatible Plutus budget: " <> Plutus.render (Plutus.pretty exBudget)

fromLedgerScriptFailure
  :: Alonzo.ScriptFailure Ledger.StandardCrypto
  -> ScriptFailure
fromLedgerScriptFailure (Alonzo.RedeemerNotNeeded rdmrPtr) =
  UnnecessaryRedeemer $ fromAlonzoRdmrPtr rdmrPtr
fromLedgerScriptFailure (Alonzo.MissingScript rdmrPtr) =
  MissingScript $ fromAlonzoRdmrPtr rdmrPtr
fromLedgerScriptFailure (Alonzo.MissingDatum dataHash) =
  MissingDatum $ ScriptDataHash dataHash
fromLedgerScriptFailure (Alonzo.ValidationFailed evalErr) =
  ValidationFailed evalErr
fromLedgerScriptFailure (Alonzo.UnknownTxIn txin) =
  MissingSpendingTxIn $ fromShelleyTxIn txin
fromLedgerScriptFailure (Alonzo.InvalidTxIn txin) =
  SpendingTxInMissingDatumHash $ fromShelleyTxIn txin
fromLedgerScriptFailure (Alonzo.IncompatibleBudget exBudget) =
  IncompatibleBudget exBudget


data TxExecutionUnitsError
  = TxExecUnitsErrorPastHorizon Consensus.PastHorizonException
  deriving Show

renderTxExecutionUnitsError :: TxExecutionUnitsError -> Text
renderTxExecutionUnitsError (TxExecUnitsErrorPastHorizon pastHorizException) =
  "PastHorizonException: " <> Text.pack (show pastHorizException)


-- | Run all the scripts in a transaction and return the execution units needed
-- for each use of each script. The total execution units for the transaction
-- is the sum of these.
--
evaluateTransactionExecutionUnits
  :: forall era mode.
     EraInMode era mode
  -> SystemStart
  -> EraHistory mode
  -> ProtocolParameters
  -> UTxO era
  -> TxBody era
  -> Either TxExecutionUnitsError --TODO: wrap error
            (Map ScriptWitnessIndex (Either ScriptFailure ExecutionUnits))
evaluateTransactionExecutionUnits _eraInMode systemstart history pparams utxo txbody =
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
               => ledgerera ~ Alonzo.AlonzoEra Ledger.StandardCrypto
               => LedgerEraConstraints ledgerera
               => ShelleyBasedEra era
               -> Ledger.Tx ledgerera
               -> Either TxExecutionUnitsError
                         (Map ScriptWitnessIndex
                              (Either ScriptFailure ExecutionUnits))
    evalAlonzo era tx =
      case Alonzo.evaluateTransactionExecutionUnits
             tx
             (toLedgerUTxO era utxo)
             (toLedgerEpochInfo history)
             systemstart
             (toAlonzoCostModels (protocolParamCostModels pparams))
        of Left  err   -> Left err
           Right exmap -> Right (fromLedgerScriptExUnitsMap exmap)

    toLedgerEpochInfo :: EraHistory mode -> EpochInfo (Either TxExecutionUnitsError)
    toLedgerEpochInfo (EraHistory _ interpreter) =
        hoistEpochInfo (first TxExecUnitsErrorPastHorizon . runExcept) $
          Consensus.interpreterToEpochInfo interpreter

    toAlonzoCostModels :: Map AnyPlutusScriptVersion CostModel
                       -> Array.Array Alonzo.Language Alonzo.CostModel
    toAlonzoCostModels costmodels =
      Array.array
        (minBound, maxBound)
        [ (toAlonzoLanguage lang, toAlonzoCostModel costmodel)
        | (lang, costmodel) <- Map.toList costmodels ]

    fromLedgerScriptExUnitsMap
      :: Map Alonzo.RdmrPtr (Either (Alonzo.ScriptFailure Ledger.StandardCrypto)
                                    Alonzo.ExUnits)
      -> Map ScriptWitnessIndex (Either ScriptFailure ExecutionUnits)
    fromLedgerScriptExUnitsMap exmap =
      Map.fromList
        [ (fromAlonzoRdmrPtr rdmrptr,
           bimap fromLedgerScriptFailure fromAlonzoExUnits exunitsOrFailure)
        | (rdmrptr, exunitsOrFailure) <- Map.toList exmap ]

evaluateTransactionFee :: forall era ledgerera.
                          ShelleyLedgerEra era ~ ledgerera
                       => Shelley.CLI ledgerera
                       => CBOR.ToCBOR (Ledger.AuxiliaryData ledgerera)
                       => CBOR.ToCBOR (Ledger.TxBody ledgerera)
                       => CBOR.ToCBOR (Ledger.Witnesses ledgerera)
                       => HasField "certs" (Ledger.TxBody (ShelleyLedgerEra era)) (StrictSeq (Shelley.DCert Ledger.StandardCrypto))
                       => ShelleyBasedEra era
                       -> ProtocolParameters
                       -> TxBody era
                       -> Lovelace
evaluateTransactionFee sbe pparams txbody =
    case makeSignedTransaction [] txbody of
      ByronTx{} -> case sbe :: ShelleyBasedEra era of {}
      --TODO: we could actually support Byron here, it'd be different but simpler
      ShelleyTx era tx' -> evalAnyEra era tx'

  where
      -- TODO: evaluateTransactionFee need to compute the number of key witnesses we need,
      -- this may require more help from the ledger to find all the places that need it
    reqKeyWits' :: Ledger.TxBody (ShelleyLedgerEra era) -> Word
    reqKeyWits' txbody' =
      -- TODO: What about tx in key witnesses?
      countCertKeyWitnesses $ getField @"certs" txbody'

    stakeCredKeyWit :: StakeCredential -> Word
    stakeCredKeyWit (StakeCredentialByKey _) = 1
    stakeCredKeyWit (StakeCredentialByScript _) = 0

    certKeyWit :: Certificate -> Word
    certKeyWit StakeAddressRegistrationCertificate {} = 0
    certKeyWit (StakeAddressDeregistrationCertificate sCred) =
      stakeCredKeyWit sCred
    certKeyWit (StakeAddressDelegationCertificate sCred _) =
      stakeCredKeyWit sCred
    certKeyWit (StakePoolRegistrationCertificate poolParams) =
      fromIntegral $ length (stakePoolOwners poolParams) + 1
    certKeyWit StakePoolRetirementCertificate {} = 1
    certKeyWit GenesisKeyDelegationCertificate {} = 1 --TODO: Double check this
    certKeyWit (MIRCertificate _ _) = 0 -- TODO: Double check this

    countCertKeyWitnesses :: StrictSeq (Shelley.DCert Ledger.StandardCrypto) -> Word
    countCertKeyWitnesses Empty = 0
    countCertKeyWitnesses (cert :<| rest) =
      certKeyWit (fromShelleyCertificate cert) + countCertKeyWitnesses rest

    evalAnyEra :: ShelleyBasedEra era -> Ledger.Tx (ShelleyLedgerEra era) -> Lovelace
    evalAnyEra sbe' tx =
      fromShelleyLovelace $
        Ledger.evaluateTransactionFee
          (toLedgerPParams sbe' pparams)
          tx
          (reqKeyWits' $ Ledger.body tx)

--data TxBalanceError = TxBalanceErrorMissingTxIns [TxIn]
--                    | TxBalanceErrorInvalidProtocolParameters

-- | Compute the total balance of the proposed transaction. Ultimately a valid
-- transaction must be fully balanced: that is have a total value of zero.
--
-- Finding the (non-zero) balance of partially constructed transaction is
-- useful for adjusting a transaction to be fully balanced.
--
evaluateTransactionBalance :: forall era. Shelley.CLI (ShelleyLedgerEra era)
                           => ShelleyBasedEra era
                           -> ProtocolParameters
                           -> Set PoolId
                           -> UTxO era
                           -> TxBody era
                           -> TxOutValue era
evaluateTransactionBalance sbe _ _ _ (ByronTxBody _) =
    case sbe :: ShelleyBasedEra era of {}
    --TODO: we could actually support Byron here, it'd be different but simpler

evaluateTransactionBalance sbe pparams poolids utxo
                           (ShelleyTxBody era txbody _ _ _) =
    withLedgerConstraints sbe evalAdaOnly evalMultiAsset
  where
    isNewPool :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> Bool
    isNewPool kh = StakePoolKeyHash kh `Set.notMember` poolids

    evalMultiAsset :: forall ledgerera.
                      ShelleyLedgerEra era ~ ledgerera
                   => LedgerEraConstraints ledgerera
                   => LedgerMultiAssetConstraints ledgerera
                   => MultiAssetSupportedInEra era
                   -> TxOutValue era
    evalMultiAsset evidence =
      TxOutValue evidence . fromMaryValue $
         Ledger.evaluateTransactionBalance
           (toLedgerPParams era pparams)
           (toLedgerUTxO era utxo)
           isNewPool
           txbody

    evalAdaOnly
      :: Ledger.Value (ShelleyLedgerEra era) ~ Shelley.Coin
      => Ledger.Era.Crypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
      => OnlyAdaSupportedInEra era -> TxOutValue era
    evalAdaOnly evidence =
     TxOutAdaOnly evidence . fromShelleyLovelace
       $ Ledger.evaluateTransactionBalance
           (toLedgerPParams era pparams)
           (toLedgerUTxO era utxo)
           isNewPool
           txbody

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


