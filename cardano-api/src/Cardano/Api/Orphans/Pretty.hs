{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans.Pretty () where

import           Cardano.Api.Orphans ()
import           Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)
import           Cardano.Ledger.Shelley.Rules.Bbody
import           Cardano.Ledger.Shelley.Rules.Deleg
import           Cardano.Ledger.Shelley.Rules.Delegs
import           Cardano.Ledger.Shelley.Rules.Delpl
import           Cardano.Ledger.Shelley.Rules.Ledger
import           Cardano.Ledger.Shelley.Rules.Pool
import           Cardano.Ledger.Shelley.Rules.Ppup
import           Cardano.Ledger.Shelley.Rules.Utxow
import           Cardano.Prelude
import           Data.Aeson (ToJSON (..), Value(..), object, (.=))
import           Data.Compact.VMap (VB, VMap)
import           Ouroboros.Consensus.Shelley.Eras as Consensus (StandardAlonzo)
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Prelude hiding ((.), map, show)
import           Prettyprinter (Pretty, (<+>))

import qualified Cardano.Api.Alonzo.Render as Render
import qualified Cardano.Api.TxBody as Api
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Core
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Bbody as Ledger
import qualified Cardano.Prelude as CP
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Compact.VMap as VMap
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Prettyprinter as PP

instance
  ( ShelleyBasedEra era
  , PP.Pretty (PredicateFailure (Core.EraRule "DELEGS" era))
  , PP.Pretty (PredicateFailure (Core.EraRule "UTXOW" era))
  ) => PP.Pretty (Consensus.ApplyTxError era) where
  pretty (Consensus.ApplyTxError predicateFailures) = PP.vsep (fmap PP.pretty predicateFailures)


instance
  ( ShelleyBasedEra era
  , PP.Pretty (PredicateFailure (Core.EraRule "DELEGS" era))
  , PP.Pretty (PredicateFailure (Core.EraRule "UTXOW" era))
  ) => PP.Pretty (LedgerPredicateFailure era) where
  pretty (UtxowFailure f) = PP.vsep
    [ "UtxowFailure:"
    , PP.nest 2 $ (PP.pretty  f)
    ]
  pretty (DelegsFailure f) = PP.vsep
    [ "DelegsFailure:"
    , PP.nest 2 $ (PP.pretty f)
    ]

instance
  ( PP.Pretty (PredicateFailure (Core.EraRule "POOL" era))
  , PP.Pretty (PredicateFailure (Core.EraRule "DELEG" era))
  ) => PP.Pretty (DelplPredicateFailure era) where
  pretty (PoolFailure   f) = PP.pretty f
  pretty (DelegFailure  f) = PP.pretty f

instance ( ShelleyBasedEra era
         , PP.Pretty (Ledger.PredicateFailure (Core.EraRule "DELPL" era))
         , ToJSON (Map (RewardAcnt (Ledger.Crypto era)) Coin)
         ) => PP.Pretty (DelegsPredicateFailure era) where
  pretty (DelegateeNotRegisteredDELEG targetPool) = PP.vsep
    [ "DelegateeNotRegisteredDELEG"
    , "targetPool:"
    , PP.nest 2 $ PP.pretty targetPool
    ]
  pretty (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) = PP.vsep
    [ "WithdrawalsNotInRewardsDELEGS"
    , "incorrectWithdrawals"
    -- TODO borrowing ToJSON instance
    , PP.nest 2 $ "[a]" <+> do prettyJson $ toJSON incorrectWithdrawals
    ]
  pretty (DelplFailure f) = PP.pretty f

instance Crypto.Crypto crypto => PP.Pretty (VMap VB VB (Shelley.KeyHash 'Shelley.StakePool crypto) (Shelley.PoolParams crypto)) where
  pretty = prettyMap . VMap.toMap

prettyMap :: forall k v ann. (PP.Pretty k, PP.Pretty v) => Map k v -> PP.Doc ann
prettyMap m = PP.vsep $ uncurry prettyEntry <$> Map.toList m
  where prettyEntry :: k -> v -> PP.Doc ann
        prettyEntry k v = PP.hsep
          [ PP.pretty k <> ": "
          , PP.nest 2 $ PP.pretty v
          ]

-- TODO Borrowing ToJSON instance for implementation for now.
instance
  ( Crypto.Crypto crypto
  , ToJSON (KeyHash 'StakePool crypto)
  ) => PP.Pretty (KeyHash 'StakePool crypto) where
  pretty v = "[b]" <+> do prettyJson $ toJSON v

instance
  ( Crypto.Crypto crypto
  , ToJSON (PoolParams crypto)
  ) => PP.Pretty (PoolParams crypto) where
  pretty v = "[c]" <+> do prettyJson $ toJSON v

instance
  ( Core.Crypto (Ledger.Crypto era)
  ) => PP.Pretty (PoolPredicateFailure era) where
  pretty (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) = "[da]" <+> do
    prettyJson $ object
      [ "kind"                .= String "StakePoolNotRegisteredOnKeyPOOL"
      , "unregisteredKeyHash" .= String (textShow unregStakePool)
      , "error"               .= String "This stake pool key hash is unregistered"
      ]
  pretty (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) = "[db]" <+> do
    prettyJson $ object
      [ "kind"                    .= String "StakePoolRetirementWrongEpochPOOL"
      , "currentEpoch"            .= String (textShow currentEpoch)
      , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
      , "maxEpochForRetirement"   .= String (textShow maxRetireEpoch)
      ]
  pretty (StakePoolCostTooLowPOOL certCost protCost) = "[dc]" <+> do
    prettyJson $ object
      [ "kind"              .= String "StakePoolCostTooLowPOOL"
      , "certificateCost"   .= String (textShow certCost)
      , "protocolParCost"   .= String (textShow protCost)
      , "error"             .= String "The stake pool cost is too low"
      ]
  pretty (PoolMedataHashTooBig poolID hashSize) = "[dd]" <+> do
    prettyJson $ object
      [ "kind"      .= String "PoolMedataHashTooBig"
      , "poolID"    .= String (textShow poolID)
      , "hashSize"  .= String (textShow hashSize)
      , "error"     .= String "The stake pool metadata hash is too large"
      ]

-- Apparently this should never happen according to the Shelley exec spec
  pretty (WrongCertificateTypePOOL index) =
    case index of
      0 -> "[ee]" <+> do
        prettyJson $ object
          [ "kind"  .= String "WrongCertificateTypePOOL"
          , "error" .= String "Wrong certificate type: Delegation certificate"
          ]
      1 -> "[ef]" <+> do
        prettyJson $ object
          [ "kind"  .= String "WrongCertificateTypePOOL"
          , "error" .= String "Wrong certificate type: MIR certificate"
          ]
      2 -> "[eg]" <+> do
        prettyJson $ object
          [ "kind"  .= String "WrongCertificateTypePOOL"
          , "error" .= String "Wrong certificate type: Genesis certificate"
          ]
      k -> "[eh]" <+> do
        prettyJson $ object
          [ "kind"            .= String "WrongCertificateTypePOOL"
          , "certificateType" .= k
          , "error"           .= String "Wrong certificate type: Unknown certificate type"
          ]

  pretty (WrongNetworkPOOL networkId listedNetworkId poolId) = "[i]" <+> do
    prettyJson $ object
      [ "kind"            .= String "WrongNetworkPOOL"
      , "networkId"       .= String (textShow networkId)
      , "listedNetworkId" .= String (textShow listedNetworkId)
      , "poolId"          .= String (textShow poolId)
      , "error"           .= String "Wrong network ID in pool registration certificate"
      ]


instance
  ( Ledger.Era era
  ) => PP.Pretty (DelegPredicateFailure era) where
  pretty (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) = "[f]" <+> do
    prettyJson $ object
      [ "kind"        .= String "StakeKeyAlreadyRegisteredDELEG"
      , "credential"  .= String (textShow alreadyRegistered)
      , "error"       .= String "Staking credential already registered"
      ]
  pretty (StakeKeyInRewardsDELEG alreadyRegistered) = "[f]" <+> do
    prettyJson $ object
      [ "kind"        .= String "StakeKeyInRewardsDELEG"
      , "credential"  .= String (textShow alreadyRegistered)
      , "error"       .= String "Staking credential registered in rewards map"
      ]
  pretty (StakeKeyNotRegisteredDELEG notRegistered) = "[f]" <+> do
    prettyJson $ object
      [ "kind"        .= String "StakeKeyNotRegisteredDELEG"
      , "credential"  .= String (textShow notRegistered)
      , "error"       .= String "Staking credential not registered"
      ]
  pretty (StakeKeyNonZeroAccountBalanceDELEG remBalance) = "[f]" <+> do
    prettyJson $ object
      [ "kind"              .= String "StakeKeyNonZeroAccountBalanceDELEG"
      , "remainingBalance"  .= remBalance
      ]
  pretty (StakeDelegationImpossibleDELEG unregistered) = "[f]" <+> do
    prettyJson $ object
      [ "kind"        .= String "StakeDelegationImpossibleDELEG"
      , "credential"  .= String (textShow unregistered)
      , "error"       .= String "Cannot delegate this stake credential because it is not registered"
      ]
  pretty WrongCertificateTypeDELEG = "[f]" <+> do
    prettyJson $ object
      [ "kind" .= String "WrongCertificateTypeDELEG"
      ]
  pretty (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) = "[f]" <+> do
    prettyJson $ object
      [ "kind"            .= String "GenesisKeyNotInMappingDELEG"
      , "unknownKeyHash"  .= String (textShow genesisKeyHash)
      , "error"           .= String "This genesis key is not in the delegation mapping"
      ]
  pretty (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) = "[f]" <+> do
    prettyJson $ object
      [ "kind"              .= String "DuplicateGenesisDelegateDELEG"
      , "duplicateKeyHash"  .= String (textShow genesisKeyHash)
      , "error"             .= String "This genesis key has already been delegated to"
      ]
  pretty (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) = "[f]" <+> do
    prettyJson $ object
      [ "kind"          .= String "InsufficientForInstantaneousRewardsDELEG"
      , "pot"           .= String potText
      , "neededAmount"  .= neededMirAmount
      , "reserves"      .= reserves
      ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  pretty (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) = "[f]" <+> do
    prettyJson $ object
      [ "kind"                        .= String "MIRCertificateTooLateinEpochDELEG"
      , "currentSlotNo"               .= currSlot
      , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
      ]
  pretty (DuplicateGenesisVRFDELEG vrfKeyHash) = "[f]" <+> do
    prettyJson $ object
      [ "kind"    .= String "DuplicateGenesisVRFDELEG"
      , "keyHash" .= vrfKeyHash
      ]
  pretty MIRTransferNotCurrentlyAllowed = "[f]" <+> do
    prettyJson $ object
      [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
      ]
  pretty MIRNegativesNotCurrentlyAllowed = "[f]" <+> do
    prettyJson $ object
      [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
      ]
  pretty (InsufficientForTransferDELEG mirpot attempted available) = "[f]" <+> do
    prettyJson $ object
      [ "kind"      .= String "DuplicateGenesisVRFDELEG"
      , "pot"       .= String potText
      , "attempted" .= attempted
      , "available" .= available
      ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  pretty MIRProducesNegativeUpdate = "[f]" <+> do
    prettyJson $ object
      [ "kind" .= String "MIRProducesNegativeUpdate"
      ]
  pretty (MIRNegativeTransfer pot coin) = "[f]" <+> do
    prettyJson $ object
      [ "kind"    .= String "MIRNegativeTransfer"
      , "error"   .= String "Attempt to transfer a negative amount from a pot."
      , "pot"     .= String potText
      , "amount"  .= coin
      ]
    where potText = case pot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"

instance
  ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
  ) => PP.Pretty (UtxowPredicateFail (Alonzo.AlonzoEra StandardCrypto)) where
  pretty (WrappedShelleyEraFailure utxoPredFail) = PP.vsep
    [ "WrappedShelleyEraFailure:"
    , PP.nest 2 $ PP.pretty utxoPredFail
    ]
  pretty (MissingRedeemers scripts) = "[g2]" <+> do
    prettyJson $ object
      [ "kind"    .= String "MissingRedeemers"
      , "scripts" .= Render.renderMissingRedeemers scripts
      ]
  pretty (MissingRequiredDatums required received) = "[g3]" <+> do
    prettyJson $ object
      [ "kind"      .= String "MissingRequiredDatums"
      , "required"  .= map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList required)
      , "received"  .= map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList received)
      ]
  pretty (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) = "[g4]" <+> do
    prettyJson $ object
      [ "kind"        .= String "PPViewHashesDontMatch"
      , "fromTxBody"  .= Render.renderScriptIntegrityHash (strictMaybeToMaybe ppHashInTxBody)
      , "fromPParams" .= Render.renderScriptIntegrityHash (strictMaybeToMaybe ppHashFromPParams)
      ]
  pretty (MissingRequiredSigners missingKeyWitnesses) = "[g5]" <+> do
    prettyJson $ object
      [ "kind"      .= String "MissingRequiredSigners"
      , "witnesses" .= Set.toList missingKeyWitnesses
      ]
  pretty (UnspendableUTxONoDatumHash txins) = "[g6]" <+> do
    prettyJson $ object
      [ "kind"  .= String "MissingRequiredSigners"
      , "txins" .= Set.toList txins
      ]
  pretty (NonOutputSupplimentaryDatums disallowed acceptable) = "[g7]" <+> do
    prettyJson $ object
      [ "kind"        .= String "NonOutputSupplimentaryDatums"
      , "disallowed"  .= Set.toList disallowed
      , "acceptable"  .= Set.toList acceptable
      ]
  pretty (ExtraRedeemers rdmrs) = "[g8]" <+> do
    prettyJson $ object
      [ "kind"  .= String "ExtraRedeemers"
      , "rdmrs" .= map (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) rdmrs
      ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  -- , ToJSON (PredicateFailure (Core.EraRule "UTXO" era))
  , Pretty (PredicateFailure (Core.EraRule "UTXO" era))
  , ToJSON (Core.AuxiliaryDataHash (Ledger.Crypto era))
  ) => PP.Pretty (UtxowPredicateFailure era) where
  pretty (ExtraneousScriptWitnessesUTXOW extraneousScripts) = "[h1]" <+> do
    prettyJson $ object
      [ "kind"              .= String "InvalidWitnessesUTXOW"
      , "extraneousScripts" .= extraneousScripts
      ]
  pretty (InvalidWitnessesUTXOW wits') = "[h2]" <+> do
    prettyJson $ object
      [ "kind"              .= String "InvalidWitnessesUTXOW"
      , "invalidWitnesses"  .= map textShow wits'
      ]
  pretty (MissingVKeyWitnessesUTXOW (WitHashes wits')) = "[h3]" <+> do
    prettyJson $ object
      [ "kind"              .= String "MissingVKeyWitnessesUTXOW"
      , "missingWitnesses"  .= wits'
      ]
  pretty (MissingScriptWitnessesUTXOW missingScripts) = "[h4]" <+> do
    prettyJson $ object
      [ "kind"            .= String "MissingScriptWitnessesUTXOW"
      , "missingScripts"  .= missingScripts
      ]
  pretty (ScriptWitnessNotValidatingUTXOW failedScripts) = "[h5]" <+> do
    prettyJson $ object
      [ "kind"          .= String "ScriptWitnessNotValidatingUTXOW"
      , "failedScripts" .= failedScripts
      ]
  pretty (UtxoFailure f) = PP.pretty f
  pretty (MIRInsufficientGenesisSigsUTXOW genesisSigs) = "[h6]" <+> do
    prettyJson $ object
      [ "kind"        .= String "MIRInsufficientGenesisSigsUTXOW"
      , "genesisSigs" .= genesisSigs
      ]
  pretty (MissingTxBodyMetadataHash metadataHash) = "[h7]" <+> do
    prettyJson $ object
      [ "kind"          .= String "MissingTxBodyMetadataHash"
      , "metadataHash"  .= metadataHash
      ]
  pretty (MissingTxMetadata txBodyMetadataHash) = "[h8]" <+> do
    prettyJson $ object
      [ "kind"                .= String "MissingTxMetadata"
      , "txBodyMetadataHash"  .= txBodyMetadataHash
      ]
  pretty (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    "[h9]" <+> do
    prettyJson $ object
      [ "kind"                .= String "ConflictingMetadataHash"
      , "txBodyMetadataHash"  .= txBodyMetadataHash
      , "fullMetadataHash"    .= fullMetadataHash
      ]
  pretty InvalidMetadata = "InvalidMetadata"


instance Pretty (Alonzo.UtxoPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  pretty (Alonzo.BadInputsUTxO badInputs) =
    "[ia]" <+> do
    prettyJson $ object
      [ "kind"      .= String "BadInputsUTxO"
      , "badInputs" .= badInputs
      , "error"     .= Render.renderBadInputsUTxOErr badInputs
      ]
  pretty (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) =
    "[ib]" <+> do
    prettyJson $ object
      [ "kind"              .= String "ExpiredUTxO"
      , "validityInterval"  .= validtyInterval
      , "slot"              .= slot
      ]
  pretty (Alonzo.MaxTxSizeUTxO txsize maxtxsize) =
    "[ic]" <+> do
    prettyJson $ object
      [ "kind"    .= String "MaxTxSizeUTxO"
      , "size"    .= txsize
      , "maxSize" .= maxtxsize
      ]
  pretty Alonzo.InputSetEmptyUTxO =
    "[id]" <+> do
    prettyJson $ object
      [ "kind" .= String "InputSetEmptyUTxO"
      ]
  pretty (Alonzo.FeeTooSmallUTxO minfee currentFee) =
    "[ie]" <+> do
    prettyJson $ object
      [ "kind"    .= String "FeeTooSmallUTxO"
      , "minimum" .= minfee
      , "fee"     .= currentFee
      ]
  pretty (Alonzo.ValueNotConservedUTxO consumed produced) =
    "[if]" <+> do
    prettyJson $ object
      [ "kind"      .= String "ValueNotConservedUTxO"
      , "consumed"  .= consumed
      , "produced"  .= produced
      , "error"     .= Render.renderValueNotConservedErr consumed produced
      ]
  pretty (Alonzo.WrongNetwork network addrs) =
    "[ig]" <+> do
    prettyJson $ object
      [ "kind"    .= String "WrongNetwork"
      , "network" .= network
      , "addrs"   .= addrs
      ]
  pretty (Alonzo.WrongNetworkWithdrawal network addrs) =
    "[ih]" <+> do
    prettyJson $ object
      [ "kind"    .= String "WrongNetworkWithdrawal"
      , "network" .= network
      , "addrs"   .= addrs
      ]
  pretty (Alonzo.OutputTooSmallUTxO badOutputs) =
    "[ii]" <+> do
    prettyJson $ object
      [ "kind"    .= String "OutputTooSmallUTxO"
      , "outputs" .= badOutputs
      , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
      ]
  pretty (Alonzo.UtxosFailure predFailure) = PP.vsep
    [ "UtxosFailure:"
    , PP.nest 2 $ PP.pretty predFailure
    ]
  pretty (Alonzo.OutputBootAddrAttrsTooBig txouts) =
    "[ik]" <+> do
    prettyJson $ object
      [ "kind"    .= String "OutputBootAddrAttrsTooBig"
      , "outputs" .= txouts
      , "error"   .= String "The Byron address attributes are too big"
      ]
  pretty Alonzo.TriesToForgeADA =
    "[il]" <+> do
    prettyJson $ object
      [ "kind"  .= String "TriesToForgeADA"
      ]
  pretty (Alonzo.OutputTooBigUTxO badOutputs) =
    "[im]" <+> do
    prettyJson $ object
      [ "kind"    .= String "OutputTooBigUTxO"
      , "outputs" .= badOutputs
      , "error"   .= String "Too many asset ids in the tx output"
      ]
  pretty (Alonzo.InsufficientCollateral computedBalance suppliedFee) =
    "[in]" <+> do
    prettyJson $ object
      [ "kind"    .= String "InsufficientCollateral"
      , "balance" .= computedBalance
      , "txfee"   .= suppliedFee
      ]
  pretty (Alonzo.ScriptsNotPaidUTxO utxos) =
    "[io]" <+> do
    prettyJson $ object
      [ "kind"  .= String "ScriptsNotPaidUTxO"
      , "utxos" .= utxos
      ]
  pretty (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) =
    "[ip]" <+> do
    prettyJson $ object
      [ "kind"        .= String "ExUnitsTooBigUTxO"
      , "maxexunits"  .= pParamsMaxExUnits
      , "exunits"     .= suppliedExUnits
      ]
  pretty (Alonzo.CollateralContainsNonADA inputs) =
    "[iq]" <+> do
    prettyJson $ object
      [ "kind"    .= String "CollateralContainsNonADA"
      , "inputs"  .= inputs
      ]
  pretty (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) =
    "[ir]" <+> do
    prettyJson $ object
      [ "kind"            .= String "WrongNetworkInTxBody"
      , "networkid"       .= actualNetworkId
      , "txbodyNetworkId" .= netIdInTxBody
      ]
  pretty (Alonzo.OutsideForecast slotNum) =
    "[is]" <+> do
    prettyJson $ object
      [ "kind" .= String "OutsideForecast"
      , "slot" .= slotNum
      ]
  pretty (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) =
    "[it]" <+> do
    prettyJson $ object
      [ "kind"    .= String "TooManyCollateralInputs"
      , "max"     .= maxCollateralInputs
      , "inputs"  .= numberCollateralInputs
      ]
  pretty Alonzo.NoCollateralInputs =
    "[iu]" <+> do
    prettyJson $ object
      [ "kind"  .= String "NoCollateralInputs"
      ]

instance Pretty (Alonzo.UtxosPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  pretty (Alonzo.ValidationTagMismatch isValidating reason) = PP.vsep
    [ "ValidationTagMismatch:"
    , PP.nest 2 $ PP.vsep
      [ "isValidating:"
      , PP.nest 2 $ PP.pretty isValidating
      , "reason:"
      , PP.nest 2 $ PP.pretty reason
      ]
    ]
  pretty (Alonzo.CollectErrors errors) =
    "[j2]" <+> do
    prettyJson $ object
      [ "kind"    .= String "CollectErrors"
      , "errors"  .= errors
      ]
  pretty (Alonzo.UpdateFailure pFailure) =
    "[j3]" <+> PP.pretty pFailure

instance
  ( Ledger.Era era
  ) => Pretty (PpupPredicateFailure era) where
  pretty (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    "[k1]" <+> do
    prettyJson $ object
      [ "kind"  .= String "NonGenesisUpdatePPUP"
      , "keys"  .= proposalKeys Set.\\ genesisKeys
      ]
  pretty (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    "[k1]" <+> do
    prettyJson $ object
      [ "kind"          .= String "PPUpdateWrongEpoch"
      , "currentEpoch"  .= currEpoch
      , "intendedEpoch" .= intendedEpoch
      , "votingPeriod"  .= String (show votingPeriod)
      ]
  pretty (PVCannotFollowPPUP badPv) =
    "[k1]" <+> do
    prettyJson $ object
      [ "kind"                .= String "PVCannotFollowPPUP"
      , "badProtocolVersion" .= badPv
      ]

instance Pretty Alonzo.IsValid where
  pretty (Alonzo.IsValid v) = case v of
    True -> "Valid"
    False -> "NoValid"

instance Pretty Alonzo.TagMismatchDescription where
  pretty tmd = case tmd of
    Alonzo.PassedUnexpectedly -> "PassedUnexpectedly"
    Alonzo.FailedUnexpectedly forReasons -> PP.vsep
      [ "FailedUnexpectedly:"
      , PP.vsep $ ("-" <+>) . PP.hang 2 . PP.pretty <$> NEL.toList forReasons
      ]

instance Pretty Alonzo.FailureDescription where
  pretty = \case
    Alonzo.OnePhaseFailure t -> "OnePhaseFailure" <+> PP.pretty t
    Alonzo.PlutusFailure t bs -> PP.vsep
      [ "PlutusFailure"
      , PP.nest 2 $ PP.vsep
        [ "-" <+> PP.hang 2 do PP.pretty (Text.strip t)
        , "-" <+> PP.hang 2 do PP.pretty (Alonzo.debugPlutus (BSU.toString bs))
        ]
      ]

instance Pretty Alonzo.PlutusDebugInfo where
  pretty = \case
    Alonzo.DebugSuccess budget -> PP.vsep
      [ "DebugSuccess:"
      , PP.nest 2 $ PP.pretty budget
      ]
    Alonzo.DebugCannotDecode msg -> PP.vsep
      [ "DebugCannotDecode:"
      , PP.nest 2 $ PP.pretty msg
      ]
    Alonzo.DebugInfo texts e d -> PP.vsep
      [ "DebugInfo:"
      , "-" <+> PP.hang 2 do PP.pretty texts
      , "-" <+> PP.hang 2 do PP.pretty e
      , "-" <+> PP.hang 2 do PP.pretty d
      ]
    Alonzo.DebugBadHex msg -> PP.vsep
      [ "DebugBadHex:"
      , PP.nest 2 $ PP.pretty msg
      ]

instance Pretty Alonzo.PlutusDebug where
  pretty = \case
    Alonzo.PlutusDebugV1 costModel exUnits sbs ds protVer -> PP.vsep
      [ "PlutusDebugV1"
      , PP.nest 2 $ PP.vsep
        [ "costModel:"  <+> PP.hang 2 do PP.pretty costModel
        , "exUnits:"    <+> PP.hang 2 do PP.pretty exUnits
        , "sbs:"        <+> PP.hang 2 do PP.pretty (Text.decodeLatin1 (B16.encode (SBS.fromShort sbs)))
        , "scriptHash:" <+> PP.hang 2 do PP.pretty (scriptHashOf Alonzo.PlutusV1 sbs)
        , "dsSummary:"  <+> PP.hang 2 do prettyPlutusDatas ds
        , "protVer:"    <+> PP.hang 2 do PP.pretty protVer
        ]
      ]
    Alonzo.PlutusDebugV2 costModel exUnits sbs ds protVer -> PP.vsep
      [ "PlutusDebugV2"
      , PP.nest 2 $ PP.vsep
        [ "costModel:"  <+> PP.hang 2 do PP.pretty costModel
        , "exUnits:"    <+> PP.hang 2 do PP.pretty exUnits
        , "sbs:"        <+> PP.hang 2 do PP.pretty (Text.decodeLatin1 (B16.encode (SBS.fromShort sbs)))
        , "scriptHash:" <+> PP.hang 2 do PP.pretty (scriptHashOf Alonzo.PlutusV2 sbs)
        , "dsSummary:"  <+> PP.hang 2 do prettyPlutusDatas ds
        , "protVer:"    <+> PP.hang 2 do PP.pretty protVer
        ]
      ]

instance Pretty ProtVer where
  pretty _ = "ProtoVer" <+> "{..}"

instance Pretty Alonzo.PlutusError where
  pretty = \case
    Alonzo.PlutusErrorV1 evaluationError -> "PlutusErrorV1" <+> PP.pretty evaluationError
    Alonzo.PlutusErrorV2 evaluationError -> "PlutusErrorV2" <+> PP.pretty evaluationError

instance Pretty Alonzo.CostModel where
  pretty _ = "CostModel"

instance Pretty Ledger.ExUnits where
  pretty _ = "ExUnits"

prettyPlutusDatas :: [Plutus.Data] -> PP.Doc ann
prettyPlutusDatas = \case
  [dat, redeemer, info] -> PP.vsep
    [ "PlutusData:"
    , PP.nest 2 $ PP.vsep
      [ "data:"     <+> PP.hang 2 do PP.pretty dat
      , "redeemer:" <+> PP.hang 2 do PP.pretty redeemer
      , "info:"     <+> PP.hang 2 do prettyPlutusData info
      ]
    ]
  [dat, info] -> PP.vsep
    [ "PlutusData:"
    , PP.nest 2 $ PP.vsep
      [ "data:" <+> PP.hang 2 do PP.pretty dat
      , "info:" <+> PP.hang 2 do prettyPlutusData info
      ]
    ]
  ds -> PP.vsep
    [ "PlutusData"
    , PP.vsep ((\d -> PP.nest 2 $ "-" <+> PP.pretty d) <$> ds)
    ]

prettyPlutusData :: Plutus.Data -> PP.Doc ann
prettyPlutusData info = case PV1.fromData info of
  Nothing -> "NoScriptContext"
  Just PV1.ScriptContext { PV1.scriptContextTxInfo, PV1.scriptContextPurpose} -> PP.vsep
    [ "ScriptContext:"
    , "-" <+> PP.hang 2 do prettyTxInfo scriptContextTxInfo
    , "-" <+> PP.hang 2 do prettyScriptPurpose scriptContextPurpose
    ]

prettyTxInfo :: PV1.TxInfo -> PP.Doc ann
prettyTxInfo txInfo = PP.vsep
  [ "TxInfo:"
  , PP.nest 2 $ PP.vsep
    [ "txInfoInputs:"       <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoInputs txInfo))
    , "txInfoOutputs:"      <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoOutputs txInfo))
    , "txInfoFee:"          <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoFee txInfo))
    , "txInfoMint:"         <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoMint txInfo))
    , "txInfoDCert:"        <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoDCert txInfo))
    , "txInfoWdrl:"         <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoWdrl txInfo))
    , "txInfoValidRange:"   <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoValidRange txInfo))
    , "txInfoSignatories:"  <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoSignatories txInfo))
    , "txInfoData:"         <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoData txInfo))
    , "txInfoId:"           <+> PP.hang 2 do prettyJson (PV1.toData (PV1.txInfoId txInfo))
    ]
  ]

prettyJson :: ToJSON a => a -> PP.Doc ann
prettyJson = PP.pretty . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

prettyScriptPurpose :: PV1.ScriptPurpose -> PP.Doc ann
prettyScriptPurpose = \case
  PV1.Minting currencySymbol -> "Minting:"
    <+> PP.pretty (PV1.toData currencySymbol)
  PV1.Spending txOutRef -> "Spending:"
    <+> PP.pretty (PV1.toData txOutRef)
  PV1.Rewarding stakingCredential -> "Rewarding:"
    <+> PP.pretty (PV1.toData stakingCredential)
  PV1.Certifying dCert -> "Certifying:"
    <+> PP.pretty (PV1.toData dCert)

-------

scriptHashOf :: Alonzo.Language -> SBS.ShortByteString -> Text
scriptHashOf lang sbs = Text.pack $ Hash.hashToStringAsHex h
  where Ledger.ScriptHash h = case lang of
          Alonzo.PlutusV1 -> Ledger.hashScript @Consensus.StandardAlonzo (Ledger.PlutusScript lang sbs)
          Alonzo.PlutusV2 -> error "not implemented"

textShow :: Show a => a -> Text
textShow = Text.pack . CP.show
