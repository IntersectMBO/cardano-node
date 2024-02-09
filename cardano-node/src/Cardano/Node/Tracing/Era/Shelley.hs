{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Era.Shelley () where

import           Cardano.Api (textShow)
import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import           Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import qualified Cardano.Ledger.Alonzo.Plutus.Evaluate as Alonzo
import           Cardano.Ledger.Alonzo.Rules (AlonzoBbodyPredFailure, AlonzoUtxoPredFailure,
                   AlonzoUtxosPredFailure, AlonzoUtxowPredFailure (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import           Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import           Cardano.Ledger.BaseTypes (activeSlotLog, strictMaybeToMaybe)
import           Cardano.Ledger.Chain
import           Cardano.Ledger.Conway.Governance (govActionIdToText)
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Ledger.Shelley.API
import           Cardano.Ledger.Shelley.Rules
import           Cardano.Logging
import           Cardano.Node.Tracing.Render (renderMissingRedeemers, renderScriptHash,
                   renderScriptIntegrityHash)
import           Cardano.Protocol.TPraos.API (ChainTransitionError (ChainTransitionError))
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock, labBlockNo)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import           Cardano.Protocol.TPraos.Rules.OCert
import           Cardano.Protocol.TPraos.Rules.Overlay
import           Cardano.Protocol.TPraos.Rules.Prtcl
                   (PrtclPredicateFailure (OverlayFailure, UpdnFailure),
                   PrtlSeqFailure (WrongBlockNoPrtclSeq, WrongBlockSequencePrtclSeq, WrongSlotIntervalPrtclSeq))
import           Cardano.Protocol.TPraos.Rules.Tickn (TicknPredicateFailure)
import           Cardano.Protocol.TPraos.Rules.Updn (UpdnPredicateFailure)
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosChainSelectView (..))
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import qualified Ouroboros.Consensus.Shelley.Protocol.Praos as Praos
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.ByteString.Base16 as B16
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

{- HLINT ignore "Use :" -}

--
-- | instances of @LogFormatting@
--
-- NOTE: this list is sorted in roughly topological order.

instance
  ( ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock protocol era)))
  , ShelleyBasedEra era
  ) => LogFormatting (GenTx (ShelleyBlock protocol era)) where
  forMachine dtal tx =
    mconcat $
        ( "txid" .= txId tx )
      : [ "tx"   .= condense tx | dtal == DDetailed ]

instance LogFormatting (Set (Credential 'Staking StandardCrypto)) where
  forMachine _dtal creds =
    mconcat [ "kind" .= String "StakeCreds"
             , "stakeCreds" .= map toJSON (Set.toList creds)
             ]

instance
  ( LogFormatting (PredicateFailure (Ledger.EraRule "DELEG" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "POOL" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "GOVCERT" era))
  ) => LogFormatting (Conway.ConwayCertPredFailure era) where
    forMachine dtal = mconcat . \case
      Conway.DelegFailure f ->
        [ "kind" .= String "DelegFailure " , "failure" .= forMachine dtal f ]
      Conway.PoolFailure f ->
        [ "kind" .= String "PoolFailure" , "failure" .= forMachine dtal f ]
      Conway.GovCertFailure f ->
        [ "kind" .= String "GovCertFailure" , "failure" .= forMachine dtal f ]

instance LogFormatting (Conway.ConwayGovCertPredFailure era) where
  forMachine _dtal = mconcat . \case
    Conway.ConwayDRepAlreadyRegistered credential ->
      [ "kind" .= String "ConwayDRepAlreadyRegistered"
      , "credential" .= String (textShow credential)
      , "error" .= String "DRep is already registered"
      ]
    Conway.ConwayDRepNotRegistered credential ->
      [ "kind" .= String "ConwayDRepNotRegistered"
      , "credential" .= String (textShow credential)
      , "error" .= String "DRep is not registered"
      ]
    Conway.ConwayDRepIncorrectDeposit givenCoin expectedCoin ->
      [ "kind" .= String "ConwayDRepIncorrectDeposit"
      , "givenCoin" .= givenCoin
      , "expectedCoin" .= expectedCoin
      , "error" .= String "DRep delegation has incorrect deposit"
      ]
    Conway.ConwayCommitteeHasPreviouslyResigned kHash ->
      [ "kind" .= String "ConwayCommitteeHasPreviouslyResigned"
      , "credential" .= String (textShow kHash)
      , "error" .= String "Committee has resigned"
      ]


instance LogFormatting (Conway.ConwayDelegPredFailure era) where
  forMachine _dtal = mconcat . \case
    Conway.IncorrectDepositDELEG coin ->
      [ "kind" .= String "IncorrectDepositDELEG"
      , "amount" .= coin
      , "error" .= String "Incorrect deposit amount"
      ]
    Conway.StakeKeyRegisteredDELEG credential ->
      [ "kind" .= String "StakeKeyRegisteredDELEG"
      , "credential" .= String (textShow credential)
      , "error" .= String "Stake key already registered"
      ]
    Conway.StakeKeyNotRegisteredDELEG credential ->
      [ "kind" .= String "StakeKeyNotRegisteredDELEG"
      , "amount" .= String (textShow credential)
      , "error" .= String "Stake key not registered"
      ]
    Conway.StakeKeyHasNonZeroRewardAccountBalanceDELEG coin ->
      [ "kind" .= String "StakeKeyHasNonZeroRewardAccountBalanceDELEG"
      , "amount" .= coin
      , "error" .= String "Stake key has non-zero account balance"
      ]
    Conway.DRepAlreadyRegisteredForStakeKeyDELEG credential ->
      [ "kind" .= String "DRepAlreadyRegisteredForStakeKeyDELEG"
      , "amount" .= String (textShow credential)
      , "error" .= String "DRep already registered for the stake key"
      ]

instance
  ( ShelleyCompatible protocol era
  ) => LogFormatting (Header (ShelleyBlock protocol era)) where
  forMachine _dtal b = mconcat
        [ "kind" .= String "ShelleyBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]

instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (ShelleyUTXO era))
  , LogFormatting (PredicateFailure (ShelleyUTXOW era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "LEDGER" era))
  ) => LogFormatting (ApplyTxError era) where
  forMachine dtal (ApplyTxError predicateFailures) =
    mconcat $ map (forMachine dtal) predicateFailures

instance
  ( Ledger.Crypto era
  ) => LogFormatting (TPraosCannotForge era) where
  forMachine _dtal (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) =
    mconcat
      [ "kind" .= String "TPraosCannotForgeKeyNotUsableYet"
      , "keyStart" .= keyStartPeriod
      , "wallClock" .= wallClockPeriod
      ]
  forMachine _dtal (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) =
    mconcat
      [ "kind" .= String "TPraosCannotLeadWrongVRF"
      , "expected" .= genDlgVRFHash
      , "actual" .= coreNodeVRFHash
      ]


instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (ShelleyUTXO era))
  , LogFormatting (PredicateFailure (ShelleyUTXOW era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "BBODY" era))
  ) => LogFormatting (ShelleyLedgerError era) where
  forMachine dtal (BBodyError (BlockTransitionError fs)) =
    mconcat [ "kind" .= String "BBodyError"
             , "failures" .= map (forMachine dtal) fs
             ]

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Ledger.PParamsUpdate era)
  ) => LogFormatting (ShelleyLedgerUpdate era) where
  forMachine dtal (ShelleyUpdatedProtocolUpdates updates) =
    mconcat [ "kind" .= String "ShelleyUpdatedProtocolUpdates"
             , "updates" .= map (forMachine dtal) updates
             ]

instance
  ( Ledger.Era era
  , ToJSON (Ledger.PParamsUpdate era)
  ) => LogFormatting (ProtocolUpdate era) where
  forMachine dtal ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} =
    mconcat [ "proposal" .= forMachine dtal protocolUpdateProposal
             , "state"    .= forMachine dtal protocolUpdateState
             ]

instance
  ( ToJSON (Ledger.PParamsUpdate era)
  ) => LogFormatting (UpdateProposal era) where
  forMachine _dtal UpdateProposal{proposalParams, proposalVersion, proposalEpoch} =
    mconcat [ "params"  .= proposalParams
             , "version" .= proposalVersion
             , "epoch"   .= proposalEpoch
             ]

instance
  ( Ledger.Crypto crypto
  ) => LogFormatting (UpdateState crypto) where
  forMachine _dtal UpdateState{proposalVotes, proposalReachedQuorum} =
    mconcat [ "proposal"      .= proposalVotes
             , "reachedQuorum" .= proposalReachedQuorum
             ]

instance
  ( Ledger.Crypto crypto
  ) => LogFormatting (ChainTransitionError crypto) where
  forMachine dtal (ChainTransitionError fs) =
    mconcat [ "kind" .= String "ChainTransitionError"
             , "failures" .= map (forMachine dtal) fs
             ]

instance LogFormatting ChainPredicateFailure where
  forMachine _dtal (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) =
    mconcat [ "kind" .= String "HeaderSizeTooLarge"
             , "headerSize" .= hdrSz
             , "maxHeaderSize" .= maxHdrSz
             ]
  forMachine _dtal (BlockSizeTooLargeCHAIN blkSz maxBlkSz) =
    mconcat [ "kind" .= String "BlockSizeTooLarge"
             , "blockSize" .= blkSz
             , "maxBlockSize" .= maxBlkSz
             ]
  forMachine _dtal (ObsoleteNodeCHAIN currentPtcl supportedPtcl) =
    mconcat [ "kind" .= String "ObsoleteNode"
             , "explanation" .= String explanation
             , "currentProtocol" .= currentPtcl
             , "supportedProtocol" .= supportedPtcl ]
      where
        explanation = mconcat
          [ "A scheduled major protocol version change (hard fork) "
          , "has taken place on the chain, but this node does not "
          , "understand the new major protocol version. This node "
          , "must be upgraded before it can continue with the new "
          , "protocol version."
          ]

instance LogFormatting (PrtlSeqFailure crypto) where
  forMachine _dtal (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mconcat [ "kind" .= String "WrongSlotInterval"
             , "lastSlot" .= lastSlot
             , "currentSlot" .= currSlot
             ]
  forMachine _dtal (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mconcat [ "kind" .= String "WrongBlockNo"
             , "lastAppliedBlockNo" .= showLastAppBlockNo lab
             , "currentBlockNo" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  forMachine _dtal (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mconcat [ "kind" .= String "WrongBlockSequence"
             , "lastAppliedBlockHash" .= String (textShow lastAppliedHash)
             , "currentBlockHash" .= String (textShow currentHash)
             ]

instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (ShelleyUTXO era))
  , LogFormatting (PredicateFailure (ShelleyUTXOW era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "LEDGER" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "LEDGERS" era))
  ) => LogFormatting (ShelleyBbodyPredFailure era) where
  forMachine _dtal (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) =
    mconcat [ "kind" .= String "WrongBlockBodySizeBBODY"
             , "actualBlockBodySize" .= actualBodySz
             , "claimedBlockBodySize" .= claimedBodySz
             ]
  forMachine _dtal (InvalidBodyHashBBODY actualHash claimedHash) =
    mconcat [ "kind" .= String "InvalidBodyHashBBODY"
             , "actualBodyHash" .= textShow actualHash
             , "claimedBodyHash" .= textShow claimedHash
             ]
  forMachine dtal (LedgersFailure f) = forMachine dtal f


instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (ShelleyUTXO era))
  , LogFormatting (PredicateFailure (ShelleyUTXOW era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "LEDGER" era))
  ) => LogFormatting (ShelleyLedgersPredFailure era) where
  forMachine dtal (LedgerFailure f) = forMachine dtal f


instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Ledger.AuxiliaryDataHash (Ledger.EraCrypto era))
  , LogFormatting (PredicateFailure (ShelleyUTXO era))
  , LogFormatting (PredicateFailure (ShelleyUTXOW era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "DELEGS" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "UTXOW" era))
  ) => LogFormatting (ShelleyLedgerPredFailure era) where
  forMachine dtal = \case
    UtxowFailure f -> forMachine dtal f
    DelegsFailure f -> forMachine dtal f

instance
  ( Api.ShelleyLedgerEra era ~ ledgerera
  , Api.IsShelleyBasedEra era
  , Consensus.ShelleyBasedEra ledgerera
  , Ledger.EraCrypto ledgerera ~ StandardCrypto
  , LogFormatting (PPUPPredFailure ledgerera)
  , LogFormatting (PredicateFailure (Ledger.EraRule "UTXO" ledgerera))
  ) => LogFormatting (AlonzoUtxowPredFailure ledgerera) where
  forMachine dtal (ShelleyInAlonzoUtxowPredFailure utxoPredFail) =
    forMachine dtal utxoPredFail
  forMachine _ (MissingRedeemers scripts) =
    mconcat [ "kind" .= String "MissingRedeemers"
             , "scripts" .= renderMissingRedeemers Api.shelleyBasedEra scripts
             ]
  forMachine _ (MissingRequiredDatums required received) =
    mconcat [ "kind" .= String "MissingRequiredDatums"
             , "required" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList required)
             , "received" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList received)
             ]
  forMachine _ (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) =
    mconcat [ "kind" .= String "PPViewHashesDontMatch"
             , "fromTxBody" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashInTxBody)
             , "fromPParams" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashFromPParams)
             ]
  forMachine _ (MissingRequiredSigners missingKeyWitnesses) =
    mconcat [ "kind" .= String "MissingRequiredSigners"
             , "witnesses" .= Set.toList missingKeyWitnesses
             ]
  forMachine _ (UnspendableUTxONoDatumHash txins) =
    mconcat [ "kind" .= String "MissingRequiredSigners"
             , "txins" .= Set.toList txins
             ]
  forMachine _ (NotAllowedSupplementalDatums disallowed acceptable) =
    mconcat [ "kind" .= String "NotAllowedSupplementalDatums"
             , "disallowed" .= Set.toList disallowed
             , "acceptable" .= Set.toList acceptable
             ]
  forMachine _ (ExtraRedeemers rdmrs) =
    Api.caseShelleyToMaryOrAlonzoEraOnwards
      (const mempty)
      (\alonzoOnwards ->
         mconcat
           [ "kind" .= String "ExtraRedeemers"
           , "rdmrs" .=  map (Api.toScriptIndex alonzoOnwards) rdmrs
           ]
      )
      (Api.shelleyBasedEra :: Api.ShelleyBasedEra era)

instance
  ( Consensus.ShelleyBasedEra era
  , Ledger.EraCrypto era ~ StandardCrypto
  , ToJSON (Ledger.AuxiliaryDataHash (Ledger.EraCrypto era))
  , LogFormatting (PredicateFailure (ShelleyUTXO era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "UTXO" era))
  ) => LogFormatting (ShelleyUtxowPredFailure era) where
  forMachine _dtal (InvalidWitnessesUTXOW wits') =
    mconcat [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalidWitnesses" .= map textShow wits'
             ]
  forMachine _dtal (MissingVKeyWitnessesUTXOW wits') =
    mconcat [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missingWitnesses" .= wits'
             ]
  forMachine _dtal (MissingScriptWitnessesUTXOW missingScripts) =
    mconcat [ "kind" .= String "MissingScriptWitnessesUTXOW"
             , "missingScripts" .= missingScripts
             ]
  forMachine _dtal (ScriptWitnessNotValidatingUTXOW failedScripts) =
    mconcat [ "kind" .= String "ScriptWitnessNotValidatingUTXOW"
             , "failedScripts" .= failedScripts
             ]
  forMachine dtal (UtxoFailure f) = forMachine dtal f
  forMachine _dtal (MIRInsufficientGenesisSigsUTXOW genesisSigs) =
    mconcat [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW"
             , "genesisSigs" .= genesisSigs
             ]
  forMachine _dtal (MissingTxBodyMetadataHash metadataHash) =
    mconcat [ "kind" .= String "MissingTxBodyMetadataHash"
             , "metadataHash" .= metadataHash
             ]
  forMachine _dtal (MissingTxMetadata txBodyMetadataHash) =
    mconcat [ "kind" .= String "MissingTxMetadata"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             ]
  forMachine _dtal (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    mconcat [ "kind" .= String "ConflictingMetadataHash"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             , "fullMetadataHash" .= fullMetadataHash
             ]
  forMachine _dtal InvalidMetadata =
    mconcat [ "kind" .= String "InvalidMetadata"
             ]
  forMachine _dtal (ExtraneousScriptWitnessesUTXOW scriptHashes) =
    mconcat [ "kind" .= String "ExtraneousScriptWitnessesUTXOW"
             , "scriptHashes" .= Set.map renderScriptHash scriptHashes
             ]

instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PPUPPredFailure era)
  ) => LogFormatting (ShelleyUtxoPredFailure era) where
  forMachine _dtal (BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (ExpiredUTxO ttl slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  forMachine _dtal (MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  forMachine _dtal (OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
            , "outputs" .= badOutputs
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
            ]
  forMachine _dtal (OutputBootAddrAttrsTooBig badOutputs) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (FeeTooSmallUTxO minfee txfee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  forMachine _dtal (ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine dtal (UpdateFailure f) = forMachine dtal f

  forMachine _dtal (WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON Allegra.ValidityInterval
  , LogFormatting (PPUPPredFailure era)
  ) => LogFormatting (AllegraUtxoPredFailure era) where
  forMachine _dtal (Allegra.BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (Allegra.OutsideValidityIntervalUTxO validityInterval slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  forMachine _dtal (Allegra.MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  forMachine _dtal Allegra.InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (Allegra.FeeTooSmallUTxO minfee txfee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  forMachine _dtal (Allegra.ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine _dtal (Allegra.WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (Allegra.WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  forMachine _dtal (Allegra.OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
            , "outputs" .= badOutputs
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
            ]
  forMachine dtal (Allegra.UpdateFailure f) = forMachine dtal f
  forMachine _dtal (Allegra.OutputBootAddrAttrsTooBig badOutputs) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal Allegra.TriesToForgeADA =
    mconcat [ "kind" .= String "TriesToForgeADA" ]
  forMachine _dtal (Allegra.OutputTooBigUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> textShow consumed <> " but produced " <> textShow produced

instance
  ( Ledger.Crypto (Ledger.EraCrypto era)
  ) => LogFormatting (ShelleyPpupPredFailure era) where
  forMachine _dtal (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mconcat [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  forMachine _dtal (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mconcat [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (textShow votingPeriod)
             ]
  forMachine _dtal (PVCannotFollowPPUP badPv) =
    mconcat [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (Ledger.EraRule "DELPL" era))
  ) => LogFormatting (ShelleyDelegsPredFailure era) where
  forMachine _dtal (DelegateeNotRegisteredDELEG targetPool) =
    mconcat [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  forMachine _dtal (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mconcat [ "kind" .= String "WithdrawalsNotInRewardsCERTS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  forMachine dtal (DelplFailure f) = forMachine dtal f


instance
  ( LogFormatting (PredicateFailure (Ledger.EraRule "POOL" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "DELEG" era))
  , Crypto.HashAlgorithm (Ledger.HASH (Ledger.EraCrypto era))
  ) => LogFormatting (ShelleyDelplPredFailure era) where
  forMachine dtal (PoolFailure f)  = forMachine dtal f
  forMachine dtal (DelegFailure f) = forMachine dtal f

instance
  ( Crypto.HashAlgorithm (Ledger.HASH (Ledger.EraCrypto era))
  ) => LogFormatting (ShelleyDelegPredFailure era) where
  forMachine _dtal (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) =
    mconcat [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential already registered"
             ]
  forMachine _dtal (StakeKeyInRewardsDELEG alreadyRegistered) =
    mconcat [ "kind" .= String "StakeKeyInRewardsDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential registered in rewards map"
             ]
  forMachine _dtal (StakeKeyNotRegisteredDELEG notRegistered) =
    mconcat [ "kind" .= String "StakeKeyNotRegisteredDELEG"
             , "credential" .= String (textShow notRegistered)
             , "error" .= String "Staking credential not registered"
             ]
  forMachine _dtal (StakeKeyNonZeroAccountBalanceDELEG remBalance) =
    mconcat [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG"
             , "remainingBalance" .= remBalance
             ]
  forMachine _dtal (StakeDelegationImpossibleDELEG unregistered) =
    mconcat [ "kind" .= String "StakeDelegationImpossibleDELEG"
             , "credential" .= String (textShow unregistered)
             , "error" .= String "Cannot delegate this stake credential because it is not registered"
             ]
  forMachine _dtal WrongCertificateTypeDELEG =
    mconcat [ "kind" .= String "WrongCertificateTypeDELEG" ]
  forMachine _dtal (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) =
    mconcat [ "kind" .= String "GenesisKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key is not in the delegation mapping"
             ]
  forMachine _dtal (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) =
    mconcat [ "kind" .= String "DuplicateGenesisDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key has already been delegated to"
             ]
  forMachine _dtal (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) =
    mconcat [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "neededAmount" .= neededMirAmount
             , "reserves" .= reserves
             ]
  forMachine _dtal (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) =
    mconcat [ "kind" .= String "MIRCertificateTooLateinEpochDELEG"
             , "currentSlotNo" .= currSlot
             , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
             ]
  forMachine _dtal (DuplicateGenesisVRFDELEG vrfKeyHash) =
    mconcat [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]
  forMachine _dtal MIRTransferNotCurrentlyAllowed =
    mconcat [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
             ]
  forMachine _dtal MIRNegativesNotCurrentlyAllowed =
    mconcat [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
             ]
  forMachine _dtal (InsufficientForTransferDELEG mirpot attempted available) =
    mconcat [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "attempted" .= attempted
             , "available" .= available
             ]
  forMachine _dtal MIRProducesNegativeUpdate =
    mconcat [ "kind" .= String "MIRProducesNegativeUpdate"
             ]
  forMachine _dtal (MIRNegativeTransfer mirpot coin) =
    mconcat [ "kind" .= String "MIRProducesNegativeUpdate"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "coin" .= coin
             ]

instance LogFormatting (ShelleyPoolPredFailure era) where
  forMachine _dtal (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) =
    mconcat [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL"
             , "unregisteredKeyHash" .= String (textShow unregStakePool)
             , "error" .= String "This stake pool key hash is unregistered"
             ]
  forMachine _dtal (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) =
    mconcat [ "kind" .= String "StakePoolRetirementWrongEpochPOOL"
             , "currentEpoch" .= String (textShow currentEpoch)
             , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
             , "maxEpochForRetirement" .= String (textShow maxRetireEpoch)
             ]
  forMachine _dtal (StakePoolCostTooLowPOOL certCost protCost) =
    mconcat [ "kind" .= String "StakePoolCostTooLowPOOL"
             , "certificateCost" .= String (textShow certCost)
             , "protocolParCost" .= String (textShow protCost)
             , "error" .= String "The stake pool cost is too low"
             ]
  forMachine _dtal (PoolMedataHashTooBig poolID hashSize) =
    mconcat [ "kind" .= String "PoolMedataHashTooBig"
             , "hashSize" .= String (textShow poolID)
             , "poolID" .= String (textShow hashSize)
             , "error" .= String "The stake pool metadata hash is too large"
             ]

  forMachine _dtal (WrongNetworkPOOL networkId listedNetworkId poolId) =
    mconcat [ "kind" .= String "WrongNetworkPOOL"
             , "networkId" .= String (textShow networkId)
             , "listedNetworkId" .= String (textShow listedNetworkId)
             , "poolId" .= String (textShow poolId)
             , "error" .= String "Wrong network ID in pool registration certificate"
             ]


instance
  ( LogFormatting (PredicateFailure (Ledger.EraRule "NEWEPOCH" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "RUPD" era))
  ) => LogFormatting (ShelleyTickPredFailure era) where
  forMachine dtal (NewEpochFailure f) = forMachine dtal f
  forMachine dtal (RupdFailure f)     = forMachine dtal f

instance LogFormatting TicknPredicateFailure where
  forMachine _dtal x = case x of {} -- no constructors

instance
  ( LogFormatting (PredicateFailure (Ledger.EraRule "EPOCH" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "MIR" era))
  ) => LogFormatting (ShelleyNewEpochPredFailure era) where
  forMachine dtal (EpochFailure f) = forMachine dtal f
  forMachine dtal (MirFailure f) = forMachine dtal f
  forMachine _dtal (CorruptRewardUpdate update) =
    mconcat [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (textShow update) ]


instance
  ( LogFormatting (PredicateFailure (Ledger.EraRule "POOLREAP" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "SNAP" era))
  , LogFormatting (UpecPredFailure era)
  ) => LogFormatting (ShelleyEpochPredFailure era) where
  forMachine dtal (PoolReapFailure f) = forMachine dtal f
  forMachine dtal (SnapFailure f)     = forMachine dtal f
  forMachine dtal (UpecFailure f)     = forMachine dtal f


instance LogFormatting (ShelleyPoolreapPredFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance LogFormatting (ShelleySnapPredFailure era) where
  forMachine _dtal x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance LogFormatting (ShelleyNewppPredFailure era) where
  forMachine _dtal (UnexpectedDepositPot outstandingDeposits depositPot) =
    mconcat [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance LogFormatting (ShelleyMirPredFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance LogFormatting (ShelleyRupdPredFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance
  ( Ledger.Crypto crypto
  ) => LogFormatting (PrtclPredicateFailure crypto) where
  forMachine  dtal (OverlayFailure f) = forMachine dtal f
  forMachine  dtal (UpdnFailure f)    = forMachine dtal f


instance
  ( Ledger.Crypto crypto
  ) => LogFormatting (OverlayPredicateFailure crypto) where
  forMachine _dtal (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) =
    mconcat [ "kind" .= String "UnknownGenesisKeyOVERLAY"
             , "unknownKeyHash" .= String (textShow genKeyHash)
             ]
  forMachine _dtal (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mconcat [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "leaderElectionValue" .= String (textShow leaderElecVal)
             ]
  forMachine _dtal (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mconcat [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "blockNonce" .= String (textShow blockNonce)
             ]
  forMachine _dtal (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) =
    mconcat [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "poolHash" .= textShow issuerHash
             , "registeredVRFKeHash" .= textShow regVRFKeyHash
             , "unregisteredVRFKeyHash" .= textShow unregVRFKeyHash
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  forMachine _dtal (VRFKeyUnknown (KeyHash kHash)) =
    mconcat [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "keyHash" .= String (textShow kHash)
             ]
  forMachine _dtal (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mconcat [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "leaderElectionValue" .= String (textShow leadElecVal)
             , "delegationPoolWeight" .= String (textShow weightOfDelegPool)
             , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
             ]
  forMachine _dtal (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mconcat [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= String (textShow notActiveSlotNo)
             ]
  forMachine _dtal (WrongGenesisColdKeyOVERLAY actual expected) =
    mconcat [ "kind" .= String "WrongGenesisColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  forMachine _dtal (WrongGenesisVRFKeyOVERLAY issuer actual expected) =
    mconcat [ "kind" .= String "WrongGenesisVRFKeyOVERLAY"
             , "issuer" .= issuer
             , "actual" .= actual
             , "expected" .= expected ]
  forMachine dtal (OcertFailure f) = forMachine dtal f


instance LogFormatting (OcertPredicateFailure crypto) where
  forMachine _dtal (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) =
    mconcat [ "kind" .= String "KESBeforeStartOCERT"
            , "opCertKESStartPeriod" .= String (textShow oCertstart)
            , "currentKESPeriod" .= String (textShow current)
            , "error" .= String
              ( mconcat
                [ "Your operational certificate's KES start period "
                , "is before the KES current period."
                ]
              )
            ]
  forMachine _dtal (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
    mconcat [ "kind" .= String "KESAfterEndOCERT"
            , "currentKESPeriod" .= String (textShow current)
            , "opCertKESStartPeriod" .= String (textShow oCertstart)
            , "maxKESEvolutions" .= String  (textShow maxKESEvolutions)
            , "error" .= String
              ( mconcat
                [ "The operational certificate's KES start period is "
                , "greater than the max number of KES + the KES current period"
                ]
              )
            ]
  forMachine _dtal (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
    mconcat [ "kind" .= String "CounterTooSmallOCert"
            , "currentKESCounter" .= String (textShow currentKESCounter)
            , "lastKESCounter" .= String (textShow lastKEScounterUsed)
            , "error" .= String
              ( mconcat
                [ "The operational certificate's last KES counter is greater "
                , "than the current KES counter."
                ]
              )
            ]
  forMachine _dtal (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) =
    mconcat [ "kind" .= String "InvalidSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertKESStartPeriod)
             , "opCertCounter" .= String (textShow oCertCounter)
             ]
  forMachine _dtal (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) =
    mconcat [ "kind" .= String "InvalidKesSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow startKESPeriod)
             , "opCertKESCurrentPeriod" .= String (textShow currKESPeriod)
             , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
             , "error" .= err ]
  forMachine _dtal (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) =
    mconcat [ "kind" .= String "NoCounterForKeyHashOCERT"
             , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
             , "error" .= String "A counter was not found for this stake pool key hash"
             ]


instance LogFormatting (UpdnPredicateFailure crypto) where
  forMachine _dtal x = case x of {} -- no constructors

instance LogFormatting (ShelleyUpecPredFailure era) where
  forMachine _dtal (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) =
    mconcat [ "kind" .= String "UnexpectedDepositPot"
             , "totalOutstanding" .=  String (textShow totalOutstanding)
             , "depositPot" .= String (textShow depositPot)
             ]

--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------
instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (Ledger.EraRule "UTXOS" era))
  ) => LogFormatting (AlonzoUtxoPredFailure era) where
  forMachine _dtal (Alonzo.BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validtyInterval
             , "slot" .= slot
             ]
  forMachine _dtal (Alonzo.MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize
             ]
  forMachine _dtal Alonzo.InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (Alonzo.FeeTooSmallUTxO minfee currentFee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= currentFee
             ]
  forMachine _dtal (Alonzo.ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine _dtal (Alonzo.WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (Alonzo.WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (Alonzo.OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
            , "outputs" .= badOutputs
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
            ]
  forMachine dtal (Alonzo.UtxosFailure predFailure) =
    forMachine dtal predFailure
  forMachine _dtal (Alonzo.OutputBootAddrAttrsTooBig txouts) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= txouts
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal Alonzo.TriesToForgeADA =
    mconcat [ "kind" .= String "TriesToForgeADA" ]
  forMachine _dtal (Alonzo.OutputTooBigUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]
  forMachine _dtal (Alonzo.InsufficientCollateral computedBalance suppliedFee) =
    mconcat [ "kind" .= String "InsufficientCollateral"
             , "balance" .= computedBalance
             , "txfee" .= suppliedFee
             ]
  forMachine _dtal (Alonzo.ScriptsNotPaidUTxO utxos) =
    mconcat [ "kind" .= String "ScriptsNotPaidUTxO"
             , "utxos" .= utxos
             ]
  forMachine _dtal (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) =
    mconcat [ "kind" .= String "ExUnitsTooBigUTxO"
             , "maxexunits" .= pParamsMaxExUnits
             , "exunits" .= suppliedExUnits
             ]
  forMachine _dtal (Alonzo.CollateralContainsNonADA inputs) =
    mconcat [ "kind" .= String "CollateralContainsNonADA"
             , "inputs" .= inputs
             ]
  forMachine _dtal (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) =
    mconcat [ "kind" .= String "WrongNetworkInTxBody"
             , "networkid" .= actualNetworkId
             , "txbodyNetworkId" .= netIdInTxBody
             ]
  forMachine _dtal (Alonzo.OutsideForecast slotNum) =
    mconcat [ "kind" .= String "OutsideForecast"
             , "slot" .= slotNum
             ]
  forMachine _dtal (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) =
    mconcat [ "kind" .= String "TooManyCollateralInputs"
             , "max" .= maxCollateralInputs
             , "inputs" .= numberCollateralInputs
             ]
  forMachine _dtal Alonzo.NoCollateralInputs =
    mconcat [ "kind" .= String "NoCollateralInputs" ]

instance
  ( ToJSON (Alonzo.CollectError ledgerera)
  , LogFormatting (PPUPPredFailure ledgerera)
  ) => LogFormatting (AlonzoUtxosPredFailure ledgerera) where
  forMachine _ (Alonzo.ValidationTagMismatch isValidating reason) =
    mconcat [ "kind" .= String "ValidationTagMismatch"
             , "isvalidating" .= isValidating
             , "reason" .= reason
             ]
  forMachine _ (Alonzo.CollectErrors errors) =
    mconcat [ "kind" .= String "CollectErrors"
             , "errors" .= errors
             ]
  forMachine dtal (Alonzo.UpdateFailure pFailure) =
    forMachine dtal pFailure

instance
  ( Ledger.Era era
  , Show (PredicateFailure (Ledger.EraRule "LEDGERS" era))
  ) => LogFormatting (AlonzoBbodyPredFailure era) where
  forMachine _ err = mconcat [ "kind" .= String "AlonzoBbodyPredFail"
                            , "error" .= String (textShow err)
                            ]
--------------------------------------------------------------------------------
-- Babbage related
--------------------------------------------------------------------------------


instance
  ( Ledger.Era era
  , LogFormatting (AlonzoUtxoPredFailure era)
  , ToJSON (Ledger.TxOut era)
  ) => LogFormatting (BabbageUtxoPredFailure era) where
  forMachine v err =
    case err of
      Babbage.AlonzoInBabbageUtxoPredFailure alonzoFail ->
        forMachine v alonzoFail

      Babbage.IncorrectTotalCollateralField provided declared ->
        mconcat [ "kind" .= String "IncorrectTotalCollateralField"
                , "collateralProvided" .= provided
                , "collateralDeclared" .= declared
                ]
      -- The transaction contains outputs that are too small
      Babbage.BabbageOutputTooSmallUTxO outputs ->
        mconcat [ "kind" .= String "OutputTooSmall"
                , "outputs" .= outputs
                ]

instance
  ( Api.ShelleyLedgerEra era ~ ledgerera
  , Api.IsShelleyBasedEra era
  , Ledger.Era ledgerera
  , Ledger.EraCrypto ledgerera ~ StandardCrypto
  , ShelleyBasedEra ledgerera
  , LogFormatting (PPUPPredFailure ledgerera)
  , LogFormatting (ShelleyUtxowPredFailure ledgerera)
  , LogFormatting (PredicateFailure (Ledger.EraRule "UTXO" ledgerera))
  ) => LogFormatting (BabbageUtxowPredFailure ledgerera) where
  forMachine v err =
    case err of
      Babbage.AlonzoInBabbageUtxowPredFailure alonzoFail ->
        forMachine v alonzoFail
      Babbage.UtxoFailure utxoFail ->
        forMachine v utxoFail
      -- TODO: Plutus team needs to expose a better error type.
      Babbage.MalformedScriptWitnesses s ->
        mconcat [ "kind" .= String "MalformedScriptWitnesses"
                , "scripts" .= s
                ]
      Babbage.MalformedReferenceScripts s ->
        mconcat [ "kind" .= String "MalformedReferenceScripts"
                , "scripts" .= s
                ]
--------------------------------------------------------------------------------
-- Conway related
--------------------------------------------------------------------------------

instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (Ledger.EraRule "UTXOW" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "GOV" era))
  , LogFormatting (PredicateFailure (Ledger.EraRule "CERTS" era))
  , LogFormatting (Set (Credential 'Staking (Ledger.EraCrypto era)))
  ) => LogFormatting (Conway.ConwayLedgerPredFailure era) where
  forMachine v (Conway.ConwayUtxowFailure f) = forMachine v f
  forMachine v (Conway.ConwayCertsFailure f) = forMachine v f
  forMachine v (Conway.ConwayGovFailure f) = forMachine v f
  forMachine v (Conway.ConwayWdrlNotDelegatedToDRep f) = forMachine v f
  forMachine _ (Conway.ConwayTreasuryValueMismatch actual inTx) =
    mconcat [ "kind" .= String "ConwayTreasuryValueMismatch"
            , "actual" .= actual
            , "submittedInTx" .= inTx
            ]

instance
  ( Consensus.ShelleyBasedEra era
  ) => LogFormatting (Conway.ConwayGovPredFailure era) where
  forMachine _ (Conway.GovActionsDoNotExist govActionIds) =
    mconcat [ "kind" .= String "GovActionsDoNotExist"
            , "govActionId" .= map govActionIdToText (NonEmpty.toList govActionIds)
            ]
  forMachine _ (Conway.MalformedProposal govAction) =
    mconcat [ "kind" .= String "MalformedProposal"
            , "govAction" .= govAction
            ]
  forMachine _ (Conway.ProposalProcedureNetworkIdMismatch rewardAcnt network) =
    mconcat [ "kind" .= String "ProposalProcedureNetworkIdMismatch"
            , "rewardAccount" .= toJSON rewardAcnt
            , "expectedNetworkId" .= toJSON network
            ]
  forMachine _ (Conway.TreasuryWithdrawalsNetworkIdMismatch rewardAcnts network) =
    mconcat [ "kind" .= String "TreasuryWithdrawalsNetworkIdMismatch"
            , "rewardAccounts" .= toJSON rewardAcnts
            , "expectedNetworkId" .= toJSON network
            ]
  forMachine _ (Conway.ProposalDepositIncorrect deposit expectedDeposit) =
    mconcat [ "kind" .= String "ProposalDepositIncorrect"
            , "deposit" .= deposit
            , "expectedDeposit" .= expectedDeposit
            ]
  forMachine _ (Conway.DisallowedVoters govActionIdToVoter) =
    mconcat [ "kind" .= String "DisallowedVoters"
            , "govActionIdToVoter" .= NonEmpty.toList govActionIdToVoter
            ]
  forMachine _ (Conway.ConflictingCommitteeUpdate creds) =
    mconcat [ "kind" .= String "ConflictingCommitteeUpdate"
            , "credentials" .= creds
            ]
  forMachine _ (Conway.ExpirationEpochTooSmall credsToEpoch) =
    mconcat [ "kind" .= String "ExpirationEpochTooSmall"
            , "credentialsToEpoch" .= credsToEpoch
            ]
  forMachine _ (Conway.InvalidPrevGovActionId proposalProcedure) =
    mconcat [ "kind" .= String "InvalidPrevGovActionId"
            , "proposalProcedure" .= proposalProcedure
            ]
  forMachine _ (Conway.VotingOnExpiredGovAction actions) =
    mconcat [ "kind" .= String "VotingOnExpiredGovAction"
            , "action" .= actions
            ]
  forMachine _ (Conway.ProposalCantFollow prevGovActionId protVer prevProtVer) =
    mconcat [ "kind" .= String "ProposalCantFollow"
            , "prevGovActionId" .= prevGovActionId
            , "protVer" .= protVer
            , "prevProtVer" .= prevProtVer
            ]
  forMachine _ (Conway.InvalidPolicyHash actualPolicyHash expectedPolicyHash) =
    mconcat [ "kind" .= String "InvalidPolicyHash"
            , "actualPolicyHash" .= actualPolicyHash
            , "expectedPolicyHash" .= expectedPolicyHash
            ]

instance
  ( Consensus.ShelleyBasedEra era
  , LogFormatting (PredicateFailure (Ledger.EraRule "CERT" era))
  ) => LogFormatting (Conway.ConwayCertsPredFailure era) where
  forMachine _ (Conway.DelegateeNotRegisteredDELEG poolID) =
    mconcat [ "kind" .= String "DelegateeNotRegisteredDELEG"
            , "poolID" .= String (textShow poolID)
            ]
  forMachine _ (Conway.WithdrawalsNotInRewardsCERTS rs) =
    mconcat [ "kind" .= String "WithdrawalsNotInRewardsCERTS"
             , "rewardAccounts" .= rs
            ]
  forMachine dtal (Conway.CertFailure certFailure) =
    forMachine dtal certFailure


instance
  ( Ledger.Crypto crypto
  ) => LogFormatting (Praos.PraosValidationErr crypto) where
  forMachine _ err' =
    case err' of
      Praos.VRFKeyUnknown unknownKeyHash ->
        mconcat [ "kind" .= String "VRFKeyUnknown"
                , "vrfKey" .= unknownKeyHash
                ]
      Praos.VRFKeyWrongVRFKey stakePoolKeyHash registeredVrfForSaidStakepool wrongKeyHashInBlockHeader ->
        mconcat [ "kind" .= String "VRFKeyWrongVRFKey"
                , "stakePoolKeyHash" .= stakePoolKeyHash
                , "stakePoolVrfKey" .= registeredVrfForSaidStakepool
                , "blockHeaderVrfKey" .= wrongKeyHashInBlockHeader
                ]
      Praos.VRFKeyBadProof slotNo nonce vrfCalculatedVal->
        mconcat [ "kind" .= String "VRFKeyBadProof"
                , "slotNumberUsedInVrfCalculation" .= slotNo
                , "nonceUsedInVrfCalculation" .= nonce
                , "calculatedVrfValue" .= String (textShow vrfCalculatedVal)
                ]
      Praos.VRFLeaderValueTooBig leaderValue sigma f->
        mconcat [ "kind" .= String "VRFLeaderValueTooBig"
                , "leaderValue" .= leaderValue
                , "sigma" .= sigma
                , "f" .= activeSlotLog f
                ]
      Praos.KESBeforeStartOCERT startKesPeriod currKesPeriod ->
        mconcat [ "kind" .= String "KESBeforeStartOCERT"
                , "opCertStartingKesPeriod" .= startKesPeriod
                , "currentKesPeriod" .= currKesPeriod
                ]
      Praos.KESAfterEndOCERT currKesPeriod startKesPeriod maxKesKeyEvos ->
        mconcat [ "kind" .= String "KESAfterEndOCERT"
                , "opCertStartingKesPeriod" .= startKesPeriod
                , "currentKesPeriod" .= currKesPeriod
                , "maxKesKeyEvolutions" .= maxKesKeyEvos
                ]
      Praos.CounterTooSmallOCERT lastCounter currentCounter ->
        mconcat [ "kind" .= String "CounterTooSmallOCERT"
                , "lastCounter" .= lastCounter
                , "currentCounter" .= currentCounter
                ]
      Praos.CounterOverIncrementedOCERT lastCounter currentCounter ->
        mconcat [ "kind" .= String "CounterOverIncrementedOCERT"
                , "lastCounter" .= lastCounter
                , "currentCounter" .= currentCounter
                ]
      Praos.InvalidSignatureOCERT counter oCertStartKesPeriod err ->
        mconcat [ "kind" .= String "InvalidSignatureOCERT"
                , "counter" .= counter
                , "opCertStartingKesPeriod" .= oCertStartKesPeriod
                , "error" .= err
                ]
      Praos.InvalidKesSignatureOCERT currentKesPeriod opCertStartKesPeriod expectedKesEvos err ->
        mconcat [ "kind" .= String "InvalidKesSignatureOCERT"
                , "currentKesPeriod" .= currentKesPeriod
                , "opCertStartingKesPeriod" .= opCertStartKesPeriod
                , "expectedKesEvolutions" .= expectedKesEvos
                , "error" .= err
                ]
      Praos.NoCounterForKeyHashOCERT stakePoolKeyHash->
        mconcat [ "kind" .= String "NoCounterForKeyHashOCERT"
                , "stakePoolKeyHash" .= stakePoolKeyHash
                ]

instance LogFormatting (Praos.PraosCannotForge crypto) where
  forMachine _ (Praos.PraosCannotForgeKeyNotUsableYet currentKesPeriod startingKesPeriod) =
    mconcat [ "kind" .= String "PraosCannotForgeKeyNotUsableYet"
            , "currentKesPeriod" .= currentKesPeriod
            , "opCertStartingKesPeriod" .= startingKesPeriod
            ]

instance LogFormatting Praos.PraosEnvelopeError where
  forMachine _ err' =
    case err' of
      Praos.ObsoleteNode maxPtclVersionFromPparams blkHeaderPtclVersion ->
        mconcat [ "kind" .= String "ObsoleteNode"
                , "maxMajorProtocolVersion" .= maxPtclVersionFromPparams
                , "headerProtocolVersion" .= blkHeaderPtclVersion
                ]
      Praos.HeaderSizeTooLarge headerSize ledgerViewMaxHeaderSize ->
        mconcat [ "kind" .= String "HeaderSizeTooLarge"
                , "maxHeaderSize" .= ledgerViewMaxHeaderSize
                , "headerSize" .= headerSize
                ]
      Praos.BlockSizeTooLarge blockSize ledgerViewMaxBlockSize ->
        mconcat [ "kind" .= String "BlockSizeTooLarge"
                , "maxBlockSize" .= ledgerViewMaxBlockSize
                , "blockSize" .= blockSize
                ]

instance Ledger.Crypto c => LogFormatting (PraosChainSelectView c) where
  forMachine _ PraosChainSelectView {
      csvChainLength
    , csvSlotNo
    , csvIssuer
    , csvIssueNo
    , csvTieBreakVRF
    } =
      mconcat [ "kind" .= String "PraosChainSelectView"
              , "chainLength" .= csvChainLength
              , "slotNo" .= csvSlotNo
              , "issuerHash" .= hashKey csvIssuer
              , "issueNo" .= csvIssueNo
              , "tieBreakVRF" .= renderVRF csvTieBreakVRF
              ]
    where
      renderVRF = Text.decodeUtf8 . B16.encode . Crypto.getOutputVRFBytes

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing  -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk
