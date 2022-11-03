{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Shelley () where

import           Cardano.Prelude

import           Data.Aeson (Value (..), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Cardano.Api (textShow)
import qualified Cardano.Api as Api
import           Cardano.Api.Orphans ()
import qualified Cardano.Api.Shelley as Api
import           Cardano.Ledger.Crypto (StandardCrypto)

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.Render (renderTxId)
import           Cardano.Node.Tracing.Tracers.KESInfo ()

import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)

import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import qualified Ouroboros.Consensus.Shelley.Protocol.Praos as Praos

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Core
import           Cardano.Ledger.BaseTypes (activeSlotLog, strictMaybeToMaybe)
import           Cardano.Ledger.Chain
import qualified Cardano.Ledger.Core as Core hiding (Crypto)
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock, labBlockNo)
import           Cardano.Protocol.TPraos.Rules.OCert
import           Cardano.Protocol.TPraos.Rules.Overlay
import           Cardano.Protocol.TPraos.Rules.Tickn
import           Cardano.Protocol.TPraos.Rules.Updn

-- TODO: this should be exposed via Cardano.Api
import           Cardano.Ledger.Shelley.API

import           Cardano.Ledger.Alonzo.Rules (AlonzoBbodyPredFailure (..), AlonzoUtxoPredFailure,
                   AlonzoUtxosPredFailure, AlonzoUtxowPredFailure (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import           Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import           Cardano.Ledger.Shelley.Rules.Bbody
import           Cardano.Ledger.Shelley.Rules.Deleg
import           Cardano.Ledger.Shelley.Rules.Delegs
import           Cardano.Ledger.Shelley.Rules.Delpl
import           Cardano.Ledger.Shelley.Rules.Epoch
import           Cardano.Ledger.Shelley.Rules.Ledger
import           Cardano.Ledger.Shelley.Rules.Ledgers
import           Cardano.Ledger.Shelley.Rules.Mir
import           Cardano.Ledger.Shelley.Rules.NewEpoch
import           Cardano.Ledger.Shelley.Rules.Newpp
import           Cardano.Ledger.Shelley.Rules.Pool
import           Cardano.Ledger.Shelley.Rules.PoolReap
import           Cardano.Ledger.Shelley.Rules.Ppup
import           Cardano.Ledger.Shelley.Rules.Rupd
import           Cardano.Ledger.Shelley.Rules.Snap
import           Cardano.Ledger.Shelley.Rules.Tick
import           Cardano.Ledger.Shelley.Rules.Upec
import           Cardano.Ledger.Shelley.Rules.Utxo
import           Cardano.Ledger.Shelley.Rules.Utxow
import           Cardano.Ledger.ShelleyMA.Rules (ShelleyMAUtxoPredFailure)
import qualified Cardano.Ledger.ShelleyMA.Rules as MA
import           Cardano.Protocol.TPraos.API (ChainTransitionError (ChainTransitionError))
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import           Cardano.Protocol.TPraos.Rules.Prtcl


{- HLINT ignore "Use :" -}

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted in roughly topological order.

instance ShelleyBasedEra era => ToObject (GenTx (ShelleyBlock protocol era)) where
  toObject _ tx = mconcat [ "txid" .= Text.take 8 (renderTxId (txId tx)) ]

instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock protocol era))) where
  toJSON = String . Text.take 8 . renderTxId

instance ShelleyCompatible protocol era => ToObject (Header (ShelleyBlock protocol era)) where
  toObject _verb b = mconcat
        [ "kind" .= String "ShelleyBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (ApplyTxError era) where
  toObject verb (ApplyTxError predicateFailures) =
    mconcat $ map (toObject verb) predicateFailures

instance Core.Crypto crypto => ToObject (TPraosCannotForge crypto) where
  toObject _verb (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) =
    mconcat
      [ "kind" .= String "TPraosCannotForgeKeyNotUsableYet"
      , "keyStart" .= keyStartPeriod
      , "wallClock" .= wallClockPeriod
      ]
  toObject _verb (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) =
    mconcat
      [ "kind" .= String "TPraosCannotLeadWrongVRF"
      , "expected" .= genDlgVRFHash
      , "actual" .= coreNodeVRFHash
      ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (Ledger.EraRule "BBODY" era))
         ) => ToObject (ShelleyLedgerError era) where
  toObject verb (BBodyError (BlockTransitionError fs)) =
    mconcat [ "kind" .= String "BBodyError"
             , "failures" .= map (toObject verb) fs
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Ledger.PParamsUpdate era)
         ) => ToObject (ShelleyLedgerUpdate era) where
  toObject verb (ShelleyUpdatedProtocolUpdates updates) =
    mconcat [ "kind" .= String "ShelleyUpdatedProtocolUpdates"
             , "updates" .= map (toObject verb) updates
             ]

instance (Ledger.Era era, ToJSON (Ledger.PParamsUpdate era))
         => ToObject (ProtocolUpdate era) where
  toObject verb ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} =
    mconcat [ "proposal" .= toObject verb protocolUpdateProposal
             , "state"    .= toObject verb protocolUpdateState
             ]

instance ToJSON (Ledger.PParamsUpdate era)
         => ToObject (UpdateProposal era) where
  toObject _verb UpdateProposal{proposalParams, proposalVersion, proposalEpoch} =
    mconcat [ "params"  .= proposalParams
             , "version" .= proposalVersion
             , "epoch"   .= proposalEpoch
             ]

instance Core.Crypto crypto => ToObject (UpdateState crypto) where
  toObject _verb UpdateState{proposalVotes, proposalReachedQuorum} =
    mconcat [ "proposal"      .= proposalVotes
             , "reachedQuorum" .= proposalReachedQuorum
             ]

instance Core.Crypto crypto => ToObject (ChainTransitionError crypto) where
  toObject verb (ChainTransitionError fs) =
    mconcat [ "kind" .= String "ChainTransitionError"
             , "failures" .= map (toObject verb) fs
             ]

instance ToObject ChainPredicateFailure where
  toObject _verb (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) =
    mconcat [ "kind" .= String "HeaderSizeTooLarge"
             , "headerSize" .= hdrSz
             , "maxHeaderSize" .= maxHdrSz
             ]
  toObject _verb (BlockSizeTooLargeCHAIN blkSz maxBlkSz) =
    mconcat [ "kind" .= String "BlockSizeTooLarge"
             , "blockSize" .= blkSz
             , "maxBlockSize" .= maxBlkSz
             ]
  toObject _verb (ObsoleteNodeCHAIN currentPtcl supportedPtcl) =
    mconcat [ "kind" .= String "ObsoleteNode"
             , "explanation" .= String explanation
             , "currentProtocol" .= currentPtcl
             , "supportedProtocol" .= supportedPtcl ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."

instance ToObject (PrtlSeqFailure crypto) where
  toObject _verb (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mconcat [ "kind" .= String "WrongSlotInterval"
             , "lastSlot" .= lastSlot
             , "currentSlot" .= currSlot
             ]
  toObject _verb (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mconcat [ "kind" .= String "WrongBlockNo"
             , "lastAppliedBlockNo" .= showLastAppBlockNo lab
             , "currentBlockNo" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  toObject _verb (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mconcat [ "kind" .= String "WrongBlockSequence"
             , "lastAppliedBlockHash" .= String (textShow lastAppliedHash)
             , "currentBlockHash" .= String (textShow currentHash)
             ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (Ledger.EraRule "LEDGERS" era))
         ) => ToObject (ShelleyBbodyPredFailure era) where
  toObject _verb (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) =
    mconcat [ "kind" .= String "WrongBlockBodySizeBBODY"
             , "actualBlockBodySize" .= actualBodySz
             , "claimedBlockBodySize" .= claimedBodySz
             ]
  toObject _verb (InvalidBodyHashBBODY actualHash claimedHash) =
    mconcat [ "kind" .= String "InvalidBodyHashBBODY"
             , "actualBodyHash" .= textShow actualHash
             , "claimedBodyHash" .= textShow claimedHash
             ]
  toObject verb (LedgersFailure f) = toObject verb f


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (ShelleyUTXO era))
         , ToObject (PredicateFailure (ShelleyUTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (ShelleyLedgersPredFailure era) where
  toObject verb (LedgerFailure f) = toObject verb f


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (ShelleyUTXO era))
         , ToObject (PredicateFailure (ShelleyUTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "DELEGS" era))
         , ToObject (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => ToObject (ShelleyLedgerPredFailure era) where
  toObject verb (UtxowFailure f) = toObject verb f
  toObject verb (DelegsFailure f) = toObject verb f

instance ( ShelleyBasedEra era
         , ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ToObject (PredicateFailure (Ledger.EraRule "PPUP" era))
         , ToObject (PredicateFailure (Ledger.EraRule "UTXO" era))
         , Ledger.Crypto era ~ StandardCrypto
         ) => ToObject (AlonzoUtxowPredFailure era) where
  toObject v (ShelleyInAlonzoUtxowPredFailure utxoPredFail) =
    toObject v utxoPredFail
  toObject _ (MissingRedeemers scripts) =
    mconcat [ "kind" .= String "MissingRedeemers"
             , "scripts" .= renderMissingRedeemers scripts
             ]
  toObject _ (MissingRequiredDatums required received) =
    mconcat [ "kind" .= String "MissingRequiredDatums"
             , "required" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList required)
             , "received" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList received)
             ]
  toObject _ (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) =
    mconcat [ "kind" .= String "PPViewHashesDontMatch"
             , "fromTxBody" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashInTxBody)
             , "fromPParams" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashFromPParams)
             ]
  toObject _ (MissingRequiredSigners missingKeyWitnesses) =
    mconcat [ "kind" .= String "MissingRequiredSigners"
             , "witnesses" .= Set.toList missingKeyWitnesses
             ]
  toObject _ (UnspendableUTxONoDatumHash txins) =
    mconcat [ "kind" .= String "MissingRequiredSigners"
             , "txins" .= Set.toList txins
             ]
  toObject _ (NonOutputSupplimentaryDatums disallowed acceptable) =
    mconcat [ "kind" .= String "NonOutputSupplimentaryDatums"
             , "disallowed" .= Set.toList disallowed
             , "acceptable" .= Set.toList acceptable
             ]
  toObject _ (ExtraRedeemers rdmrs) =
    mconcat [ "kind" .= String "ExtraRedeemers"
             , "rdmrs" .= map (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) rdmrs
             ]

renderScriptIntegrityHash :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto) -> Aeson.Value
renderScriptIntegrityHash (Just witPPDataHash) =
  Aeson.String . Crypto.hashToTextAsHex $ SafeHash.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null

renderScriptHash :: ScriptHash StandardCrypto -> Text
renderScriptHash = Api.serialiseToRawBytesHexText . Api.fromShelleyScriptHash

renderMissingRedeemers :: [(Alonzo.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto)] -> Aeson.Value
renderMissingRedeemers scripts = Aeson.object $ map renderTuple  scripts
 where
  renderTuple :: (Alonzo.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto) -> Aeson.Pair
  renderTuple (scriptPurpose, sHash) =
    Aeson.fromText (renderScriptHash sHash) .= renderScriptPurpose scriptPurpose

renderScriptPurpose :: Alonzo.ScriptPurpose StandardCrypto -> Aeson.Value
renderScriptPurpose (Alonzo.Minting pid) =
  Aeson.object [ "minting" .= toJSON pid]
renderScriptPurpose (Alonzo.Spending txin) =
  Aeson.object [ "spending" .= Api.fromShelleyTxIn txin]
renderScriptPurpose (Alonzo.Rewarding rwdAcct) =
  Aeson.object [ "rewarding" .= Aeson.String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)]
renderScriptPurpose (Alonzo.Certifying cert) =
  Aeson.object [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate cert)]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (ShelleyUTXO era))
         , ToObject (PredicateFailure (Core.EraRule "UTXO" era))
         ) => ToObject (ShelleyUtxowPredFailure era) where
  toObject _verb (ExtraneousScriptWitnessesUTXOW extraneousScripts) =
    mconcat [ "kind" .= String "InvalidWitnessesUTXOW"
             , "extraneousScripts" .= extraneousScripts
             ]
  toObject _verb (InvalidWitnessesUTXOW wits') =
    mconcat [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalidWitnesses" .= map textShow wits'
             ]
  toObject _verb (MissingVKeyWitnessesUTXOW wits') =
    mconcat [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missingWitnesses" .= wits'
             ]
  toObject _verb (MissingScriptWitnessesUTXOW missingScripts) =
    mconcat [ "kind" .= String "MissingScriptWitnessesUTXOW"
             , "missingScripts" .= missingScripts
             ]
  toObject _verb (ScriptWitnessNotValidatingUTXOW failedScripts) =
    mconcat [ "kind" .= String "ScriptWitnessNotValidatingUTXOW"
             , "failedScripts" .= failedScripts
             ]
  toObject verb (UtxoFailure f) = toObject verb f
  toObject _verb (MIRInsufficientGenesisSigsUTXOW genesisSigs) =
    mconcat [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW"
             , "genesisSigs" .= genesisSigs
             ]
  toObject _verb (MissingTxBodyMetadataHash metadataHash) =
    mconcat [ "kind" .= String "MissingTxBodyMetadataHash"
             , "metadataHash" .= metadataHash
             ]
  toObject _verb (MissingTxMetadata txBodyMetadataHash) =
    mconcat [ "kind" .= String "MissingTxMetadata"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             ]
  toObject _verb (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    mconcat [ "kind" .= String "ConflictingMetadataHash"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             , "fullMetadataHash" .= fullMetadataHash
             ]
  toObject _verb InvalidMetadata =
    mconcat [ "kind" .= String "InvalidMetadata"
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => ToObject (ShelleyUtxoPredFailure era) where
  toObject _verb (BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (ExpiredUTxO ttl slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  toObject _verb (MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject _verb (OutputBootAddrAttrsTooBig badOutputs) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  toObject _verb InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (FeeTooSmallUTxO minfee txfee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject verb (UpdateFailure f) = toObject verb f

  toObject _verb (WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]


instance ToJSON MA.ValidityInterval where
  toJSON vi =
    Aeson.object $
        [ "invalidBefore"    .= x | x <- mbfield (MA.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield (MA.invalidHereafter vi) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
         ) => ToObject (ShelleyMAUtxoPredFailure era) where
  toObject _verb (MA.BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (MA.OutsideValidityIntervalUTxO validityInterval slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  toObject _verb (MA.MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  toObject _verb MA.InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (MA.FeeTooSmallUTxO minfee txfee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (MA.ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject _verb (MA.WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (MA.WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (MA.OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject verb (MA.UpdateFailure f) = toObject verb f
  toObject _verb (MA.OutputBootAddrAttrsTooBig badOutputs) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  toObject _verb MA.TriesToForgeADA =
    mconcat [ "kind" .= String "TriesToForgeADA" ]
  toObject _verb (MA.OutputTooBigUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Aeson.Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Aeson.Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance Ledger.Era era => ToObject (ShelleyPpupPredFailure era) where
  toObject _verb (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mconcat [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  toObject _verb (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mconcat [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (show votingPeriod)
             ]
  toObject _verb (PVCannotFollowPPUP badPv) =
    mconcat [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToObject (ShelleyDelegsPredFailure era) where
  toObject _verb (DelegateeNotRegisteredDELEG targetPool) =
    mconcat [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  toObject _verb (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mconcat [ "kind" .= String "WithdrawalsNotInRewardsDELEGS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  toObject verb (DelplFailure f) = toObject verb f


instance ( ToObject (PredicateFailure (Core.EraRule "POOL" era))
         , ToObject (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToObject (ShelleyDelplPredFailure era) where
  toObject verb (PoolFailure f) = toObject verb f
  toObject verb (DelegFailure f) = toObject verb f

instance Ledger.Era era => ToObject (ShelleyDelegPredFailure era) where
  toObject _verb (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) =
    mconcat [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential already registered"
             ]
  toObject _verb (StakeKeyInRewardsDELEG alreadyRegistered) =
    mconcat [ "kind" .= String "StakeKeyInRewardsDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential registered in rewards map"
             ]
  toObject _verb (StakeKeyNotRegisteredDELEG notRegistered) =
    mconcat [ "kind" .= String "StakeKeyNotRegisteredDELEG"
             , "credential" .= String (textShow notRegistered)
             , "error" .= String "Staking credential not registered"
             ]
  toObject _verb (StakeKeyNonZeroAccountBalanceDELEG remBalance) =
    mconcat [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG"
             , "remainingBalance" .= remBalance
             ]
  toObject _verb (StakeDelegationImpossibleDELEG unregistered) =
    mconcat [ "kind" .= String "StakeDelegationImpossibleDELEG"
             , "credential" .= String (textShow unregistered)
             , "error" .= String "Cannot delegate this stake credential because it is not registered"
             ]
  toObject _verb WrongCertificateTypeDELEG =
    mconcat [ "kind" .= String "WrongCertificateTypeDELEG" ]
  toObject _verb (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) =
    mconcat [ "kind" .= String "GenesisKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key is not in the delegation mapping"
             ]
  toObject _verb (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) =
    mconcat [ "kind" .= String "DuplicateGenesisDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key has already been delegated to"
             ]
  toObject _verb (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) =
    mconcat [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "neededAmount" .= neededMirAmount
             , "reserves" .= reserves
             ]
  toObject _verb (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) =
    mconcat [ "kind" .= String "MIRCertificateTooLateinEpochDELEG"
             , "currentSlotNo" .= currSlot
             , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
             ]
  toObject _verb (DuplicateGenesisVRFDELEG vrfKeyHash) =
    mconcat [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]
  toObject _verb MIRTransferNotCurrentlyAllowed =
    mconcat [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
             ]
  toObject _verb MIRNegativesNotCurrentlyAllowed =
    mconcat [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
             ]
  toObject _verb (InsufficientForTransferDELEG mirpot attempted available) =
    mconcat [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "attempted" .= attempted
             , "available" .= available
             ]
  toObject _verb MIRProducesNegativeUpdate =
    mconcat [ "kind" .= String "MIRProducesNegativeUpdate"
             ]
  toObject _verb (MIRNegativeTransfer pot coin) =
    mconcat [ "kind" .= String "MIRNegativeTransfer"
             , "error" .= String "Attempt to transfer a negative amount from a pot."
             , "pot" .= String (case pot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "amount" .= coin
             ]

instance ToObject (ShelleyPoolPredFailure era) where
  toObject _verb (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) =
    mconcat [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL"
             , "unregisteredKeyHash" .= String (textShow unregStakePool)
             , "error" .= String "This stake pool key hash is unregistered"
             ]
  toObject _verb (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) =
    mconcat [ "kind" .= String "StakePoolRetirementWrongEpochPOOL"
             , "currentEpoch" .= String (textShow currentEpoch)
             , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
             , "maxEpochForRetirement" .= String (textShow maxRetireEpoch)
             ]
  toObject _verb (StakePoolCostTooLowPOOL certCost protCost) =
    mconcat [ "kind" .= String "StakePoolCostTooLowPOOL"
             , "certificateCost" .= String (textShow certCost)
             , "protocolParCost" .= String (textShow protCost)
             , "error" .= String "The stake pool cost is too low"
             ]
  toObject _verb (PoolMedataHashTooBig poolID hashSize) =
    mconcat [ "kind" .= String "PoolMedataHashTooBig"
             , "poolID" .= String (textShow poolID)
             , "hashSize" .= String (textShow hashSize)
             , "error" .= String "The stake pool metadata hash is too large"
             ]

-- Apparently this should never happen according to the Shelley exec spec
  toObject _verb (WrongCertificateTypePOOL index) =
    case index of
      0 -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Delegation certificate"
                    ]
      1 -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: MIR certificate"
                    ]
      2 -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Genesis certificate"
                    ]
      k -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
                    , "certificateType" .= k
                    , "error" .= String "Wrong certificate type: Unknown certificate type"
                    ]

  toObject _verb (WrongNetworkPOOL networkId listedNetworkId poolId) =
    mconcat [ "kind" .= String "WrongNetworkPOOL"
             , "networkId" .= String (textShow networkId)
             , "listedNetworkId" .= String (textShow listedNetworkId)
             , "poolId" .= String (textShow poolId)
             , "error" .= String "Wrong network ID in pool registration certificate"
             ]

instance ( ToObject (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToObject (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToObject (ShelleyTickPredFailure era) where
  toObject verb (NewEpochFailure f) = toObject verb f
  toObject verb (RupdFailure f) = toObject verb f

instance ToObject TicknPredicateFailure where
  toObject _verb x = case x of {} -- no constructors

instance ( ToObject (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToObject (PredicateFailure (Core.EraRule "MIR" era))
         ) => ToObject (ShelleyNewEpochPredFailure era) where
  toObject verb (EpochFailure f) = toObject verb f
  toObject verb (MirFailure f) = toObject verb f
  toObject _verb (CorruptRewardUpdate update) =
    mconcat [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (show update) ]


instance ( ToObject (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToObject (PredicateFailure (Core.EraRule "SNAP" era))
         , ToObject (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToObject (ShelleyEpochPredFailure era) where
  toObject verb (PoolReapFailure f) = toObject verb f
  toObject verb (SnapFailure f) = toObject verb f
  toObject verb (UpecFailure f) = toObject verb f


instance ToObject (ShelleyPoolreapPredFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (ShelleySnapPredFailure era) where
  toObject _verb x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance ToObject (ShelleyNewppPredFailure era) where
  toObject _verb (UnexpectedDepositPot outstandingDeposits depositPot) =
    mconcat [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance ToObject (ShelleyMirPredFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (ShelleyRupdPredFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance Core.Crypto crypto => ToObject (PrtclPredicateFailure crypto) where
  toObject  verb (OverlayFailure f) = toObject verb f
  toObject  verb (UpdnFailure f) = toObject verb f


instance Core.Crypto crypto => ToObject (OverlayPredicateFailure crypto) where
  toObject _verb (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) =
    mconcat [ "kind" .= String "UnknownGenesisKeyOVERLAY"
             , "unknownKeyHash" .= String (textShow genKeyHash)
             ]
  toObject _verb (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mconcat [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "leaderElectionValue" .= String (textShow leaderElecVal)
             ]
  toObject _verb (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mconcat [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "blockNonce" .= String (textShow blockNonce)
             ]
  toObject _verb (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) =
    mconcat [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "poolHash" .= textShow issuerHash
             , "registeredVRFKeHash" .= textShow regVRFKeyHash
             , "unregisteredVRFKeyHash" .= textShow unregVRFKeyHash
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  toObject _verb (VRFKeyUnknown (KeyHash kHash)) =
    mconcat [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "keyHash" .= String (textShow kHash)
             ]
  toObject _verb (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mconcat [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "leaderElectionValue" .= String (textShow leadElecVal)
             , "delegationPoolWeight" .= String (textShow weightOfDelegPool)
             , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
             ]
  toObject _verb (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mconcat [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= String (textShow notActiveSlotNo)
             ]
  toObject _verb (WrongGenesisColdKeyOVERLAY actual expected) =
    mconcat [ "kind" .= String "WrongGenesisColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  toObject _verb (WrongGenesisVRFKeyOVERLAY issuer actual expected) =
    mconcat [ "kind" .= String "WrongGenesisVRFKeyOVERLAY"
             , "issuer" .= issuer
             , "actual" .= actual
             , "expected" .= expected ]
  toObject verb (OcertFailure f) = toObject verb f


instance ToObject (OcertPredicateFailure crypto) where
  toObject _verb (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) =
    mconcat [ "kind" .= String "KESBeforeStartOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "currentKESPeriod" .= String (textShow current)
             , "error" .= String "Your operational certificate's KES start period \
                                 \is before the KES current period."
             ]
  toObject _verb (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
    mconcat [ "kind" .= String "KESAfterEndOCERT"
             , "currentKESPeriod" .= String (textShow current)
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "maxKESEvolutions" .= String  (textShow maxKESEvolutions)
             , "error" .= String "The operational certificate's KES start period is \
                                 \greater than the max number of KES + the KES current period"
             ]
  toObject _verb (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
    mconcat [ "kind" .= String "CounterTooSmallOCert"
             , "currentKESCounter" .= String (textShow currentKESCounter)
             , "lastKESCounter" .= String (textShow lastKEScounterUsed)
             , "error" .= String "The operational certificate's last KES counter is greater \
                                 \than the current KES counter."
             ]
  toObject _verb (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) =
    mconcat [ "kind" .= String "InvalidSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertKESStartPeriod)
             , "opCertCounter" .= String (textShow oCertCounter)
             ]
  toObject _verb (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) =
    mconcat [ "kind" .= String "InvalidKesSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow startKESPeriod)
             , "opCertKESCurrentPeriod" .= String (textShow currKESPeriod)
             , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
             , "error" .= err ]
  toObject _verb (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) =
    mconcat [ "kind" .= String "NoCounterForKeyHashOCERT"
             , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
             , "error" .= String "A counter was not found for this stake pool key hash"
             ]

instance ToObject HotKey.KESInfo where
  toObject _verb HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } =
    mconcat
      [ "kind" .= String "KESInfo"
      , "startPeriod" .= kesStartPeriod
      , "endPeriod" .= kesEndPeriod
      , "evolution" .= kesEvolution
      ]

instance ToObject HotKey.KESEvolutionError where
  toObject verb (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mconcat
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= toObject verb kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  toObject verb (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mconcat
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= toObject verb kesInfo
      , "targetPeriod" .= targetPeriod
      ]

instance ToObject (UpdnPredicateFailure crypto) where
  toObject _verb x = case x of {} -- no constructors

instance ToObject (ShelleyUpecPredFailure era) where
  toObject _verb (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) =
    mconcat [ "kind" .= String "UnexpectedDepositPot"
             , "totalOutstanding" .=  String (textShow totalOutstanding)
             , "depositPot" .= String (textShow depositPot)
             ]


--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------


instance ( Ledger.Era era
         , ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ToObject (PredicateFailure (Ledger.EraRule "UTXOS" era))
         , ShelleyBasedEra era
         ) => ToObject (AlonzoUtxoPredFailure era) where
  toObject _verb (Alonzo.BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validtyInterval
             , "slot" .= slot
             ]
  toObject _verb (Alonzo.MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize
             ]
  toObject _verb Alonzo.InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (Alonzo.FeeTooSmallUTxO minfee currentFee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= currentFee
             ]
  toObject _verb (Alonzo.ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject _verb (Alonzo.WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (Alonzo.WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (Alonzo.OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject verb (Alonzo.UtxosFailure predFailure) =
    toObject verb predFailure
  toObject _verb (Alonzo.OutputBootAddrAttrsTooBig txouts) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= txouts
             , "error" .= String "The Byron address attributes are too big"
             ]
  toObject _verb Alonzo.TriesToForgeADA =
    mconcat [ "kind" .= String "TriesToForgeADA" ]
  toObject _verb (Alonzo.OutputTooBigUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]
  toObject _verb (Alonzo.InsufficientCollateral computedBalance suppliedFee) =
    mconcat [ "kind" .= String "InsufficientCollateral"
             , "balance" .= computedBalance
             , "txfee" .= suppliedFee
             ]
  toObject _verb (Alonzo.ScriptsNotPaidUTxO utxos) =
    mconcat [ "kind" .= String "ScriptsNotPaidUTxO"
             , "utxos" .= utxos
             ]
  toObject _verb (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) =
    mconcat [ "kind" .= String "ExUnitsTooBigUTxO"
             , "maxexunits" .= pParamsMaxExUnits
             , "exunits" .= suppliedExUnits
             ]
  toObject _verb (Alonzo.CollateralContainsNonADA inputs) =
    mconcat [ "kind" .= String "CollateralContainsNonADA"
             , "inputs" .= inputs
             ]
  toObject _verb (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) =
    mconcat [ "kind" .= String "WrongNetworkInTxBody"
             , "networkid" .= actualNetworkId
             , "txbodyNetworkId" .= netIdInTxBody
             ]
  toObject _verb (Alonzo.OutsideForecast slotNum) =
    mconcat [ "kind" .= String "OutsideForecast"
             , "slot" .= slotNum
             ]
  toObject _verb (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) =
    mconcat [ "kind" .= String "TooManyCollateralInputs"
             , "max" .= maxCollateralInputs
             , "inputs" .= numberCollateralInputs
             ]
  toObject _verb Alonzo.NoCollateralInputs =
    mconcat [ "kind" .= String "NoCollateralInputs" ]

instance ( ToJSON (Alonzo.CollectError (Ledger.Crypto era))
         , ToObject (PredicateFailure (Ledger.EraRule "PPUP" era))
         ) =>ToObject (AlonzoUtxosPredFailure era) where
  toObject _ (Alonzo.ValidationTagMismatch isValidating reason) =
    mconcat [ "kind" .= String "ValidationTagMismatch"
             , "isvalidating" .= isValidating
             , "reason" .= reason
             ]
  toObject _ (Alonzo.CollectErrors errors) =
    mconcat [ "kind" .= String "CollectErrors"
             , "errors" .= errors
             ]
  toObject verb (Alonzo.UpdateFailure pFailure) =
    toObject verb pFailure

deriving newtype instance ToJSON Alonzo.IsValid

instance ToJSON (Alonzo.CollectError StandardCrypto) where
  toJSON cError =
    case cError of
      Alonzo.NoRedeemer sPurpose ->
        object
          [ "kind" .= String "CollectError"
          , "error" .= String "NoRedeemer"
          , "scriptpurpose" .= renderScriptPurpose sPurpose
          ]
      Alonzo.NoWitness sHash ->
        object
          [ "kind" .= String "CollectError"
          , "error" .= String "NoWitness"
          , "scripthash" .= toJSON sHash
          ]
      Alonzo.NoCostModel lang ->
        object
          [ "kind" .= String "CollectError"
          , "error" .= String "NoCostModel"
          , "language" .= toJSON lang
          ]
      Alonzo.BadTranslation err ->
        object
          [ "kind" .= String "PlutusTranslationError"
          , "error" .= case err of
              Alonzo.ByronTxOutInContext txOutSource ->
                String $
                  "Cannot construct a Plutus ScriptContext from this transaction "
                    <> "due to a Byron UTxO being created or spent: "
                    <> show txOutSource
              Alonzo.TranslationLogicMissingInput txin ->
                String $ "Transaction input does not exist in the UTxO: " <> show txin
              Alonzo.RdmrPtrPointsToNothing ptr ->
                object
                  [ "kind" .= String "RedeemerPointerPointsToNothing"
                  , "ptr" .= (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) ptr
                  ]
              Alonzo.LanguageNotSupported lang ->
                String $ "Language not supported: " <> show lang
              Alonzo.InlineDatumsNotSupported txOutSource ->
                String $ "Inline datums not supported, output source: " <> show txOutSource
              Alonzo.ReferenceScriptsNotSupported txOutSource ->
                String $ "Reference scripts not supported, output source: " <> show txOutSource
              Alonzo.ReferenceInputsNotSupported txins ->
                String $ "Reference inputs not supported: " <> show txins
              Alonzo.TimeTranslationPastHorizon msg ->
                String $ "Time translation requested past the horizon: " <> show msg
          ]

instance ToJSON Alonzo.TagMismatchDescription where
  toJSON tmd = case tmd of
    Alonzo.PassedUnexpectedly ->
      object
        [ "kind" .= String "TagMismatchDescription"
        , "error" .= String "PassedUnexpectedly"
        ]
    Alonzo.FailedUnexpectedly forReasons ->
      object
        [ "kind" .= String "TagMismatchDescription"
        , "error" .= String "FailedUnexpectedly"
        , "reconstruction" .= forReasons
        ]

instance ToJSON Alonzo.FailureDescription where
  toJSON f = case f of
    Alonzo.PlutusFailure t _bs ->
      object
        [ "kind" .= String "FailureDescription"
        , "error" .= String "PlutusFailure"
        , "description" .= t
        -- , "reconstructionDetail" .= bs
        ]

instance ( Ledger.Era era
         , Show (PredicateFailure (Ledger.EraRule "LEDGERS" era))
         ) => ToObject (AlonzoBbodyPredFailure era) where
  toObject _ err = mconcat [ "kind" .= String "AlonzoBbodyPredFail"
                            , "error" .= String (show err)
                            ]

--------------------------------------------------------------------------------
-- Babbage related
--------------------------------------------------------------------------------

instance ( ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ShelleyBasedEra era
         , ToObject (ShelleyUtxowPredFailure era)
         , ToObject (PredicateFailure (Ledger.EraRule "UTXOS" era))
         ) => ToObject (BabbageUtxoPredFailure era) where
  toObject v err =
    case err of
      Babbage.AlonzoInBabbageUtxoPredFailure alonzoFail ->
        toObject v alonzoFail

      Babbage.IncorrectTotalCollateralField provided declared ->
        mconcat [ "kind" .= String "UnequalCollateralReturn"
                , "collateralProvided" .= provided
                , "collateralDeclared" .= declared
                ]
      Babbage.BabbageOutputTooSmallUTxO outputs->
        mconcat [ "kind" .= String "BabbageOutputTooSmall"
                , "outputs" .= outputs
                ]

instance ( Ledger.Era era
         , ShelleyBasedEra era
         , Ledger.Crypto era ~ StandardCrypto
         , ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ToObject (ShelleyUtxowPredFailure era)
         , ToObject (PredicateFailure (Ledger.EraRule "PPUP" era))
         , ToObject (PredicateFailure (Ledger.EraRule "UTXO" era))
         ) => ToObject (BabbageUtxowPredFailure era) where
  toObject v err =
    case err of
      Babbage.AlonzoInBabbageUtxowPredFailure alonzoFail ->
        toObject v alonzoFail
      Babbage.UtxoFailure utxoFail ->
        toObject v utxoFail
      -- TODO: Plutus team needs to expose a better error type.
      Babbage.MalformedScriptWitnesses s ->
        mconcat [ "kind" .= String "MalformedScriptWitnesses"
                , "scripts" .= s
                ]
      Babbage.MalformedReferenceScripts s ->
        mconcat [ "kind" .= String "MalformedReferenceScripts"
                , "scripts" .= s
                ]

instance Core.Crypto crypto => ToObject (Praos.PraosValidationErr crypto) where
  toObject _ err' =
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
                , "calculatedVrfValue" .= String (show vrfCalculatedVal)
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

instance ToObject (Praos.PraosCannotForge crypto) where
  toObject _ (Praos.PraosCannotForgeKeyNotUsableYet currentKesPeriod startingKesPeriod) =
    mconcat [ "kind" .= String "PraosCannotForgeKeyNotUsableYet"
            , "currentKesPeriod" .= currentKesPeriod
            , "opCertStartingKesPeriod" .= startingKesPeriod
            ]

instance ToObject Praos.PraosEnvelopeError where
  toObject _ err' =
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

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk

-- Common to cardano-cli

deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)

deriving newtype instance Core.Crypto crypto => ToJSON (TxId crypto)
