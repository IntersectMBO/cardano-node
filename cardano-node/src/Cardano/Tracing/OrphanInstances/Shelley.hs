{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

import           Data.Aeson (ToJSONKey (..), ToJSONKeyFunction (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Strict as HMS
import           Data.Scientific (Scientific)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()

import           Cardano.Crypto.Hash.Class as Crypto

import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)

import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCannotForge (..))
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

import qualified Cardano.Ledger.AuxiliaryData as Core
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.Mary.Value as MA
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Ledger.Torsor as Core

-- TODO: this should be exposed via Cardano.Api
import           Shelley.Spec.Ledger.API hiding (ShelleyBasedEra)
import           Shelley.Spec.Ledger.BlockChain (LastAppliedBlock (..))
import           Shelley.Spec.Ledger.Coin (DeltaCoin (..))
import           Shelley.Spec.Ledger.PParams (PParamsUpdate)

import           Shelley.Spec.Ledger.STS.Bbody
import           Shelley.Spec.Ledger.STS.Chain
import           Shelley.Spec.Ledger.STS.Deleg
import           Shelley.Spec.Ledger.STS.Delegs
import           Shelley.Spec.Ledger.STS.Delpl
import           Shelley.Spec.Ledger.STS.Epoch
import           Shelley.Spec.Ledger.STS.Ledger
import           Shelley.Spec.Ledger.STS.Ledgers
import           Shelley.Spec.Ledger.STS.Mir
import           Shelley.Spec.Ledger.STS.NewEpoch
import           Shelley.Spec.Ledger.STS.Newpp
import           Shelley.Spec.Ledger.STS.Ocert
import           Shelley.Spec.Ledger.STS.Overlay
import           Shelley.Spec.Ledger.STS.Pool
import           Shelley.Spec.Ledger.STS.PoolReap
import           Shelley.Spec.Ledger.STS.Ppup
import           Shelley.Spec.Ledger.STS.Rupd
import           Shelley.Spec.Ledger.STS.Snap
import           Shelley.Spec.Ledger.STS.Tick
import           Shelley.Spec.Ledger.STS.Updn
import           Shelley.Spec.Ledger.STS.Upec
import           Shelley.Spec.Ledger.STS.Utxo
import           Shelley.Spec.Ledger.STS.Utxow

{- HLINT ignore "Use :" -}

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted in roughly topological order.

instance ShelleyBasedEra era => ToObject (GenTx (ShelleyBlock era)) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]

instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock era))) where
  toJSON i = toJSON (condense i)

instance ShelleyBasedEra era => ToObject (Header (ShelleyBlock era)) where
  toObject _verb b = mkObject
        [ "kind" .= String "ShelleyBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (ApplyTxError era) where
  toObject verb (ApplyTxError predicateFailures) =
    HMS.unions $ map (toObject verb) predicateFailures

instance ToObject (TPraosCannotForge era) where
  toObject _verb (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) =
    mkObject
      [ "kind" .= String "TPraosCannotForgeKeyNotUsableYet"
      , "keyStart" .= keyStartPeriod
      , "wallClock" .= wallClockPeriod
      ]
  toObject _verb (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) =
    mkObject
      [ "kind" .= String "TPraosCannotLeadWrongVRF"
      , "expected" .= genDlgVRFHash
      , "actual" .= coreNodeVRFHash
      ]

deriving newtype instance ToJSON KESPeriod

instance ToObject HotKey.KESInfo where
  toObject _verb HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } =
    mkObject
      [ "kind" .= String "KESInfo"
      , "startPeriod" .= kesStartPeriod
      , "endPeriod" .= kesEndPeriod
      , "evolution" .= kesEvolution
      ]

instance ToObject HotKey.KESEvolutionError where
  toObject verb (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= toObject verb kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  toObject verb (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= toObject verb kesInfo
      , "targetPeriod" .= targetPeriod
      ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "BBODY" era))
         ) => ToObject (ShelleyLedgerError era) where
  toObject verb (BBodyError (BlockTransitionError fs)) =
    mkObject [ "kind" .= String "BBodyError"
             , "failures" .= map (toObject verb) fs
             ]

instance ShelleyBasedEra era => ToObject (ShelleyLedgerUpdate era) where
  toObject verb (ShelleyUpdatedProtocolUpdates updates) =
    mkObject [ "kind" .= String "ShelleyUpdatedProtocolUpdates"
             , "updates" .= map (toObject verb) updates
             ]

instance ToObject (ProtocolUpdate crypto) where
  toObject verb ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} =
    mkObject [ "proposal" .= toObject verb protocolUpdateProposal
             , "state"    .= toObject verb protocolUpdateState
             ]

instance ToObject (UpdateProposal crypto) where
  toObject _verb UpdateProposal{proposalParams, proposalVersion, proposalEpoch} =
    mkObject [ "params"  .= proposalParams
             , "version" .= proposalVersion
             , "epoch"   .= proposalEpoch
             ]

instance ToObject (UpdateState crypto) where
  toObject _verb UpdateState{proposalVotes, proposalReachedQuorum} =
    mkObject [ "proposal"      .= proposalVotes
             , "reachedQuorum" .= proposalReachedQuorum
             ]

instance ToJSON (PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (_minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (_minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (_maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (_maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (_maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (_keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (_poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (_eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (_nOpt pp) ]
     ++ [ "a0" .= (fromRational x :: Scientific)
                                       | x <- mbfield (_a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (_rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (_tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (_d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (_extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (_protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (_minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (_minPoolCost pp) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance Core.Crypto crypto => ToObject (ChainTransitionError crypto) where
  toObject verb (ChainTransitionError fs) =
    mkObject [ "kind" .= String "ChainTransitionError"
             , "failures" .= map (toObject verb) fs
             ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (Core.EraRule "UTXOW" era))
         , ToObject (PredicateFailure (Core.EraRule "BBODY" era))
         , ToObject (PredicateFailure (Core.EraRule "TICK" era))
         , ToObject (PredicateFailure (Core.EraRule "TICKN" era))
         ) => ToObject (ChainPredicateFailure era) where
  toObject _verb (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) =
    mkObject [ "kind" .= String "HeaderSizeTooLarge"
             , "headerSize" .= hdrSz
             , "maxHeaderSize" .= maxHdrSz
             ]
  toObject _verb (BlockSizeTooLargeCHAIN blkSz maxBlkSz) =
    mkObject [ "kind" .= String "BlockSizeTooLarge"
             , "blockSize" .= blkSz
             , "maxBlockSize" .= maxBlkSz
             ]
  toObject _verb (ObsoleteNodeCHAIN currentPtcl supportedPtcl) =
    mkObject [ "kind" .= String "ObsoleteNode"
             , "explanation" .= String explanation
             , "currentProtocol" .= currentPtcl
             , "supportedProtocol" .= supportedPtcl ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."
  toObject verb (BbodyFailure f) = toObject verb f
  toObject verb (TickFailure  f) = toObject verb f
  toObject verb (TicknFailure  f) = toObject verb f
  toObject verb (PrtclFailure f) = toObject verb f
  toObject verb (PrtclSeqFailure f) = toObject verb f

instance ToObject (PrtlSeqFailure crypto) where
  toObject _verb (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mkObject [ "kind" .= String "WrongSlotInterval"
             , "lastSlot" .= lastSlot
             , "currentSlot" .= currSlot
             ]
  toObject _verb (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mkObject [ "kind" .= String "WrongBlockNo"
             , "lastAppliedBlockNo" .= showLastAppBlockNo lab
             , "currentBlockNo" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  toObject _verb (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mkObject [ "kind" .= String "WrongBlockSequence"
             , "lastAppliedBlockHash" .= String (textShow lastAppliedHash)
             , "currentBlockHash" .= String (textShow currentHash)
             ]

instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (BbodyPredicateFailure era) where
  toObject _verb (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) =
    mkObject [ "kind" .= String "WrongBlockBodySizeBBODY"
             , "actualBlockBodySize" .= actualBodySz
             , "claimedBlockBodySize" .= claimedBodySz
             ]
  toObject _verb (InvalidBodyHashBBODY actualHash claimedHash) =
    mkObject [ "kind" .= String "InvalidBodyHashBBODY"
             , "actualBodyHash" .= textShow actualHash
             , "claimedBodyHash" .= textShow claimedHash
             ]
  toObject verb (LedgersFailure f) = toObject verb f


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (LedgersPredicateFailure era) where
  toObject verb (LedgerFailure f) = toObject verb f


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "DELEGS" era))
         , ToObject (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => ToObject (LedgerPredicateFailure era) where
  toObject verb (UtxowFailure f) = toObject verb f
  toObject verb (DelegsFailure f) = toObject verb f


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (Core.EraRule "UTXO" era))
         ) => ToObject (UtxowPredicateFailure era) where
  toObject _verb (InvalidWitnessesUTXOW wits) =
    mkObject [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalidWitnesses" .= map textShow wits
             ]
  toObject _verb (MissingVKeyWitnessesUTXOW (WitHashes wits)) =
    mkObject [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missingWitnesses" .= wits
             ]
  toObject _verb (MissingScriptWitnessesUTXOW missingScripts) =
    mkObject [ "kind" .= String "MissingScriptWitnessesUTXOW"
             , "missingScripts" .= missingScripts
             ]
  toObject _verb (ScriptWitnessNotValidatingUTXOW failedScripts) =
    mkObject [ "kind" .= String "ScriptWitnessNotValidatingUTXOW"
             , "failedScripts" .= failedScripts
             ]
  toObject verb (UtxoFailure f) = toObject verb f
  toObject _verb (MIRInsufficientGenesisSigsUTXOW genesisSigs) =
    mkObject [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW"
             , "genesisSigs" .= genesisSigs
             ]
  toObject _verb (MissingTxBodyMetadataHash metadataHash) =
    mkObject [ "kind" .= String "MissingTxBodyMetadataHash"
             , "metadataHash" .= metadataHash
             ]
  toObject _verb (MissingTxMetadata txBodyMetadataHash) =
    mkObject [ "kind" .= String "MissingTxMetadata"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             ]
  toObject _verb (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    mkObject [ "kind" .= String "ConflictingMetadataHash"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             , "fullMetadataHash" .= fullMetadataHash
             ]
  toObject _verb InvalidMetadata =
    mkObject [ "kind" .= String "InvalidMetadata"
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.Delta (Core.Value era))
         , ToJSON (Core.TxOut era)
         , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => ToObject (UtxoPredicateFailure era) where
  toObject _verb (BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (ExpiredUTxO ttl slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  toObject _verb (MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject _verb (OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  toObject _verb InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject verb (UpdateFailure f) = toObject verb f

  toObject _verb (WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]

instance ToJSON (MA.Value era) where
  toJSON (MA.Value l ps) =
    Aeson.object
      [ "lovelace" .= toJSON l
      , "policies" .= toJSON ps
      ]

instance ToJSON (MA.PolicyID era) where
  toJSON (MA.PolicyID (ScriptHash h)) = Aeson.String (hashToTextAsHex h)

instance ToJSONKey (MA.PolicyID era) where
  toJSONKey = ToJSONKeyText render (Aeson.text . render)
    where
      render (MA.PolicyID (ScriptHash h)) = hashToTextAsHex h

instance ToJSON MA.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . MA.assetName

instance ToJSONKey MA.AssetName where
  toJSONKey = ToJSONKeyText render (Aeson.text . render)
    where
      render = Text.decodeLatin1 . B16.encode . MA.assetName

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
         , ToJSON (Core.Delta (Core.Value era))
         , ToJSON (Core.TxOut era)
         , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
         ) => ToObject (MA.UtxoPredicateFailure era) where
  toObject _verb (MA.BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (MA.OutsideValidityIntervalUTxO validityInterval slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  toObject _verb (MA.MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  toObject _verb MA.InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (MA.FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (MA.ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject _verb (MA.WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (MA.WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (MA.OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject verb (MA.UpdateFailure f) = toObject verb f
  toObject _verb (MA.OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  toObject _verb MA.TriesToForgeADA =
    mkObject [ "kind" .= String "TriesToForgeADA" ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance ToObject (PpupPredicateFailure era) where
  toObject _verb (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mkObject [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  toObject _verb (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mkObject [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (show votingPeriod)
             ]
  toObject _verb (PVCannotFollowPPUP badPv) =
    mkObject [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance ( ShelleyBasedEra era
         , ToObject (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToObject (DelegsPredicateFailure era) where
  toObject _verb (DelegateeNotRegisteredDELEG targetPool) =
    mkObject [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  toObject _verb (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mkObject [ "kind" .= String "WithdrawalsNotInRewardsDELEGS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  toObject verb (DelplFailure f) = toObject verb f


instance ( ToObject (PredicateFailure (Core.EraRule "POOL" era))
         , ToObject (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToObject (DelplPredicateFailure era) where
  toObject verb (PoolFailure f) = toObject verb f
  toObject verb (DelegFailure f) = toObject verb f

instance ToObject (DelegPredicateFailure era) where
  toObject _verb (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential already registered"
             ]
  toObject _verb (StakeKeyInRewardsDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyInRewardsDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential registered in rewards map"
             ]
  toObject _verb (StakeKeyNotRegisteredDELEG notRegistered) =
    mkObject [ "kind" .= String "StakeKeyNotRegisteredDELEG"
             , "credential" .= String (textShow notRegistered)
             , "error" .= String "Staking credential not registered"
             ]
  toObject _verb (StakeKeyNonZeroAccountBalanceDELEG remBalance) =
    mkObject [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG"
             , "remainingBalance" .= remBalance
             ]
  toObject _verb (StakeDelegationImpossibleDELEG unregistered) =
    mkObject [ "kind" .= String "StakeDelegationImpossibleDELEG"
             , "credential" .= String (textShow unregistered)
             , "error" .= String "Cannot delegate this stake credential because it is not registered"
             ]
  toObject _verb WrongCertificateTypeDELEG =
    mkObject [ "kind" .= String "WrongCertificateTypeDELEG" ]
  toObject _verb (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "GenesisKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key is not in the delegation mapping"
             ]
  toObject _verb (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "DuplicateGenesisDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key has already been delegated to"
             ]
  toObject _verb (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) =
    mkObject [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "neededAmount" .= neededMirAmount
             , "reserves" .= reserves
             ]
  toObject _verb (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) =
    mkObject [ "kind" .= String "MIRCertificateTooLateinEpochDELEG"
             , "currentSlotNo" .= currSlot
             , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
             ]
  toObject _verb (DuplicateGenesisVRFDELEG vrfKeyHash) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]


instance ToObject (PoolPredicateFailure era) where
  toObject _verb (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) =
    mkObject [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL"
             , "unregisteredKeyHash" .= String (textShow unregStakePool)
             , "error" .= String "This stake pool key hash is unregistered"
             ]
  toObject _verb (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) =
    mkObject [ "kind" .= String "StakePoolRetirementWrongEpochPOOL"
             , "currentEpoch" .= String (textShow currentEpoch)
             , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
             , "maxEpochForRetirement" .= String (textShow maxRetireEpoch)
             ]
  toObject _verb (StakePoolCostTooLowPOOL certCost protCost) =
    mkObject [ "kind" .= String "StakePoolCostTooLowPOOL"
             , "certificateCost" .= String (textShow certCost)
             , "protocolParCost" .= String (textShow protCost)
             , "error" .= String "The stake pool cost is too low"
             ]


-- Apparently this should never happen according to the Shelley exec spec
  toObject _verb (WrongCertificateTypePOOL index) =
    case index of
      0 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Delegation certificate"
                    ]
      1 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: MIR certificate"
                    ]
      2 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Genesis certificate"
                    ]
      k -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "certificateType" .= k
                    , "error" .= String "Wrong certificate type: Unknown certificate type"
                    ]


instance ( ToObject (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToObject (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToObject (TickPredicateFailure era) where
  toObject verb (NewEpochFailure f) = toObject verb f
  toObject verb (RupdFailure f) = toObject verb f

instance ToObject TicknPredicateFailure where
  toObject _verb x = case x of {} -- no constructors

instance ( ToObject (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToObject (PredicateFailure (Core.EraRule "MIR" era))
         ) => ToObject (NewEpochPredicateFailure era) where
  toObject verb (EpochFailure f) = toObject verb f
  toObject verb (MirFailure f) = toObject verb f
  toObject _verb (CorruptRewardUpdate update) =
    mkObject [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (show update) ]


instance ( ToObject (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToObject (PredicateFailure (Core.EraRule "SNAP" era))
         , ToObject (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToObject (EpochPredicateFailure era) where
  toObject verb (PoolReapFailure f) = toObject verb f
  toObject verb (SnapFailure f) = toObject verb f
  toObject verb (UpecFailure f) = toObject verb f


instance ToObject (PoolreapPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (SnapPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance ToObject (NewppPredicateFailure era) where
  toObject _verb (UnexpectedDepositPot outstandingDeposits depositPot) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance ToObject (MirPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (RupdPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance Core.Crypto crypto => ToObject (PrtclPredicateFailure crypto) where
  toObject  verb (OverlayFailure f) = toObject verb f
  toObject  verb (UpdnFailure f) = toObject verb f


instance Core.Crypto crypto => ToObject (OverlayPredicateFailure crypto) where
  toObject _verb (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) =
    mkObject [ "kind" .= String "UnknownGenesisKeyOVERLAY"
             , "unknownKeyHash" .= String (textShow genKeyHash)
             ]
  toObject _verb (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mkObject [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "leaderElectionValue" .= String (textShow leaderElecVal)
             ]
  toObject _verb (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mkObject [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "blockNonce" .= String (textShow blockNonce)
             ]
  toObject _verb (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) =
    mkObject [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "poolHash" .= textShow issuerHash
             , "registeredVRFKeHash" .= textShow regVRFKeyHash
             , "unregisteredVRFKeyHash" .= textShow unregVRFKeyHash
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  toObject _verb (VRFKeyUnknown (KeyHash kHash)) =
    mkObject [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "keyHash" .= String (textShow kHash)
             ]
  toObject _verb (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mkObject [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "leaderElectionValue" .= String (textShow leadElecVal)
             , "delegationPoolWeight" .= String (textShow weightOfDelegPool)
             , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
             ]
  toObject _verb (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mkObject [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= String (textShow notActiveSlotNo)
             ]
  toObject _verb (WrongGenesisColdKeyOVERLAY actual expected) =
    mkObject [ "kind" .= String "WrongGenesisColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  toObject _verb (WrongGenesisVRFKeyOVERLAY issuer actual expected) =
    mkObject [ "kind" .= String "WrongGenesisVRFKeyOVERLAY"
             , "issuer" .= issuer
             , "actual" .= actual
             , "expected" .= expected ]
  toObject verb (OcertFailure f) = toObject verb f


instance ToObject (OcertPredicateFailure crypto) where
  toObject _verb (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) =
    mkObject [ "kind" .= String "KESBeforeStartOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "currentKESPeriod" .= String (textShow current)
             , "error" .= String "Your operational certificate's KES start period \
                                 \is before the KES current period."
             ]
  toObject _verb (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
    mkObject [ "kind" .= String "KESAfterEndOCERT"
             , "currentKESPeriod" .= String (textShow current)
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "maxKESEvolutions" .= String  (textShow maxKESEvolutions)
             , "error" .= String "The operational certificate's KES start period is \
                                 \greater than the max number of KES + the KES current period"
             ]
  toObject _verb (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
    mkObject [ "kind" .= String "CounterTooSmallOCert"
             , "currentKESCounter" .= String (textShow currentKESCounter)
             , "lastKESCounter" .= String (textShow lastKEScounterUsed)
             , "error" .= String "The operational certificate's last KES counter is greater \
                                 \than the current KES counter."
             ]
  toObject _verb (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) =
    mkObject [ "kind" .= String "InvalidSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertKESStartPeriod)
             , "opCertCounter" .= String (textShow oCertCounter)
             ]
  toObject _verb (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) =
    mkObject [ "kind" .= String "InvalidKesSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow startKESPeriod)
             , "opCertKESCurrentPeriod" .= String (textShow currKESPeriod)
             , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
             , "error" .= err ]
  toObject _verb (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) =
    mkObject [ "kind" .= String "NoCounterForKeyHashOCERT"
             , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
             , "error" .= String "A counter was not found for this stake pool key hash"
             ]


instance ToObject (UpdnPredicateFailure crypto) where
  toObject _verb x = case x of {} -- no constructors

instance ToObject (UpecPredicateFailure era) where
  toObject _verb (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "totalOutstanding" .=  String (textShow totalOutstanding)
             , "depositPot" .= String (textShow depositPot)
             ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk

-- Common to cardano-cli

deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)

deriving instance Core.Crypto crypto => ToJSON (TxIn crypto)
deriving newtype instance ToJSON (TxId crypto)
deriving newtype instance ToJSON DeltaCoin
instance Core.Crypto crypto => ToJSONKey (TxIn crypto) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: Core.Crypto crypto => TxIn crypto -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

instance (ShelleyBasedEra era, ToJSON (Core.Value era)) => ToJSON (TxOut era) where
  toJSON (TxOut addr amount) =
    Aeson.object
      [ "address" .= addr
      , "amount" .= amount
      ]

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex
