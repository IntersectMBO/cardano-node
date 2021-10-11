{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Era.Shelley ()
  where


import           Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cardano.Api as Api
import           Cardano.Api.Orphans ()
import qualified Cardano.Api.Shelley as Api
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Logging
import           Cardano.Prelude
import           Cardano.Slotting.Block (BlockNo (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo,
                     blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)

import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCannotForge (..))
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock, labBlockNo)
import           Cardano.Protocol.TPraos.Rules.OCert
import           Cardano.Protocol.TPraos.Rules.Overlay
import           Cardano.Protocol.TPraos.Rules.Updn


import qualified Cardano.Ledger.Alonzo as Alonzo
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo
import           Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Core
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA


-- TODO: this should be exposed via Cardano.Api
import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)

import           Cardano.Ledger.Shelley.Rules.Bbody
import           Cardano.Ledger.Shelley.Rules.Chain
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

import           Cardano.Tracing.OrphanInstances.Shelley ()

{- HLINT ignore "Use :" -}

--
-- | instances of @LogFormatting@
--
-- NOTE: this list is sorted in roughly topological order.

instance (  ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock era)))
         ,  ShelleyBasedEra era)
         => LogFormatting (GenTx (ShelleyBlock era)) where
  forMachine dtal tx =
    mkObject $
        ( "txid" .= txId tx )
      : [ "tx"   .= condense tx | dtal == DDetailed ]

-- instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock era))) where
--   toJSON i = toJSON (condense i)

instance ShelleyBasedEra era => LogFormatting (Header (ShelleyBlock era)) where
  forMachine _dtal b = mkObject
        [ "kind" .= String "ShelleyBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => LogFormatting (ApplyTxError era) where
  forMachine dtal (ApplyTxError predicateFailures) =
    HMS.unions $ map (forMachine dtal) predicateFailures

instance Core.Crypto era => LogFormatting (TPraosCannotForge era) where
  forMachine _dtal (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) =
    mkObject
      [ "kind" .= String "TPraosCannotForgeKeyNotUsableYet"
      , "keyStart" .= keyStartPeriod
      , "wallClock" .= wallClockPeriod
      ]
  forMachine _dtal (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) =
    mkObject
      [ "kind" .= String "TPraosCannotLeadWrongVRF"
      , "expected" .= genDlgVRFHash
      , "actual" .= coreNodeVRFHash
      ]

-- TODO Tracers RemoveOld
-- deriving newtype instance ToJSON KESPeriod

instance LogFormatting HotKey.KESInfo where
  forMachine _dtal forgeStateInfo =
    let maxKesEvos = endKesPeriod - startKesPeriod
        oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (oCertExpiryKesPeriod - currKesPeriod)
    in
      if kesPeriodsUntilExpiry > 7
        then mkObject
              [ "kind" .= String "KESInfo"
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod
              , "evolution" .= endKesPeriod
              ]
        else mkObject
              [ "kind" .= String "ExpiryLogMessage"
              , "keyExpiresIn" .= kesPeriodsUntilExpiry
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod
              , "evolution" .= endKesPeriod
              ]
    where
    HotKey.KESInfo
      { kesStartPeriod = KESPeriod startKesPeriod
      , kesEvolution = currKesPeriod
      , kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  forHuman forgeStateInfo =
    let maxKesEvos = endKesPeriod - startKesPeriod
        oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (oCertExpiryKesPeriod - currKesPeriod)
    in if kesPeriodsUntilExpiry > 7
      then "KES info startPeriod  " <> show startKesPeriod
            <> " currPeriod " <> show currKesPeriod
            <> " endPeriod " <> show endKesPeriod
             <> (Text.pack . show) kesPeriodsUntilExpiry
             <> " KES periods."
      else "Operational key will expire in "
             <> (Text.pack . show) kesPeriodsUntilExpiry
             <> " KES periods."
    where
    HotKey.KESInfo
      { kesStartPeriod = KESPeriod startKesPeriod
      , kesEvolution = currKesPeriod
      , kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  asMetrics forgeStateInfo =
      let maxKesEvos = endKesPeriod - startKesPeriod
          oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
          -- TODO JNF: What is the sense of it?
      in  [
            IntM "operationalCertificateStartKESPeriod"
              (fromIntegral startKesPeriod)
          , IntM "operationalCertificateExpiryKESPeriod"
              (fromIntegral (startKesPeriod + maxKesEvos))
          , IntM "currentKESPeriod"
              (fromIntegral currKesPeriod)
          , IntM "remainingKESPeriods"
              (fromIntegral (max 0 (oCertExpiryKesPeriod - currKesPeriod)))
          ]
    where
    HotKey.KESInfo
      { kesStartPeriod = KESPeriod startKesPeriod
      , kesEvolution = currKesPeriod
      , kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo


instance LogFormatting HotKey.KESEvolutionError where
  forMachine dtal (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  forMachine dtal (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "BBODY" era))
         ) => LogFormatting (ShelleyLedgerError era) where
  forMachine dtal (BBodyError (BlockTransitionError fs)) =
    mkObject [ "kind" .= String "BBodyError"
             , "failures" .= map (forMachine dtal) fs
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Core.PParamsDelta era)
         ) => LogFormatting (ShelleyLedgerUpdate era) where
  forMachine dtal (ShelleyUpdatedProtocolUpdates updates) =
    mkObject [ "kind" .= String "ShelleyUpdatedProtocolUpdates"
             , "updates" .= map (forMachine dtal) updates
             ]

instance (Ledger.Era era, ToJSON (Core.PParamsDelta era))
         => LogFormatting (ProtocolUpdate era) where
  forMachine dtal ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} =
    mkObject [ "proposal" .= forMachine dtal protocolUpdateProposal
             , "state"    .= forMachine dtal protocolUpdateState
             ]

instance ToJSON (Core.PParamsDelta era)
         => LogFormatting (UpdateProposal era) where
  forMachine _dtal UpdateProposal{proposalParams, proposalVersion, proposalEpoch} =
    mkObject [ "params"  .= proposalParams
             , "version" .= proposalVersion
             , "epoch"   .= proposalEpoch
             ]

instance Core.Crypto crypto => LogFormatting (UpdateState crypto) where
  forMachine _dtal UpdateState{proposalVotes, proposalReachedQuorum} =
    mkObject [ "proposal"      .= proposalVotes
             , "reachedQuorum" .= proposalReachedQuorum
             ]

instance Core.Crypto crypto => LogFormatting (ChainTransitionError crypto) where
  forMachine dtal (ChainTransitionError fs) =
    mkObject [ "kind" .= String "ChainTransitionError"
             , "failures" .= map (forMachine dtal) fs
             ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (Core.EraRule "UTXOW" era))
         , LogFormatting (PredicateFailure (Core.EraRule "BBODY" era))
         , LogFormatting (PredicateFailure (Core.EraRule "TICK" era))
         , LogFormatting (PredicateFailure (Core.EraRule "TICKN" era))
         ) => LogFormatting (ChainPredicateFailure era) where
  forMachine _dtal (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) =
    mkObject [ "kind" .= String "HeaderSizeTooLarge"
             , "headerSize" .= hdrSz
             , "maxHeaderSize" .= maxHdrSz
             ]
  forMachine _dtal (BlockSizeTooLargeCHAIN blkSz maxBlkSz) =
    mkObject [ "kind" .= String "BlockSizeTooLarge"
             , "blockSize" .= blkSz
             , "maxBlockSize" .= maxBlkSz
             ]
  forMachine _dtal (ObsoleteNodeCHAIN currentPtcl supportedPtcl) =
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
  forMachine dtal (BbodyFailure f) = forMachine dtal f
  forMachine dtal (TickFailure  f) = forMachine dtal f
  forMachine dtal (TicknFailure  f) = forMachine dtal f
  forMachine dtal (PrtclFailure f) = forMachine dtal f
  forMachine dtal (PrtclSeqFailure f) = forMachine dtal f

instance LogFormatting (PrtlSeqFailure crypto) where
  forMachine _dtal (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mkObject [ "kind" .= String "WrongSlotInterval"
             , "lastSlot" .= lastSlot
             , "currentSlot" .= currSlot
             ]
  forMachine _dtal (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mkObject [ "kind" .= String "WrongBlockNo"
             , "lastAppliedBlockNo" .= showLastAppBlockNo lab
             , "currentBlockNo" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  forMachine _dtal (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mkObject [ "kind" .= String "WrongBlockSequence"
             , "lastAppliedBlockHash" .= String (textShow lastAppliedHash)
             , "currentBlockHash" .= String (textShow currentHash)
             ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGER" era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGERS" era))
         ) => LogFormatting (BbodyPredicateFailure era) where
  forMachine _dtal (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) =
    mkObject [ "kind" .= String "WrongBlockBodySizeBBODY"
             , "actualBlockBodySize" .= actualBodySz
             , "claimedBlockBodySize" .= claimedBodySz
             ]
  forMachine _dtal (InvalidBodyHashBBODY actualHash claimedHash) =
    mkObject [ "kind" .= String "InvalidBodyHashBBODY"
             , "actualBodyHash" .= textShow actualHash
             , "claimedBodyHash" .= textShow claimedHash
             ]
  forMachine dtal (LedgersFailure f) = forMachine dtal f


instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => LogFormatting (LedgersPredicateFailure era) where
  forMachine dtal (LedgerFailure f) = forMachine dtal f


instance ( ShelleyBasedEra era
         , ToJSON (Core.AuxiliaryDataHash (Ledger.Crypto era))
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "DELEGS" era))
         , LogFormatting (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => LogFormatting (LedgerPredicateFailure era) where
  forMachine dtal (UtxowFailure f) = forMachine dtal f
  forMachine dtal (DelegsFailure f) = forMachine dtal f

instance LogFormatting (AlonzoPredFail (Alonzo.AlonzoEra StandardCrypto)) where
  forMachine dtal (WrappedShelleyEraFailure utxoPredFail) =
    forMachine dtal utxoPredFail
  forMachine _ (MissingRedeemers scripts) =
    mkObject [ "kind" .= String "MissingRedeemers"
             , "scripts" .= renderMissingRedeemers scripts
             ]
  forMachine _ (MissingRequiredDatums required received) =
    mkObject [ "kind" .= String "MissingRequiredDatums"
             , "required" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList required)
             , "received" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList received)
             ]
  forMachine _ (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) =
    mkObject [ "kind" .= String "PPViewHashesDontMatch"
             , "fromTxBody" .= renderScriptIntegrityHash
                                  (strictMaybeToMaybe ppHashInTxBody)
             , "fromPParams" .= renderScriptIntegrityHash
                                  (strictMaybeToMaybe ppHashFromPParams)
             ]
  forMachine _ (MissingRequiredSigners missingKeyWitnesses) =
    mkObject [ "kind" .= String "MissingRequiredSigners"
             , "witnesses" .= Set.toList missingKeyWitnesses
             ]
  forMachine _ (UnspendableUTxONoDatumHash txins) =
    mkObject [ "kind" .= String "MissingRequiredSigners"
             , "txins" .= Set.toList txins
             ]
  forMachine _ (NonOutputSupplimentaryDatums disallowed acceptable) =
    mkObject [ "kind" .= String "NonOutputSupplimentaryDatums"
             , "disallowed" .= Set.toList disallowed
             , "acceptable" .= Set.toList acceptable
             ]
  forMachine _ (ExtraRedeemers rdmrs) =
    mkObject [ "kind" .= String "ExtraRedeemers"
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
  renderTuple (scriptPurpose, sHash) =  renderScriptHash sHash .= renderScriptPurpose scriptPurpose

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
         , ToJSON (Core.AuxiliaryDataHash (Ledger.Crypto era))
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (Core.EraRule "UTXO" era))
         ) => LogFormatting (UtxowPredicateFailure era) where
  forMachine _dtal (InvalidWitnessesUTXOW wits') =
    mkObject [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalidWitnesses" .= map textShow wits'
             ]
  forMachine _dtal (MissingVKeyWitnessesUTXOW (WitHashes wits')) =
    mkObject [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missingWitnesses" .= wits'
             ]
  forMachine _dtal (MissingScriptWitnessesUTXOW missingScripts) =
    mkObject [ "kind" .= String "MissingScriptWitnessesUTXOW"
             , "missingScripts" .= missingScripts
             ]
  forMachine _dtal (ScriptWitnessNotValidatingUTXOW failedScripts) =
    mkObject [ "kind" .= String "ScriptWitnessNotValidatingUTXOW"
             , "failedScripts" .= failedScripts
             ]
  forMachine dtal (UtxoFailure f) = forMachine dtal f
  forMachine _dtal (MIRInsufficientGenesisSigsUTXOW genesisSigs) =
    mkObject [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW"
             , "genesisSigs" .= genesisSigs
             ]
  forMachine _dtal (MissingTxBodyMetadataHash metadataHash) =
    mkObject [ "kind" .= String "MissingTxBodyMetadataHash"
             , "metadataHash" .= metadataHash
             ]
  forMachine _dtal (MissingTxMetadata txBodyMetadataHash) =
    mkObject [ "kind" .= String "MissingTxMetadata"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             ]
  forMachine _dtal (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    mkObject [ "kind" .= String "ConflictingMetadataHash"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             , "fullMetadataHash" .= fullMetadataHash
             ]
  forMachine _dtal InvalidMetadata =
    mkObject [ "kind" .= String "InvalidMetadata"
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , LogFormatting (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => LogFormatting (UtxoPredicateFailure era) where
  forMachine _dtal (BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (ExpiredUTxO ttl slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  forMachine _dtal (MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  forMachine _dtal (OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  forMachine _dtal (OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  forMachine _dtal (ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine dtal (UpdateFailure f) = forMachine dtal f

  forMachine _dtal (WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]

-- instance ToJSON MA.ValidityInterval where
--   toJSON vi =
--     Aeson.object $
--         [ "invalidBefore"    .= x | x <- mbfield (MA.invalidBefore    vi) ]
--      ++ [ "invalidHereafter" .= x | x <- mbfield (MA.invalidHereafter vi) ]
--     where
--       mbfield SNothing  = []
--       mbfield (SJust x) = [x]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToJSON MA.ValidityInterval
         , LogFormatting (PredicateFailure (Core.EraRule "PPUP" era))
         ) => LogFormatting (MA.UtxoPredicateFailure era) where
  forMachine _dtal (MA.BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (MA.OutsideValidityIntervalUTxO validityInterval slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  forMachine _dtal (MA.MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  forMachine _dtal MA.InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (MA.FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  forMachine _dtal (MA.ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine _dtal (MA.WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (MA.WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  forMachine _dtal (MA.OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  forMachine dtal (MA.UpdateFailure f) = forMachine dtal f
  forMachine _dtal (MA.OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal MA.TriesToForgeADA =
    mkObject [ "kind" .= String "TriesToForgeADA" ]
  forMachine _dtal (MA.OutputTooBigUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance Core.Crypto (Ledger.Crypto era) => LogFormatting (PpupPredicateFailure era) where
  forMachine _dtal (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mkObject [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  forMachine _dtal (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mkObject [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (show votingPeriod)
             ]
  forMachine _dtal (PVCannotFollowPPUP badPv) =
    mkObject [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (Core.EraRule "DELPL" era))
         ) => LogFormatting (DelegsPredicateFailure era) where
  forMachine _dtal (DelegateeNotRegisteredDELEG targetPool) =
    mkObject [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  forMachine _dtal (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mkObject [ "kind" .= String "WithdrawalsNotInRewardsDELEGS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  forMachine dtal (DelplFailure f) = forMachine dtal f


instance ( LogFormatting (PredicateFailure (Core.EraRule "POOL" era))
         , LogFormatting (PredicateFailure (Core.EraRule "DELEG" era))
         , Crypto.HashAlgorithm (Core.HASH (Ledger.Crypto era))
         ) => LogFormatting (DelplPredicateFailure era) where
  forMachine dtal (PoolFailure f)  = forMachine dtal f
  forMachine dtal (DelegFailure f) = forMachine dtal f

instance     Crypto.HashAlgorithm (Core.HASH (Ledger.Crypto era))
          => LogFormatting (DelegPredicateFailure era) where
  forMachine _dtal (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential already registered"
             ]
  forMachine _dtal (StakeKeyInRewardsDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyInRewardsDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential registered in rewards map"
             ]
  forMachine _dtal (StakeKeyNotRegisteredDELEG notRegistered) =
    mkObject [ "kind" .= String "StakeKeyNotRegisteredDELEG"
             , "credential" .= String (textShow notRegistered)
             , "error" .= String "Staking credential not registered"
             ]
  forMachine _dtal (StakeKeyNonZeroAccountBalanceDELEG remBalance) =
    mkObject [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG"
             , "remainingBalance" .= remBalance
             ]
  forMachine _dtal (StakeDelegationImpossibleDELEG unregistered) =
    mkObject [ "kind" .= String "StakeDelegationImpossibleDELEG"
             , "credential" .= String (textShow unregistered)
             , "error" .= String "Cannot delegate this stake credential because it is not registered"
             ]
  forMachine _dtal WrongCertificateTypeDELEG =
    mkObject [ "kind" .= String "WrongCertificateTypeDELEG" ]
  forMachine _dtal (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "GenesisKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key is not in the delegation mapping"
             ]
  forMachine _dtal (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "DuplicateGenesisDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key has already been delegated to"
             ]
  forMachine _dtal (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) =
    mkObject [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "neededAmount" .= neededMirAmount
             , "reserves" .= reserves
             ]
  forMachine _dtal (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) =
    mkObject [ "kind" .= String "MIRCertificateTooLateinEpochDELEG"
             , "currentSlotNo" .= currSlot
             , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
             ]
  forMachine _dtal (DuplicateGenesisVRFDELEG vrfKeyHash) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]
  forMachine _dtal MIRTransferNotCurrentlyAllowed =
    mkObject [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
             ]
  forMachine _dtal MIRNegativesNotCurrentlyAllowed =
    mkObject [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
             ]
  forMachine _dtal (InsufficientForTransferDELEG mirpot attempted available) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "attempted" .= attempted
             , "available" .= available
             ]
  forMachine _dtal MIRProducesNegativeUpdate =
    mkObject [ "kind" .= String "MIRProducesNegativeUpdate"
             ]

instance LogFormatting (PoolPredicateFailure era) where
  forMachine _dtal (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) =
    mkObject [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL"
             , "unregisteredKeyHash" .= String (textShow unregStakePool)
             , "error" .= String "This stake pool key hash is unregistered"
             ]
  forMachine _dtal (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) =
    mkObject [ "kind" .= String "StakePoolRetirementWrongEpochPOOL"
             , "currentEpoch" .= String (textShow currentEpoch)
             , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
             , "maxEpochForRetirement" .= String (textShow maxRetireEpoch)
             ]
  forMachine _dtal (StakePoolCostTooLowPOOL certCost protCost) =
    mkObject [ "kind" .= String "StakePoolCostTooLowPOOL"
             , "certificateCost" .= String (textShow certCost)
             , "protocolParCost" .= String (textShow protCost)
             , "error" .= String "The stake pool cost is too low"
             ]
  forMachine _dtal (PoolMedataHashTooBig poolID hashSize) =
    mkObject [ "kind" .= String "PoolMedataHashTooBig"
             , "hashSize" .= String (textShow poolID)
             , "poolID" .= String (textShow hashSize)
             , "error" .= String "The stake pool metadata hash is too large"
             ]

-- Apparently this should never happen according to the Shelley exec spec
  forMachine _dtal (WrongCertificateTypePOOL index) =
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

  forMachine _dtal (WrongNetworkPOOL networkId listedNetworkId poolId) =
    mkObject [ "kind" .= String "WrongNetworkPOOL"
             , "networkId" .= String (textShow networkId)
             , "listedNetworkId" .= String (textShow listedNetworkId)
             , "poolId" .= String (textShow poolId)
             , "error" .= String "Wrong network ID in pool registration certificate"
             ]


instance ( LogFormatting (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , LogFormatting (PredicateFailure (Core.EraRule "RUPD" era))
         ) => LogFormatting (TickPredicateFailure era) where
  forMachine dtal (NewEpochFailure f) = forMachine dtal f
  forMachine dtal (RupdFailure f)     = forMachine dtal f

instance LogFormatting TicknPredicateFailure where
  forMachine _dtal x = case x of {} -- no constructors

instance ( LogFormatting (PredicateFailure (Core.EraRule "EPOCH" era))
         , LogFormatting (PredicateFailure (Core.EraRule "MIR" era))
         ) => LogFormatting (NewEpochPredicateFailure era) where
  forMachine dtal (EpochFailure f) = forMachine dtal f
  forMachine dtal (MirFailure f) = forMachine dtal f
  forMachine _dtal (CorruptRewardUpdate update) =
    mkObject [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (show update) ]


instance ( LogFormatting (PredicateFailure (Core.EraRule "POOLREAP" era))
         , LogFormatting (PredicateFailure (Core.EraRule "SNAP" era))
         , LogFormatting (PredicateFailure (Core.EraRule "UPEC" era))
         ) => LogFormatting (EpochPredicateFailure era) where
  forMachine dtal (PoolReapFailure f) = forMachine dtal f
  forMachine dtal (SnapFailure f)     = forMachine dtal f
  forMachine dtal (UpecFailure f)     = forMachine dtal f


instance LogFormatting (PoolreapPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance LogFormatting (SnapPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance LogFormatting (NewppPredicateFailure era) where
  forMachine _dtal (UnexpectedDepositPot outstandingDeposits depositPot) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance LogFormatting (MirPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance LogFormatting (RupdPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance Core.Crypto crypto => LogFormatting (PrtclPredicateFailure crypto) where
  forMachine  dtal (OverlayFailure f) = forMachine dtal f
  forMachine  dtal (UpdnFailure f)    = forMachine dtal f


instance Core.Crypto crypto => LogFormatting (OverlayPredicateFailure crypto) where
  forMachine _dtal (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) =
    mkObject [ "kind" .= String "UnknownGenesisKeyOVERLAY"
             , "unknownKeyHash" .= String (textShow genKeyHash)
             ]
  forMachine _dtal (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mkObject [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "leaderElectionValue" .= String (textShow leaderElecVal)
             ]
  forMachine _dtal (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mkObject [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "blockNonce" .= String (textShow blockNonce)
             ]
  forMachine _dtal (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) =
    mkObject [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "poolHash" .= textShow issuerHash
             , "registeredVRFKeHash" .= textShow regVRFKeyHash
             , "unregisteredVRFKeyHash" .= textShow unregVRFKeyHash
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  forMachine _dtal (VRFKeyUnknown (KeyHash kHash)) =
    mkObject [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "keyHash" .= String (textShow kHash)
             ]
  forMachine _dtal (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mkObject [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "leaderElectionValue" .= String (textShow leadElecVal)
             , "delegationPoolWeight" .= String (textShow weightOfDelegPool)
             , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
             ]
  forMachine _dtal (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mkObject [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= String (textShow notActiveSlotNo)
             ]
  forMachine _dtal (WrongGenesisColdKeyOVERLAY actual expected) =
    mkObject [ "kind" .= String "WrongGenesisColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  forMachine _dtal (WrongGenesisVRFKeyOVERLAY issuer actual expected) =
    mkObject [ "kind" .= String "WrongGenesisVRFKeyOVERLAY"
             , "issuer" .= issuer
             , "actual" .= actual
             , "expected" .= expected ]
  forMachine dtal (OcertFailure f) = forMachine dtal f


instance LogFormatting (OcertPredicateFailure crypto) where
  forMachine _dtal (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) =
    mkObject [ "kind" .= String "KESBeforeStartOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "currentKESPeriod" .= String (textShow current)
             , "error" .= String "Your operational certificate's KES start period \
                                 \is before the KES current period."
             ]
  forMachine _dtal (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
    mkObject [ "kind" .= String "KESAfterEndOCERT"
             , "currentKESPeriod" .= String (textShow current)
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "maxKESEvolutions" .= String  (textShow maxKESEvolutions)
             , "error" .= String "The operational certificate's KES start period is \
                                 \greater than the max number of KES + the KES current period"
             ]
  forMachine _dtal (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
    mkObject [ "kind" .= String "CounterTooSmallOCert"
             , "currentKESCounter" .= String (textShow currentKESCounter)
             , "lastKESCounter" .= String (textShow lastKEScounterUsed)
             , "error" .= String "The operational certificate's last KES counter is greater \
                                 \than the current KES counter."
             ]
  forMachine _dtal (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) =
    mkObject [ "kind" .= String "InvalidSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertKESStartPeriod)
             , "opCertCounter" .= String (textShow oCertCounter)
             ]
  forMachine _dtal (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) =
    mkObject [ "kind" .= String "InvalidKesSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow startKESPeriod)
             , "opCertKESCurrentPeriod" .= String (textShow currKESPeriod)
             , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
             , "error" .= err ]
  forMachine _dtal (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) =
    mkObject [ "kind" .= String "NoCounterForKeyHashOCERT"
             , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
             , "error" .= String "A counter was not found for this stake pool key hash"
             ]


instance LogFormatting (UpdnPredicateFailure crypto) where
  forMachine _dtal x = case x of {} -- no constructors

instance LogFormatting (UpecPredicateFailure era) where
  forMachine _dtal (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "totalOutstanding" .=  String (textShow totalOutstanding)
             , "depositPot" .= String (textShow depositPot)
             ]

--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------

instance LogFormatting (Alonzo.UtxoPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  forMachine _dtal (Alonzo.BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validtyInterval
             , "slot" .= slot
             ]
  forMachine _dtal (Alonzo.MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize
             ]
  forMachine _dtal Alonzo.InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (Alonzo.FeeTooSmallUTxO minfee currentFee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= currentFee
             ]
  forMachine _dtal (Alonzo.ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine _dtal (Alonzo.WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (Alonzo.WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (Alonzo.OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  forMachine dtal (Alonzo.UtxosFailure predFailure) =
    forMachine dtal predFailure
  forMachine _dtal (Alonzo.OutputBootAddrAttrsTooBig txouts) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= txouts
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal Alonzo.TriesToForgeADA =
    mkObject [ "kind" .= String "TriesToForgeADA" ]
  forMachine _dtal (Alonzo.OutputTooBigUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]
  forMachine _dtal (Alonzo.InsufficientCollateral computedBalance suppliedFee) =
    mkObject [ "kind" .= String "InsufficientCollateral"
             , "balance" .= computedBalance
             , "txfee" .= suppliedFee
             ]
  forMachine _dtal (Alonzo.ScriptsNotPaidUTxO utxos) =
    mkObject [ "kind" .= String "ScriptsNotPaidUTxO"
             , "utxos" .= utxos
             ]
  forMachine _dtal (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) =
    mkObject [ "kind" .= String "ExUnitsTooBigUTxO"
             , "maxexunits" .= pParamsMaxExUnits
             , "exunits" .= suppliedExUnits
             ]
  forMachine _dtal (Alonzo.CollateralContainsNonADA inputs) =
    mkObject [ "kind" .= String "CollateralContainsNonADA"
             , "inputs" .= inputs
             ]
  forMachine _dtal (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) =
    mkObject [ "kind" .= String "WrongNetworkInTxBody"
             , "networkid" .= actualNetworkId
             , "txbodyNetworkId" .= netIdInTxBody
             ]
  forMachine _dtal (Alonzo.OutsideForecast slotNum) =
    mkObject [ "kind" .= String "OutsideForecast"
             , "slot" .= slotNum
             ]
  forMachine _dtal (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) =
    mkObject [ "kind" .= String "TooManyCollateralInputs"
             , "max" .= maxCollateralInputs
             , "inputs" .= numberCollateralInputs
             ]
  forMachine _dtal Alonzo.NoCollateralInputs =
    mkObject [ "kind" .= String "NoCollateralInputs" ]

instance LogFormatting (Alonzo.UtxosPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  forMachine _ (Alonzo.ValidationTagMismatch isValidating reason) =
    mkObject [ "kind" .= String "ValidationTagMismatch"
             , "isvalidating" .= isValidating
             , "reason" .= reason
             ]
  forMachine _ (Alonzo.CollectErrors errors) =
    mkObject [ "kind" .= String "CollectErrors"
             , "errors" .= errors
             ]
  forMachine dtal (Alonzo.UpdateFailure pFailure) =
    forMachine dtal pFailure

-- TODO Trace RemoveOld
-- deriving newtype instance ToJSON Alonzo.IsValid
--
-- instance ToJSON (Alonzo.CollectError StandardCrypto) where
--   toJSON cError =
--     case cError of
--       Alonzo.NoRedeemer sPurpose ->
--         object
--           [ "kind" .= String "CollectError"
--           , "error" .= String "NoRedeemer"
--           , "scriptpurpose" .= renderScriptPurpose sPurpose
--           ]
--       Alonzo.NoWitness sHash ->
--         object
--           [ "kind" .= String "CollectError"
--           , "error" .= String "NoWitness"
--           , "scripthash" .= toJSON sHash
--           ]
--       Alonzo.NoCostModel lang ->
--         object
--           [ "kind" .= String "CollectError"
--           , "error" .= String "NoCostModel"
--           , "language" .= toJSON lang
--           ]
--
-- instance ToJSON Alonzo.TagMismatchDescription where
--   toJSON tmd = case tmd of
--     Alonzo.PassedUnexpectedly ->
--       object
--         [ "kind" .= String "TagMismatchDescription"
--         , "error" .= String "PassedUnexpectedly"
--         ]
--     Alonzo.FailedUnexpectedly forReasons ->
--       object
--         [ "kind" .= String "TagMismatchDescription"
--         , "error" .= String "FailedUnexpectedly"
--         , "reconstruction" .= forReasons
--         ]
--
-- instance ToJSON Alonzo.FailureDescription where
--   toJSON f = case f of
--     Alonzo.OnePhaseFailure t ->
--       object
--         [ "kind" .= String "FailureDescription"
--         , "error" .= String "OnePhaseFailure"
--         , "description" .= t
--         ]
--     Alonzo.PlutusFailure t bs ->
--       object
--         [ "kind" .= String "FailureDescription"
--         , "error" .= String "PlutusFailure"
--         , "description" .= t
--         , "reconstructionDetail" .= bs
--         ]

instance LogFormatting (AlonzoBbodyPredFail (Alonzo.AlonzoEra StandardCrypto)) where
  forMachine _ err = mkObject [ "kind" .= String "AlonzoBbodyPredFail"
                            , "error" .= String (show err)
                            ]


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing  -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk

-- TODO Tracers RemoveOld
-- -- Common to cardano-cli
--
-- deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)
--
-- deriving newtype instance Core.Crypto crypto => ToJSON (TxId crypto)
