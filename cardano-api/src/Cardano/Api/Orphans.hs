{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Api.Orphans () where

import           Data.Aeson (ToJSON (..), Value (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Maybe.Strict (strictMaybeToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Tuple (swap)
import           Data.UMap (Trip (Triple))
import           Data.Word (Word64)
import           GHC.Word (Word8)
import           Numeric.Natural (Natural)

import qualified Cardano.Api.Address as Api
import qualified Cardano.Api.Certificate as Api
import           Cardano.Api.OperationalCertificate (KESPeriod (..))
import           Cardano.Api.Script ()
import qualified Cardano.Api.SerialiseTextEnvelope as Api
import qualified Cardano.Api.TxBody as Api
import           Cardano.Api.Utils (textShow)

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure,
                   AlonzoUtxowPredFailure)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import           Cardano.Ledger.BaseTypes (Network, ProtVer, StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Chain (ChainPredicateFailure (..))
import           Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Ledger.Shelley.API (ApplyTxError (..), BlockTransitionError (..),
                   MIRPot (..), ShelleyUTXO)
import qualified Cardano.Ledger.Shelley.API as Shelley
import           Cardano.Ledger.Shelley.API.Types (ShelleyUTXOW)
import           Cardano.Ledger.Shelley.PParams ()
import           Cardano.Ledger.TxIn (TxIn)
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.AuxiliaryData as Core
import           Cardano.Protocol.TPraos.API (ChainTransitionError (..))
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock (..))
import qualified Cardano.Protocol.TPraos.BHeader as Protocol
import           Cardano.Protocol.TPraos.Rules.OCert (OcertPredicateFailure (..), PredicateFailure)
import           Cardano.Protocol.TPraos.Rules.Overlay (OverlayPredicateFailure (..))
import           Cardano.Protocol.TPraos.Rules.Prtcl (PrtclPredicateFailure (..),
                   PrtlSeqFailure (..))
import           Cardano.Protocol.TPraos.Rules.Tickn (TicknPredicateFailure)
import           Cardano.Protocol.TPraos.Rules.Updn (UpdnPredicateFailure)
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (WithOrigin, withOriginToMaybe)

import           Ouroboros.Consensus.Block (EpochNo, Header)
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import           Ouroboros.Consensus.Shelley.Ledger (GenTx, ShelleyBasedEra, ShelleyBlock,
                   ShelleyCompatible, ShelleyLedgerError (..),
                   StakeSnapshot (ssGoPool, ssMarkPool, ssSetPool),
                   StakeSnapshots (ssGoTotal, ssMarkTotal, ssSetTotal, ssStakeSnapshots))
import           Ouroboros.Consensus.Shelley.Ledger.Inspect (ProtocolUpdate (..),
                   ShelleyLedgerUpdate (..), UpdateProposal (..), UpdateState (..))
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)


import           Cardano.Ledger.Shelley.Rules (PPUPPredFailure, ShelleyBbodyPredFailure (..),
                   ShelleyDelegPredFailure (..), ShelleyDelegsPredFailure (..),
                   ShelleyDelplPredFailure (..), ShelleyEpochPredFailure (..),
                   ShelleyLedgerPredFailure (..), ShelleyLedgersPredFailure (..),
                   ShelleyNewEpochPredFailure (..), ShelleyNewppPredFailure (..),
                   ShelleyPoolPredFailure (..), ShelleyPpupPredFailure (..),
                   ShelleyTickPredFailure (..), ShelleyUpecPredFailure (..),
                   ShelleyUtxoPredFailure (..), ShelleyUtxowPredFailure (..))

import           Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Api as Allegra
import qualified Cardano.Ledger.Api as Alonzo

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance (ToJSON coin, ToJSON ptr, ToJSON pool) => ToJSON (Trip coin ptr pool) where
  toJSON = object . toTripPair
  toEncoding = Aeson.pairs . mconcat . toTripPair

toTripPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  ) => Trip coin ptr pool -> [a]
toTripPair (Triple !coin !ptr !pool) =
  [ "coin" .= coin
  , "ptr" .= ptr
  , "pool" .= pool
  ]

instance Crypto.Crypto crypto => ToJSON (Consensus.StakeSnapshots crypto) where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair :: (Aeson.KeyValue a, Crypto.Crypto crypto) => Consensus.StakeSnapshots crypto -> [a]
stakeSnapshotsToPair Consensus.StakeSnapshots
    { Consensus.ssStakeSnapshots
    , Consensus.ssMarkTotal
    , Consensus.ssSetTotal
    , Consensus.ssGoTotal
    } =
    [ "pools" .= ssStakeSnapshots
    , "total" .= object
      [ "stakeMark" .= ssMarkTotal
      , "stakeSet" .= ssSetTotal
      , "stakeGo" .= ssGoTotal
      ]
    ]

instance ToJSON (Consensus.StakeSnapshot crypto) where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue a => Consensus.StakeSnapshot crypto -> [a]
stakeSnapshotToPair Consensus.StakeSnapshot
    { Consensus.ssMarkPool
    , Consensus.ssSetPool
    , Consensus.ssGoPool
    } =
    [ "stakeMark" .= ssMarkPool
    , "stakeSet" .= ssSetPool
    , "stakeGo" .= ssGoPool
    ]

instance ToJSON (PredicateFailure (Core.EraRule "LEDGER" era)) => ToJSON (ApplyTxError era) where
  toJSON (ApplyTxError es) = toJSON es

instance
  (  ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock protocol era)))
  ,  ShelleyBasedEra era)
  => ToJSON (GenTx (ShelleyBlock protocol era)) where
  toJSON tx =
    object
      [ "txid" .= txId tx
      , "tx"   .= condense tx
      ]

instance ShelleyCompatible protocol era => ToJSON (Header (ShelleyBlock protocol era)) where
  toJSON b = object
    [ "kind"      .= toJSON @String "ShelleyBlock"
    , "hash"      .= (condense (blockHash b)  :: String)
    , "slotNo"    .= (condense (blockSlot b)  :: String)
    , "blockNo"   .= (condense (blockNo b)    :: String)
    -- , "delegate"  .= condense (headerSignerVk h)
    ]

instance (Crypto.Crypto crypto, Crypto.VRF crypto ~ crypto) => ToJSON (TPraosCannotForge crypto) where
  toJSON (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) = object
    [ "kind"      .= toJSON @String "TPraosCannotForgeKeyNotUsableYet"
    , "keyStart"  .= (keyStartPeriod  :: KESPeriod)
    , "wallClock" .= (wallClockPeriod :: KESPeriod)
    ]
  toJSON (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) = object
    [ "kind"      .= toJSON @String "TPraosCannotLeadWrongVRF"
    , "expected"  .= genDlgVRFHash
    , "actual"    .= coreNodeVRFHash
    ]

deriving newtype instance ToJSON KESPeriod

instance ToJSON HotKey.KESInfo where
  toJSON HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } = object
    [ "kind"        .= toJSON @String "KESInfo"
    , "startPeriod" .= (kesStartPeriod  :: KESPeriod)
    , "endPeriod"   .= (kesEndPeriod    :: KESPeriod)
    , "evolution"   .= (kesEvolution    :: Word)
    ]

instance ToJSON HotKey.KESEvolutionError where
  toJSON (HotKey.KESCouldNotEvolve kesInfo targetPeriod) = object
    [ "kind"          .= toJSON @String "KESCouldNotEvolve"
    , "kesInfo"       .= (kesInfo       :: HotKey.KESInfo)
    , "targetPeriod"  .= (targetPeriod  :: KESPeriod)
    ]
  toJSON (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) = object
    [ "kind"          .= toJSON @String "KESKeyAlreadyPoisoned"
    , "kesInfo"       .= (kesInfo       :: HotKey.KESInfo)
    , "targetPeriod"  .= (targetPeriod  :: KESPeriod)
    ]

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (PredicateFailure (Shelley.UTxO era))
  -- , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "BBODY" era))
  , ToJSON (BlockTransitionError era)
  ) => ToJSON (ShelleyLedgerError era) where
  toJSON (BBodyError (BlockTransitionError fs)) = object
    [ "kind"      .= toJSON @String "BBodyError"
    , "failures"  .= (map toJSON fs  :: [Value])
    ]

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Ledger.PParamsUpdate era)
  , ToJSON (Ouroboros.Consensus.Shelley.Ledger.Inspect.ProtocolUpdate era)
  ) => ToJSON (ShelleyLedgerUpdate era) where
  toJSON (ShelleyUpdatedProtocolUpdates updates) = object
    [ "kind"    .= toJSON @String "ShelleyUpdatedProtocolUpdates"
    , "updates" .= (map toJSON updates :: [Value])
    ]

instance (Ledger.Era era, ToJSON (Core.PParamsUpdate era))
         => ToJSON (ProtocolUpdate era) where
  toJSON ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} = object
    [ "proposal" .= (protocolUpdateProposal :: UpdateProposal era)
    , "state"    .= (protocolUpdateState    :: UpdateState (Consensus.EraCrypto era))
    ]

instance ToJSON (Ledger.PParamsUpdate era)
         => ToJSON (UpdateProposal era) where
  toJSON UpdateProposal{proposalParams, proposalVersion, proposalEpoch} = object
    [ "params"  .= (proposalParams  :: Ledger.PParamsUpdate era)
    , "version" .= (proposalVersion :: Maybe ProtVer)
    , "epoch"   .= (proposalEpoch   :: EpochNo)
    ]

instance
  ( Crypto.Crypto crypto
  -- , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  ) => ToJSON (UpdateState crypto) where
  toJSON UpdateState{proposalVotes, proposalReachedQuorum} = object
    [ "proposal"      .= (proposalVotes         :: [KeyHash 'Genesis crypto])
    , "reachedQuorum" .= (proposalReachedQuorum :: Bool)
    ]

instance
  ( Crypto.Crypto crypto
  -- , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  -- , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON (PrtclPredicateFailure crypto)
  , ToJSON L.ActiveSlotCoeff
  ) => ToJSON (ChainTransitionError crypto) where
  toJSON (ChainTransitionError fs) = object
    [ "kind"      .= toJSON @String "ChainTransitionError"
    , "failures"  .= (map toJSON fs :: [Value])
    ]

instance ToJSON ChainPredicateFailure where
  toJSON (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) = object
    [ "kind"          .= toJSON @String "HeaderSizeTooLarge"
    , "headerSize"    .= (hdrSz     :: Natural)
    , "maxHeaderSize" .= (maxHdrSz  :: Natural)
    ]
  toJSON (BlockSizeTooLargeCHAIN blkSz maxBlkSz) = object
    [ "kind"          .= toJSON @String "BlockSizeTooLarge"
    , "blockSize"     .= (blkSz     :: Natural)
    , "maxBlockSize"  .= (maxBlkSz  :: Natural)
    ]
  toJSON (ObsoleteNodeCHAIN currentPtcl supportedPtcl) = object
    [ "kind"              .= toJSON @String "ObsoleteNode"
    , "explanation"       .= toJSON @String explanation
    , "currentProtocol"   .= currentPtcl
    , "supportedProtocol" .= supportedPtcl
    ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."

instance
  ( ToJSON (Protocol.PrevHash crypto)
  , ToJSON (WithOrigin (LastAppliedBlock crypto))
  , ToJSON BlockNo
  ) => ToJSON (PrtlSeqFailure crypto) where
  toJSON (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) = object
    [ "kind"        .= toJSON @String "WrongSlotInterval"
    , "lastSlot"    .= (lastSlot :: Word64)
    , "currentSlot" .= (currSlot :: Word64)
    ]
  toJSON (WrongBlockNoPrtclSeq lab currentBlockNo) = object
    [ "kind"                .= toJSON @String "WrongBlockNo"
    , "lastAppliedBlockNo"  .= (showLastAppBlockNo lab :: Text)
    , "currentBlockNo"      .= (toJSON @Text . textShow $ unBlockNo currentBlockNo)
    ]
  toJSON (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) = object
    [ "kind"                  .= toJSON @String "WrongBlockSequence"
    , "lastAppliedBlockHash"  .= toJSON @Text (textShow lastAppliedHash)
    , "currentBlockHash"      .= toJSON @Text (textShow currentHash)
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (Ledger.EraRule "LEDGERS" era))
         ) => ToJSON (ShelleyBbodyPredFailure era) where
  toJSON (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) = object
    [ "kind"                  .= toJSON @String "WrongBlockBodySizeBBODY"
    , "actualBlockBodySize"   .= (actualBodySz  :: Int)
    , "claimedBlockBodySize"  .= (claimedBodySz :: Int)
    ]
  toJSON (InvalidBodyHashBBODY actualHash claimedHash) = object
    [ "kind"            .= toJSON @String "InvalidBodyHashBBODY"
    , "actualBodyHash"  .= (textShow actualHash   :: Text)
    , "claimedBodyHash" .= (textShow claimedHash  :: Text)
    ]
  toJSON (LedgersFailure f) = toJSON f


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (ShelleyUTXO era))
         , ToJSON (PredicateFailure (ShelleyUTXOW era))
         , ToJSON (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToJSON (ShelleyLedgersPredFailure era) where
  toJSON (LedgerFailure f) = object
    [ "kind"  .= toJSON @String "LedgerFailure"
    , "value" .= f
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (ShelleyUTXO era))
         , ToJSON (PredicateFailure (ShelleyUTXOW era))
         , ToJSON (PredicateFailure (Core.EraRule "DELEGS" era))
         , ToJSON (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => ToJSON (ShelleyLedgerPredFailure era) where
  toJSON (UtxowFailure f) = object
    [ "kind"  .= toJSON @String "UtxowFailure"
    , "value" .= (f :: PredicateFailure (Ledger.EraRule "UTXOW" era))
    ]
  toJSON (DelegsFailure f) = object
    [ "kind"  .= toJSON @String "DelegsFailure"
    , "value" .= (f :: PredicateFailure (Ledger.EraRule "DELEGS" era))
    ]



instance ToJSON (Alonzo.ScriptPurpose Consensus.StandardCrypto) where
  toJSON = \case
    Alonzo.Minting pid -> object
      [ "minting" .= toJSON pid
      ]
    Alonzo.Spending txin -> object
      [ "spending" .= Api.fromShelleyTxIn txin
      ]
    Alonzo.Rewarding rwdAcct -> object
      [ "rewarding" .= toJSON @Text (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)
      ]
    Alonzo.Certifying cert -> object
      [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate cert)
      ]

-- instance ToJSONKey (Shelley.ScriptHash Consensus.StandardCrypto) where
--   toJSONKey = contramap (Api.serialiseToRawBytesHexText . Script.ScriptHash) toJSONKey

instance ( ShelleyBasedEra era
         , Ledger.EraCrypto era ~ StandardCrypto
         , ToJSON (PPUPPredFailure era)
         , ToJSON (PredicateFailure (Ledger.EraRule "UTXO" era))
         ) => ToJSON (AlonzoUtxowPredFailure era) where
  toJSON (Alonzo.ShelleyInAlonzoUtxowPredFailure _) = toJSON @String "TODO"
  toJSON (Alonzo.MissingRedeemers scripts) = object
    [ "kind"    .= toJSON @String "MissingRedeemers"
    , "scripts" .= Map.fromList (fmap swap scripts)
    ]
  toJSON (Alonzo.MissingRequiredDatums required received) = object
    [ "kind"      .= toJSON @String "MissingRequiredDatums"
    , "required"  .= (map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList required) :: [Text])
    , "received"  .= (map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList received) :: [Text])
    ]
  toJSON (Alonzo.PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) = object
    [ "kind"        .= toJSON @String "PPViewHashesDontMatch"
    , "fromTxBody"  .= (strictMaybeToMaybe ppHashInTxBody    :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto))
    , "fromPParams" .= (strictMaybeToMaybe ppHashFromPParams :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto))
    ]
  toJSON (Alonzo.MissingRequiredSigners missingKeyWitnesses) = object
    [ "kind"      .= toJSON @String "MissingRequiredSigners"
    , "witnesses" .= (Set.toList missingKeyWitnesses :: [KeyHash 'Witness StandardCrypto])
    ]
  toJSON (Alonzo.UnspendableUTxONoDatumHash txins) = object
    [ "kind"  .= toJSON @String "MissingRequiredSigners"
    , "txins" .= (Set.toList txins :: [TxIn StandardCrypto])
    ]
  toJSON (Alonzo.NonOutputSupplimentaryDatums disallowed acceptable) = object
    [ "kind"        .= toJSON @String "NonOutputSupplimentaryDatums"
    , "disallowed"  .= (Set.toList disallowed :: [Ledger.DataHash StandardCrypto])
    , "acceptable"  .= (Set.toList acceptable :: [Ledger.DataHash StandardCrypto])
    ]
  toJSON (Alonzo.ExtraRedeemers rdmrs) = object
    [ "kind"  .= toJSON @String "ExtraRedeemers"
    , "rdmrs" .= (map (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) rdmrs :: [String])
    ]

instance ( ShelleyBasedEra era
         , Ledger.EraCrypto era ~ StandardCrypto
         , ToJSON (Core.AuxiliaryDataHash (Ledger.EraCrypto era))
         , ToJSON (PredicateFailure (ShelleyUTXO era))
         , ToJSON (PredicateFailure (Core.EraRule "UTXO" era))
         ) => ToJSON (ShelleyUtxowPredFailure era) where
  toJSON (ExtraneousScriptWitnessesUTXOW extraneousScripts) = object
    [ "kind"              .= toJSON @String "InvalidWitnessesUTXOW"
    , "extraneousScripts" .= extraneousScripts
    ]
  toJSON (InvalidWitnessesUTXOW wits') = object
    [ "kind"              .= toJSON @String "InvalidWitnessesUTXOW"
    , "invalidWitnesses"  .= (map textShow wits' :: [Text])
    ]
  toJSON (MissingVKeyWitnessesUTXOW wits') = object
    [ "kind"              .= toJSON @String "MissingVKeyWitnessesUTXOW"
    , "missingWitnesses"  .= wits'
    ]
  toJSON (MissingScriptWitnessesUTXOW missingScripts) = object
    [ "kind"            .= toJSON @String "MissingScriptWitnessesUTXOW"
    , "missingScripts"  .= missingScripts
    ]
  toJSON (ScriptWitnessNotValidatingUTXOW failedScripts) = object
    [ "kind"          .= toJSON @String "ScriptWitnessNotValidatingUTXOW"
    , "failedScripts" .= failedScripts
    ]
  toJSON (UtxoFailure f) = toJSON f
  toJSON (MIRInsufficientGenesisSigsUTXOW genesisSigs) = object
    [ "kind"        .= toJSON @String "MIRInsufficientGenesisSigsUTXOW"
    , "genesisSigs" .= genesisSigs
    ]
  toJSON (MissingTxBodyMetadataHash metadataHash) = object
    [ "kind"          .= toJSON @String "MissingTxBodyMetadataHash"
    , "metadataHash"  .= metadataHash
    ]
  toJSON (MissingTxMetadata txBodyMetadataHash) = object
    [ "kind"                .= toJSON @String "MissingTxMetadata"
    , "txBodyMetadataHash"  .= txBodyMetadataHash
    ]
  toJSON (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) = object
    [ "kind"                .= toJSON @String "ConflictingMetadataHash"
    , "txBodyMetadataHash"  .= txBodyMetadataHash
    , "fullMetadataHash"    .= fullMetadataHash
    ]
  toJSON InvalidMetadata = object
    [ "kind"  .= toJSON @String "InvalidMetadata"
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => ToJSON (ShelleyUtxoPredFailure era) where
  toJSON (BadInputsUTxO badInputs) = object
    [ "kind"      .= toJSON @String "BadInputsUTxO"
    , "badInputs" .= badInputs
    , "error"     .= renderBadInputsUTxOErr badInputs
    ]
  toJSON (ExpiredUTxO ttl slot) = object
    [ "kind" .= toJSON @String "ExpiredUTxO"
    , "ttl"  .= (ttl  :: SlotNo)
    , "slot" .= (slot :: SlotNo)
    ]
  toJSON (MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= toJSON @String "MaxTxSizeUTxO"
    , "size"    .= (txsize    :: Integer)
    , "maxSize" .= (maxtxsize :: Integer)
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooSmallUTxO"
    , "outputs" .= (badOutputs :: [Ledger.TxOut era])
    , "error"   .= toJSON @String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= toJSON @String "OutputBootAddrAttrsTooBig"
    , "outputs" .= (badOutputs :: [Ledger.TxOut era])
    , "error"   .= toJSON @String "The Byron address attributes are too big"
    ]
  toJSON InputSetEmptyUTxO = object
    [ "kind" .= toJSON @String "InputSetEmptyUTxO"
    ]
  toJSON (FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= toJSON @String "FeeTooSmallUTxO"
    , "minimum" .= (minfee  :: Coin)
    , "fee"     .= (txfee   :: Coin)
    ]
  toJSON (ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= toJSON @String "ValueNotConservedUTxO"
    , "consumed"  .= (consumed  :: Ledger.Value era)
    , "produced"  .= (produced  :: Ledger.Value era)
    , "error"     .= renderValueNotConservedErr consumed produced
    ]
  toJSON (UpdateFailure _f) = object
    [ "kind"  .= toJSON @String "UpdateFailure"
    , "value" .= Aeson.Null -- TODO
    ]
  toJSON (WrongNetwork network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetwork"
    , "network" .= (network :: Network)
    , "addrs"   .= addrs
    ]
  toJSON (WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetworkWithdrawal"
    , "network" .= (network :: Network)
    , "addrs"   .= addrs
    ]

instance ToJSON Alonzo.ValidityInterval where
  toJSON vi = object $
        [ "invalidBefore"    .= x | x <- mbfield' (Alonzo.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield' (Alonzo.invalidHereafter vi) ]
    where mbfield' SNothing  = []
          mbfield' (SJust x) = [x]

instance Ledger.Era era => ToJSON (ShelleyPpupPredFailure era) where
  toJSON (NonGenesisUpdatePPUP proposalKeys genesisKeys) = object
    [ "kind"  .= toJSON @String "NonGenesisUpdatePPUP"
    , "keys"  .= proposalKeys Set.\\ genesisKeys
    ]
  toJSON (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) = object
    [ "kind"          .= toJSON @String "PPUpdateWrongEpoch"
    , "currentEpoch"  .= (currEpoch     :: EpochNo)
    , "intendedEpoch" .= (intendedEpoch :: EpochNo)
    , "votingPeriod"  .= toJSON @String (show votingPeriod)
    ]
  toJSON (PVCannotFollowPPUP badPv) = object
    [ "kind"                .= toJSON @String "PVCannotFollowPPUP"
    , "badProtocolVersion"  .= (badPv :: ProtVer)
    ]


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToJSON (ShelleyDelegsPredFailure era) where
  toJSON (DelegateeNotRegisteredDELEG targetPool) = object
    [ "kind"        .= toJSON @String "DelegateeNotRegisteredDELEG"
    , "targetPool"  .= targetPool
    ]
  toJSON (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) = object
    [ "kind"                  .= toJSON @String "WithdrawalsNotInRewardsDELEGS"
    , "incorrectWithdrawals"  .= incorrectWithdrawals -- :: Map (RewardAcnt (Ledger.Crypto era)) Coin
    ]
  toJSON (DelplFailure f) = toJSON (f :: PredicateFailure (Ledger.EraRule "DELPL" era))

instance ( ToJSON (PredicateFailure (Core.EraRule "POOL" era))
         , ToJSON (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToJSON (ShelleyDelplPredFailure era) where
  toJSON (PoolFailure   f) = toJSON (f :: PredicateFailure (Ledger.EraRule "POOL"  era))
  toJSON (DelegFailure  f) = toJSON (f :: PredicateFailure (Ledger.EraRule "DELEG" era))

instance Ledger.Era era => ToJSON (ShelleyDelegPredFailure era) where
  toJSON (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) = object
    [ "kind"        .= toJSON @String "StakeKeyAlreadyRegisteredDELEG"
    , "credential"  .= toJSON @Text (textShow alreadyRegistered)
    , "error"       .= toJSON @String "Staking credential already registered"
    ]
  toJSON (StakeKeyInRewardsDELEG alreadyRegistered) = object
    [ "kind"        .= toJSON @String "StakeKeyInRewardsDELEG"
    , "credential"  .= toJSON @Text (textShow alreadyRegistered)
    , "error"       .= toJSON @String "Staking credential registered in rewards map"
    ]
  toJSON (StakeKeyNotRegisteredDELEG notRegistered) = object
    [ "kind"        .= toJSON @String "StakeKeyNotRegisteredDELEG"
    , "credential"  .= toJSON @Text (textShow notRegistered)
    , "error"       .= toJSON @String "Staking credential not registered"
    ]
  toJSON (StakeKeyNonZeroAccountBalanceDELEG remBalance) = object
    [ "kind"              .= toJSON @String "StakeKeyNonZeroAccountBalanceDELEG"
    , "remainingBalance"  .= remBalance
    ]
  toJSON (StakeDelegationImpossibleDELEG unregistered) = object
    [ "kind"        .= toJSON @String "StakeDelegationImpossibleDELEG"
    , "credential"  .= toJSON @Text (textShow unregistered)
    , "error"       .= toJSON @String "Cannot delegate this stake credential because it is not registered"
    ]
  toJSON WrongCertificateTypeDELEG = object
    [ "kind" .= toJSON @String "WrongCertificateTypeDELEG"
    ]
  toJSON (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"            .= toJSON @String "GenesisKeyNotInMappingDELEG"
    , "unknownKeyHash"  .= toJSON @Text (textShow genesisKeyHash)
    , "error"           .= toJSON @String "This genesis key is not in the delegation mapping"
    ]
  toJSON (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"              .= toJSON @String "DuplicateGenesisDelegateDELEG"
    , "duplicateKeyHash"  .= toJSON @Text (textShow genesisKeyHash)
    , "error"             .= toJSON @String "This genesis key has already been delegated to"
    ]
  toJSON (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) = object
    [ "kind"          .= toJSON @String "InsufficientForInstantaneousRewardsDELEG"
    , "pot"           .= toJSON @String potText
    , "neededAmount"  .= (neededMirAmount :: Coin)
    , "reserves"      .= (reserves        :: Coin)
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) = object
    [ "kind"                        .= toJSON @String "MIRCertificateTooLateinEpochDELEG"
    , "currentSlotNo"               .= (currSlot    :: SlotNo)
    , "mustBeSubmittedBeforeSlotNo" .= (boundSlotNo :: SlotNo)
    ]
  toJSON (DuplicateGenesisVRFDELEG vrfKeyHash) = object
    [ "kind"    .= toJSON @String "DuplicateGenesisVRFDELEG"
    , "keyHash" .= vrfKeyHash -- :: Crypto.Hash (Crypto.HASH (Ledger.Crypto era)) (VerKeyVRF (Ledger.Crypto era))
    ]
  toJSON MIRTransferNotCurrentlyAllowed = object
    [ "kind" .= toJSON @String "MIRTransferNotCurrentlyAllowed"
    ]
  toJSON MIRNegativesNotCurrentlyAllowed = object
    [ "kind" .= toJSON @String "MIRNegativesNotCurrentlyAllowed"
    ]
  toJSON (InsufficientForTransferDELEG mirpot attempted available) = object
    [ "kind"      .= toJSON @String "DuplicateGenesisVRFDELEG"
    , "pot"       .= toJSON @String potText
    , "attempted" .= (attempted :: Coin)
    , "available" .= (available :: Coin)
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON MIRProducesNegativeUpdate = object
    [ "kind" .= toJSON @String "MIRProducesNegativeUpdate"
    ]
  toJSON (MIRNegativeTransfer pot coin) = object
    [ "kind"    .= toJSON @String "MIRNegativeTransfer"
    , "error"   .= toJSON @String "Attempt to transfer a negative amount from a pot."
    , "pot"     .= toJSON @String potText
    , "amount"  .= (coin :: Coin)
    ]
    where potText = case pot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"

instance ToJSON (ShelleyPoolPredFailure era) where
  toJSON (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) = object
    [ "kind"                .= toJSON @String "StakePoolNotRegisteredOnKeyPOOL"
    , "unregisteredKeyHash" .= toJSON @Text (textShow unregStakePool)
    , "error"               .= toJSON @String "This stake pool key hash is unregistered"
    ]
  toJSON (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) = object
    [ "kind"                    .= toJSON @String "StakePoolRetirementWrongEpochPOOL"
    , "currentEpoch"            .= toJSON @Text (textShow currentEpoch)
    , "intendedRetirementEpoch" .= toJSON @Text (textShow intendedRetireEpoch)
    , "maxEpochForRetirement"   .= toJSON @Text (textShow maxRetireEpoch)
    ]
  toJSON (StakePoolCostTooLowPOOL certCost protCost) = object
    [ "kind"              .= toJSON @String "StakePoolCostTooLowPOOL"
    , "certificateCost"   .= toJSON @Text (textShow certCost)
    , "protocolParCost"   .= toJSON @Text (textShow protCost)
    , "error"             .= toJSON @String "The stake pool cost is too low"
    ]
  toJSON (PoolMedataHashTooBig poolID hashSize) = object
    [ "kind"      .= toJSON @String "PoolMedataHashTooBig"
    , "poolID"    .= toJSON @Text (textShow poolID)
    , "hashSize"  .= toJSON @Text (textShow hashSize)
    , "error"     .= toJSON @String "The stake pool metadata hash is too large"
    ]

-- Apparently this should never happen according to the Shelley exec spec
  toJSON (WrongCertificateTypePOOL index) =
    case index of
      0 -> object
        [ "kind"  .= toJSON @String "WrongCertificateTypePOOL"
        , "error" .= toJSON @String "Wrong certificate type: Delegation certificate"
        ]
      1 -> object
        [ "kind"  .= toJSON @String "WrongCertificateTypePOOL"
        , "error" .= toJSON @String "Wrong certificate type: MIR certificate"
        ]
      2 -> object
        [ "kind"  .= toJSON @String "WrongCertificateTypePOOL"
        , "error" .= toJSON @String "Wrong certificate type: Genesis certificate"
        ]
      k -> object
        [ "kind"            .= toJSON @String "WrongCertificateTypePOOL"
        , "certificateType" .= (k :: Word8)
        , "error"           .= toJSON @String "Wrong certificate type: Unknown certificate type"
        ]

  toJSON (WrongNetworkPOOL networkId listedNetworkId poolId) = object
    [ "kind"            .= toJSON @String "WrongNetworkPOOL"
    , "networkId"       .= toJSON @Text (textShow networkId)
    , "listedNetworkId" .= toJSON @Text (textShow listedNetworkId)
    , "poolId"          .= toJSON @Text (textShow poolId)
    , "error"           .= toJSON @String "Wrong network ID in pool registration certificate"
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToJSON (ShelleyTickPredFailure era) where
  toJSON (NewEpochFailure f) = object
    [ "kind"  .= toJSON @String "NewEpochFailure"
    , "value" .= (f :: PredicateFailure (Ledger.EraRule "NEWEPOCH" era))
    ]
  toJSON (RupdFailure f) = object
    [ "kind"  .= toJSON @String "RupdFailure"
    , "value" .= (f :: PredicateFailure (Ledger.EraRule "RUPD" era))
    ]


instance ( ToJSON (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "MIR" era))
         ) => ToJSON (ShelleyNewEpochPredFailure era) where
  toJSON (EpochFailure f) = object
    [ "kind"    .= toJSON @String "EpochFailure"
    , "update"  .= (f :: PredicateFailure (Ledger.EraRule "EPOCH" era))
    ]
  toJSON (MirFailure f) = object
    [ "kind"    .= toJSON @String "MirFailure"
    , "update"  .= (f :: PredicateFailure (Ledger.EraRule "MIR" era))
    ]
  toJSON (CorruptRewardUpdate update) = object
    [ "kind"    .= toJSON @String "CorruptRewardUpdate"
    , "update"  .= toJSON @String (show update)
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "SNAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToJSON (ShelleyEpochPredFailure era) where
  toJSON (PoolReapFailure f) = object
    [ "kind"    .= toJSON @String "PoolReapFailure"
    , "update"  .= f
    ]
  toJSON (SnapFailure f) = object
    [ "kind"    .= toJSON @String "SnapFailure"
    , "update"  .= f
    ]
  toJSON (UpecFailure _f) = object
    [ "kind"    .= toJSON @String "UpecFailure"
    , "update"  .= Aeson.Null -- TODO
    ]

-- TODO: Need to elaborate more on this error
instance ToJSON (ShelleyNewppPredFailure era) where
  toJSON (UnexpectedDepositPot outstandingDeposits depositPot) = object
    [ "kind"                .= toJSON @String "UnexpectedDepositPot"
    , "outstandingDeposits" .= toJSON @Text (textShow outstandingDeposits)
    , "depositPot"          .= toJSON @Text (textShow depositPot)
    ]

instance
  ( Crypto.Crypto crypto
  , ToJSON (OverlayPredicateFailure crypto)
  ) => ToJSON (PrtclPredicateFailure crypto) where
  toJSON (OverlayFailure f) = object
    [ "kind"    .= toJSON @String "OverlayFailure"
    , "update"  .= (f :: OverlayPredicateFailure crypto)
    ]
  toJSON (UpdnFailure f) = object
    [ "kind"    .= toJSON @String "UpdnFailure"
    , "update"  .= (f :: UpdnPredicateFailure crypto)
    ]

instance
  ( Crypto.Crypto crypto
  , ToJSON (OcertPredicateFailure crypto)
  , ToJSON L.ActiveSlotCoeff
  ) => ToJSON (OverlayPredicateFailure crypto) where
  toJSON (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) = object
    [ "kind"            .= toJSON @String "UnknownGenesisKeyOVERLAY"
    , "unknownKeyHash"  .= toJSON @Text (textShow genKeyHash)
    ]
  toJSON (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) = object
    [ "kind"                .= toJSON @String "VRFKeyBadLeaderValueOVERLAY"
    , "seedNonce"           .= toJSON @Text (textShow seedNonce)
    , "currentSlot"         .= toJSON @Text (textShow currSlotNo)
    , "previousHashAsNonce" .= toJSON @Text (textShow prevHashNonce)
    , "leaderElectionValue" .= toJSON @Text (textShow leaderElecVal)
    ]
  toJSON (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) = object
    [ "kind"                .= toJSON @String "VRFKeyBadNonceOVERLAY"
    , "seedNonce"           .= toJSON @Text (textShow seedNonce)
    , "currentSlot"         .= toJSON @Text (textShow currSlotNo)
    , "previousHashAsNonce" .= toJSON @Text (textShow prevHashNonce)
    , "blockNonce"          .= toJSON @Text (textShow blockNonce)
    ]
  toJSON (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) = object
    [ "kind"                    .= toJSON @String "VRFKeyWrongVRFKeyOVERLAY"
    , "poolHash"                .= textShow issuerHash
    , "registeredVRFKeHash"     .= textShow regVRFKeyHash
    , "unregisteredVRFKeyHash"  .= textShow unregVRFKeyHash
    ]
  toJSON (VRFKeyUnknown (KeyHash kHash)) = object
    [ "kind"    .= toJSON @String "VRFKeyUnknownOVERLAY"
    , "keyHash" .= toJSON @Text (textShow kHash)
    ]
  toJSON (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) = object
    [ "kind"                  .= toJSON @String "VRFLeaderValueTooBigOVERLAY"
    , "leaderElectionValue"   .= toJSON @Text (textShow leadElecVal)
    , "delegationPoolWeight"  .= toJSON @Text (textShow weightOfDelegPool)
    , "activeSlotCoefficient" .= toJSON @Text (textShow actSlotCoefff)
    ]
  toJSON (NotActiveSlotOVERLAY notActiveSlotNo) = object
    [ "kind" .= toJSON @String "NotActiveSlotOVERLAY"
    , "slot" .= toJSON @Text (textShow notActiveSlotNo)
    ]
  toJSON (WrongGenesisColdKeyOVERLAY actual expected) = object
    [ "kind"      .= toJSON @String "WrongGenesisColdKeyOVERLAY"
    , "actual"    .= (actual    :: KeyHash 'BlockIssuer crypto)
    , "expected"  .= (expected  :: KeyHash 'GenesisDelegate crypto)
    ]
  toJSON (WrongGenesisVRFKeyOVERLAY issuer actual expected) = object
    [ "kind"      .= toJSON @String "WrongGenesisVRFKeyOVERLAY"
    , "issuer"    .= (issuer    :: KeyHash 'BlockIssuer crypto)
    , "actual"    .= (actual)
    , "expected"  .= (expected)
    ]
  toJSON (OcertFailure f) = toJSON f

instance
  ( Crypto.Crypto crypto
  ) => ToJSON (OcertPredicateFailure crypto) where
  toJSON (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) = object
    [ "kind"                  .= toJSON @String "KESBeforeStartOCERT"
    , "opCertKESStartPeriod"  .= toJSON @Text (textShow oCertstart)
    , "currentKESPeriod"      .= toJSON @Text (textShow current)
    , "error"                 .= toJSON @String "Your operational certificate's KES start period is before the KES current period."
    ]
  toJSON (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) = object
    [ "kind"                  .= toJSON @String "KESAfterEndOCERT"
    , "currentKESPeriod"      .= toJSON @Text (textShow current)
    , "opCertKESStartPeriod"  .= toJSON @Text (textShow oCertstart)
    , "maxKESEvolutions"      .= toJSON @Text (textShow maxKESEvolutions)
    , "error"                 .= toJSON @String "The operational certificate's KES start period is greater than the max number of KES + the KES current period"
    ]
  toJSON (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) = object
    [ "kind"              .= toJSON @String "CounterTooSmallOCert"
    , "currentKESCounter" .= toJSON @Text (textShow currentKESCounter)
    , "lastKESCounter"    .= toJSON @Text (textShow lastKEScounterUsed)
    , "error"             .= toJSON @String "The operational certificate's last KES counter is greater than the current KES counter."
    ]
  toJSON (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) = object
    [ "kind"                  .= toJSON @String "InvalidSignatureOCERT"
    , "opCertKESStartPeriod"  .= toJSON @Text (textShow oCertKESStartPeriod)
    , "opCertCounter"         .= toJSON @Text (textShow oCertCounter)
    ]
  toJSON (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) = object
    [ "kind"                        .= toJSON @String "InvalidKesSignatureOCERT"
    , "opCertKESStartPeriod"        .= toJSON @Text (textShow startKESPeriod)
    , "opCertKESCurrentPeriod"      .= toJSON @Text (textShow currKESPeriod)
    , "opCertExpectedKESEvolutions" .= toJSON @Text (textShow expectedKESEvolutions)
    , "error"                       .= (err :: String)
    ]
  toJSON (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) = object
    [ "kind" .= toJSON @String "NoCounterForKeyHashOCERT"
    , "stakePoolKeyHash" .= toJSON @Text (textShow stakePoolKeyHash)
    , "error" .= toJSON @String "A counter was not found for this stake pool key hash"
    ]


instance ToJSON (ShelleyUpecPredFailure era) where
  toJSON (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) = object
    [ "kind"              .= toJSON @String "UnexpectedDepositPot"
    , "totalOutstanding"  .= toJSON @Text (textShow totalOutstanding)
    , "depositPot"        .= toJSON @Text (textShow depositPot)
    ]


--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------


instance ( Ledger.Era era
         , ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ToJSON (PredicateFailure (Ledger.EraRule "UTXOS" era))
         , Consensus.ShelleyBasedEra era
         ) => ToJSON (AlonzoUtxoPredFailure era) where
  toJSON (Alonzo.BadInputsUTxO badInputs) = object
    [ "kind"      .= toJSON @String "BadInputsUTxO"
    , "badInputs" .= badInputs -- :: Set (TxIn (Ledger.Crypto (Consensus.AlonzoEra StandardCrypto)))
    , "error"     .= (renderBadInputsUTxOErr badInputs :: Value)
    ]
  toJSON (Alonzo.OutsideValidityIntervalUTxO validityInterval slot) = object
    [ "kind"              .= toJSON @String "ExpiredUTxO"
    , "validityInterval"  .= validityInterval
    , "slot"              .= (slot            :: SlotNo)
    ]
  toJSON (Alonzo.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= toJSON @String "MaxTxSizeUTxO"
    , "size"    .= (txsize    :: Integer)
    , "maxSize" .= (maxtxsize :: Integer)
    ]
  toJSON Alonzo.InputSetEmptyUTxO = object
    [ "kind" .= toJSON @String "InputSetEmptyUTxO"
    ]
  toJSON (Alonzo.FeeTooSmallUTxO minfee currentFee) = object
    [ "kind"    .= toJSON @String "FeeTooSmallUTxO"
    , "minimum" .= (minfee      :: Coin)
    , "fee"     .= (currentFee  :: Coin)
    ]
  toJSON (Alonzo.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= toJSON @String "ValueNotConservedUTxO"
    , "consumed"  .= (consumed                                     :: Ledger.Value era)
    , "produced"  .= (produced                                     :: Ledger.Value era)
    , "error"     .= (renderValueNotConservedErr consumed produced :: Value)
    ]
  toJSON (Alonzo.WrongNetwork network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetwork"
    , "network" .= (network :: Network)
    , "addrs"   .= addrs
    ]
  toJSON (Alonzo.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetworkWithdrawal"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (Alonzo.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooSmallUTxO"
    , "outputs" .= badOutputs
    , "error"   .= toJSON @String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (Alonzo.UtxosFailure predFailure) = object
    [ "kind"    .= toJSON @String "UtxosFailure"
    , "error"   .= toJSON predFailure
    ]
  toJSON (Alonzo.OutputBootAddrAttrsTooBig txouts) = object
    [ "kind"    .= toJSON @String "OutputBootAddrAttrsTooBig"
    , "outputs" .= txouts
    , "error"   .= toJSON @String "The Byron address attributes are too big"
    ]
  toJSON Alonzo.TriesToForgeADA = object
    [ "kind"  .= toJSON @String "TriesToForgeADA"
    ]
  toJSON (Alonzo.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooBigUTxO"
    , "outputs" .= badOutputs
    , "error"   .= toJSON @String "Too many asset ids in the tx output"
    ]
  toJSON (Alonzo.InsufficientCollateral computedBalance suppliedFee) = object
    [ "kind"    .= toJSON @String "InsufficientCollateral"
    , "balance" .= computedBalance
    , "txfee"   .= suppliedFee
    ]
  toJSON (Alonzo.ScriptsNotPaidUTxO utxos) = object
    [ "kind"  .= toJSON @String "ScriptsNotPaidUTxO"
    , "utxos" .= utxos
    ]
  toJSON (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) = object
    [ "kind"        .= toJSON @String "ExUnitsTooBigUTxO"
    , "maxexunits"  .= pParamsMaxExUnits
    , "exunits"     .= suppliedExUnits
    ]
  toJSON (Alonzo.CollateralContainsNonADA inputs) = object
    [ "kind"    .= toJSON @String "CollateralContainsNonADA"
    , "inputs"  .= inputs
    ]
  toJSON (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) = object
    [ "kind"            .= toJSON @String "WrongNetworkInTxBody"
    , "networkid"       .= actualNetworkId
    , "txbodyNetworkId" .= netIdInTxBody
    ]
  toJSON (Alonzo.OutsideForecast slotNum) = object
    [ "kind" .= toJSON @String "OutsideForecast"
    , "slot" .= slotNum
    ]
  toJSON (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) = object
    [ "kind"    .= toJSON @String "TooManyCollateralInputs"
    , "max"     .= maxCollateralInputs
    , "inputs"  .= numberCollateralInputs
    ]
  toJSON Alonzo.NoCollateralInputs = object
    [ "kind"  .= toJSON @String "NoCollateralInputs"
    ]

instance ( -- ToJSON (Alonzo.CollectError (Ledger.Crypto era))
         -- ,
        --  ToJSON (PredicateFailure (Ledger.EraRule "PPUP" era))
         ) =>ToJSON (AlonzoUtxosPredFailure era) where
  toJSON (Alonzo.ValidationTagMismatch isValidating reason) = object
    [ "kind"          .= toJSON @String "ValidationTagMismatch"
    , "isvalidating"  .= isValidating
    , "reason"        .= (reason :: Alonzo.TagMismatchDescription)
    ]
  toJSON (Alonzo.CollectErrors _errors) = object
    [ "kind"    .= toJSON @String "CollectErrors"
    , "errors"  .= toJSON @String "TODO" -- errors
    ]
  toJSON (Alonzo.UpdateFailure _pFailure) = toJSON @String "TODO" -- toJSON pFailure

deriving newtype instance ToJSON Alonzo.IsValid

-- instance ToJSON (Alonzo.CollectError (Ledger.Crypto era)) where
--   toJSON = \case
--     Alonzo.NoRedeemer sPurpose -> object
--       [ "kind"          .= String "CollectError"
--       , "error"         .= String "NoRedeemer"
--       , "scriptpurpose" .= (sPurpose :: Alonzo.ScriptPurpose (Ledger.Crypto era))
--       ]
--     Alonzo.NoWitness sHash -> object
--       [ "kind" .= String "CollectError"
--       , "error" .= String "NoWitness"
--       , "scripthash" .= toJSON sHash
--       ]
--     Alonzo.NoCostModel lang -> object
--       [ "kind" .= String "CollectError"
--       , "error" .= String "NoCostModel"
--       , "language" .= toJSON lang
--       ]
--     Alonzo.BadTranslation err -> object
--       [ "kind" .= String "PlutusTranslationError"
--       , "error" .= case err of
--           Alonzo.ByronTxOutInContext txOutSource ->
--             object
--               [ "kind" .= String "ByronTxOutInContext"
--               , "txOutSource" .= txOutSource
--               ]
--           Alonzo.TranslationLogicMissingInput txin ->
--             object
--               [ "kind" .= String "TranslationLogicMissingInput"
--               , "txin" .= txin
--               ]
--           Alonzo.RdmrPtrPointsToNothing ptr ->
--             object
--               [ "kind" .= String "RedeemerPointerPointsToNothing"
--               , "ptr" .= Api.renderScriptWitnessIndex (Api.fromAlonzoRdmrPtr ptr)
--               ]
--           Alonzo.LanguageNotSupported lang ->
--             object
--               [ "kind" .= String "LanguageNotSupported"
--               , "lang" .= toJSON lang
--               ]
--           Alonzo.InlineDatumsNotSupported txOutSource ->
--             object
--               [ "kind" .= String "InlineDatumsNotSupported"
--               , "txOutSource" .= toJSON txOutSource
--               ]
--           Alonzo.ReferenceScriptsNotSupported txOutSource ->
--             object
--               [ "kind" .= String "ReferenceScriptsNotSupported"
--               , "txOutSource" .= toJSON txOutSource
--               ]
--           Alonzo.ReferenceInputsNotSupported txins ->
--             object
--               [ "kind" .= String "ReferenceInputsNotSupported"
--               , "txins" .= toJSON txins
--               ]
--           Alonzo.TimeTranslationPastHorizon msg ->
--             object
--               [ "kind" .= String "TimeTranslationPastHorizon"
--               , "msg" .= toJSON msg
--               ]
--       ]

instance ToJSON (Alonzo.TxOutSource StandardCrypto) where
  toJSON = \case
    Alonzo.TxOutFromInput txin ->
      object
        [ "kind" .= toJSON @String "TxOutFromInput"
        , "msg" .= toJSON txin
        ]
    Alonzo.TxOutFromOutput txix ->
      object
        [ "kind" .= toJSON @String "TxOutFromOutput"
        , "msg" .= toJSON txix
        ]

instance ToJSON Alonzo.FailureDescription where
  toJSON (Alonzo.PlutusFailure _t _bs) = object
    [ "kind"                  .= toJSON @String "FailureDescription"
    , "error"                 .= toJSON @String "PlutusFailure"
    , "reconstructionDetail"  .= toJSON @String "TODO" -- do Alonzo.debugPlutus (BSU.toString bs) :: Alonzo.PlutusDebugInfo
    -- , "description"           .= t
    ]

-- instance
--   ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
--   ) => ToJSON (AlonzoBbodyPredFail (Alonzo.AlonzoEra StandardCrypto)) where
--   toJSON err = object
--     [ "kind"  .= String "AlonzoBbodyPredFail"
--     , "error" .= String (show err)
--     ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- instance ToJSON Alonzo.PlutusDebugInfo where
--   toJSON = \case
--     Alonzo.DebugSuccess budget -> object
--       [ "kind"    .= String "DebugSuccess"
--       , "budget"  .= (budget :: Cek.ExBudget)
--       ]
--     Alonzo.DebugCannotDecode msg -> object
--       [ "kind"    .= String "DebugCannotDecode"
--       , "message" .= (msg :: String)
--       ]
--     Alonzo.DebugInfo texts e d -> object
--       [ "kind"  .= String "DebugInfo"
--       , "texts" .= (texts :: [Text])
--       , "error" .= (e     :: Alonzo.PlutusError)
--       , "debug" .= (d     :: Alonzo.PlutusDebug)
--       ]
--     Alonzo.DebugBadHex msg -> object
--       [ "kind"    .= String "DebugBadHex"
--       , "message" .= (msg :: String)
--       ]

-- instance ToJSON Alonzo.PlutusError where
--   toJSON = \case
--     Alonzo.PlutusErrorV1 evaluationError -> toJSON (evaluationError :: PV1.EvaluationError)
--     Alonzo.PlutusErrorV2 evaluationError -> toJSON (evaluationError :: PV1.EvaluationError)

-- instance ToJSON Alonzo.PlutusDebug where
--   toJSON = \case
--     Alonzo.PlutusDebugV1 _costModel exUnits sbs ds protVer -> object
--       [ "exUnits"     .= (exUnits                                               :: Ledger.ExUnits)
--       , "sbs"         .= (Text.decodeLatin1 (B16.encode (Short.fromShort sbs))  :: Text)
--       , "scriptHash"  .= (scriptHashOf Alonzo.PlutusV1 sbs                      :: Text)
--       , "dsSummary"   .= (plutusDataToDsSummary ds                              :: Aeson.Value)
--       , "protVer"     .= (protVer                                               :: ProtVer)
--       -- , "ds"          .= toJSON ds
--       -- , "costModel"   .= costModel
--       ]
--     Alonzo.PlutusDebugV2 _costModel exUnits sbs ds protVer -> object
--       [ "exUnits"     .= (exUnits                                               :: Ledger.ExUnits)
--       , "sbs"         .= (Text.decodeLatin1 (B16.encode (Short.fromShort sbs))  :: Text)
--       , "scriptHash"  .= (scriptHashOf Alonzo.PlutusV2 sbs                      :: Text)
--       , "dsSummary"   .= (plutusDataToDsSummary ds                              :: Aeson.Value)
--       , "protVer"     .= (protVer                                               :: ProtVer)
--       -- , "ds"          .= toJSON ds
--       -- , "costModel"   .= costModel
--       ]

-- plutusDataToDsSummary :: [Plutus.Data] -> Aeson.Value
-- plutusDataToDsSummary [dat, redeemer, info] = Aeson.object
--   [ "data"      .= (dat                             :: Plutus.Data)
--   , "redeemer"  .= (redeemer                        :: Plutus.Data)
--   , "info"      .= (plutusInfoDataToDsSummary info  :: Value)
--   ]
-- plutusDataToDsSummary [dat, info] = Aeson.object
--   [ "data"      .= (dat                             :: Plutus.Data)
--   , "info"      .= (plutusInfoDataToDsSummary info  :: Value)
--   ]
-- plutusDataToDsSummary _ = Aeson.Null

-- plutusInfoDataToDsSummary :: Plutus.Data -> Aeson.Value
-- plutusInfoDataToDsSummary info = case PV1.fromData info of
--   Nothing -> String "no-info"
--   Just PV1.ScriptContext { PV1.scriptContextTxInfo, PV1.scriptContextPurpose} -> object
--     [ "txInfo"  .= (txInfoToJson scriptContextTxInfo          :: Value)
--     , "purpose" .= (scriptPurposeToJson scriptContextPurpose  :: Value)
--     ]

-- txInfoToJson :: PV1.TxInfo -> Value
-- txInfoToJson txInfo = Aeson.object
--   [ "inputs"      .= (PV1.txInfoInputs      txInfo :: [PV1.TxInInfo])
--   , "outputs"     .= (PV1.txInfoOutputs     txInfo :: [PV1.TxOut])
--   , "fee"         .= (PV1.txInfoFee         txInfo :: PV1.Value)
--   , "mint"        .= (PV1.txInfoMint        txInfo :: PV1.Value)
--   , "dCert"       .= (PV1.txInfoDCert       txInfo :: [PV1.DCert])
--   , "wdrl"        .= (PV1.txInfoWdrl        txInfo :: [(PV1.StakingCredential, Integer)])
--   , "validRange"  .= (PV1.txInfoValidRange  txInfo :: (PV1.Interval PV1.POSIXTime))
--   , "signatories" .= (PV1.txInfoSignatories txInfo :: [PV1.PubKeyHash])
--   , "data"        .= (PV1.txInfoData        txInfo :: [(PV1.DatumHash, PV1.Datum)])
--   , "id"          .= (PV1.txInfoId          txInfo :: PV1.TxId)
--   ]

-- instance ToJSON PV1.Datum where
--   toJSON v = toJSON (PV1.builtinDataToData (PV1.getDatum v) :: Plutus.Data)

-- instance ToJSON PV1.DatumHash where
--   toJSON v = toJSON (show v :: Text)

-- instance ToJSON PV1.DCert where
--   toJSON = \case
--     PV1.DCertDelegRegKey stakingCredential -> object
--       [ "DCertDelegRegKey" .= (stakingCredential :: PV1.StakingCredential)
--       ]
--     PV1.DCertDelegDeRegKey stakingCredential -> object
--       [ "DCertDelegDeRegKey" .= (stakingCredential :: PV1.StakingCredential)
--       ]
--     PV1.DCertDelegDelegate delegator delagatee -> object
--       [ "DCertDelegDelegate" .= object
--         [ "delegator" .= (delegator :: PV1.StakingCredential)
--         , "delegatee" .= (delagatee :: PV1.PubKeyHash)
--         ]
--       ]
--     PV1.DCertPoolRegister poolId poolVfr -> object
--       [ "DCertPoolRegister" .= object
--         [ "poolId"  .= (poolId  :: PV1.PubKeyHash)
--         , "poolVfr" .= (poolVfr :: PV1.PubKeyHash)
--         ]
--       ]
--     PV1.DCertPoolRetire pkh n -> object
--       [ "DCertPoolRetire" .= object
--         [ "stakePoolId"   .= (pkh :: PV1.PubKeyHash)
--         , "epochRetiring" .= (n   :: Integer)
--         ]
--       ]
--     PV1.DCertGenesis -> String "DCertGenesis"
--     PV1.DCertMir -> String "DCertMir"

-- instance ToJSON (PV1.Interval PV1.POSIXTime) where
--   toJSON (PV1.Interval lo hi) = toJSON $
--     lowerBoundToJsonArray lo <>
--     upperBoundToJsonArray hi
--     where
--       lowerBoundToJsonArray :: PV1.LowerBound PV1.POSIXTime -> [Value]
--       lowerBoundToJsonArray = \case
--         PV1.LowerBound PV1.PosInf     _     -> ["(", "+∞"                 ]
--         PV1.LowerBound PV1.NegInf     _     -> ["(", "-∞"                 ]
--         PV1.LowerBound (PV1.Finite a) True  -> ["[", toJSON (PV1.toData a)]
--         PV1.LowerBound (PV1.Finite a) False -> ["(", toJSON (PV1.toData a)]

--       upperBoundToJsonArray :: PV1.UpperBound PV1.POSIXTime -> [Value]
--       upperBoundToJsonArray = \case
--         PV1.UpperBound PV1.PosInf     _     -> ["+∞"                 , ")"]
--         PV1.UpperBound PV1.NegInf     _     -> ["-∞"                 , ")"]
--         PV1.UpperBound (PV1.Finite a) True  -> [toJSON (PV1.toData a), "]"]
--         PV1.UpperBound (PV1.Finite a) False -> [toJSON (PV1.toData a), ")"]

-- instance ToJSON PV1.PubKeyHash where
--   toJSON v = toJSON (show v :: Text)

-- instance ToJSON PV1.StakingCredential where
--   toJSON = \case
--     PV1.StakingHash credential -> object
--       [ "StakingHash" .= (credential :: PV1.Credential)
--       ]
--     PV1.StakingPtr a b c -> toJSON ([a, b, c] :: [Integer])

-- instance ToJSON PV1.Credential where
--   toJSON = \case
--     PV1.PubKeyCredential pubKeyHash -> object
--       [ "PubKeyCredential" .= (pubKeyHash :: PV1.PubKeyHash)
--       ]
--     PV1.ScriptCredential validatorHash -> object
--       [ "ScriptCredential" .= (validatorHash :: PV1.ValidatorHash)
--       ]

-- instance ToJSON PV1.ValidatorHash where
--   toJSON h = toJSON (show h :: Text)

-- instance ToJSON PV1.TxId where
--   toJSON v = toJSON (show v :: Text)

-- instance ToJSON PV1.TxInInfo where
--   toJSON v = object
--     [ "outRef"    .= toJSON (PV1.txInInfoOutRef   v :: PV1.TxOutRef)
--     , "resolved"  .= toJSON (PV1.txInInfoResolved v :: PV1.TxOut)
--     ]

-- instance ToJSON PV1.TxOut where
--   toJSON v = object
--     [ "address"   .= (PV1.txOutAddress   v :: PV1.Address)
--     , "value"     .= (PV1.txOutValue     v :: PV1.Value)
--     , "datumHash" .= (PV1.txOutDatumHash v :: Maybe PV1.DatumHash)
--     ]

-- instance ToJSON PV1.Address where
--   toJSON v = object
--     [ "credential"        .= (PV1.addressCredential        v :: PV1.Credential)
--     , "stakingCredential" .= (PV1.addressStakingCredential v :: Maybe PV1.StakingCredential)
--     ]

-- instance ToJSON PV1.Value where
--   toJSON (PV1.Value m) = toJSON (m :: AssocMap.Map PV1.CurrencySymbol (AssocMap.Map PV1.TokenName Integer))

-- instance ToJSON PV1.TokenName where
--   toJSON v = toJSON (show v :: Text)

-- instance ToJSONKey PV1.TokenName where
--   toJSONKey = contramap (builtinByteStringToBase16Text . PV1.unTokenName) toJSONKey -- toJSONKeyText $ show @PV1.TokenName @Text

-- builtinByteStringToBase16Text :: PV1.BuiltinByteString -> Text
-- builtinByteStringToBase16Text bs = Text.filter (/= '"') (Text.pack (show bs)) -- TODO is there a better way to encode as Text

-- instance ToJSON PV1.CurrencySymbol where
--   toJSON (PV1.CurrencySymbol bs) = toJSON (show bs :: Text)

-- instance ToJSONKey PV1.CurrencySymbol where
--   toJSONKey = toJSONKeyText $ show @PV1.CurrencySymbol @Text

-- instance (ToJSONKey k, Ord k, ToJSON a) => ToJSON (AssocMap.Map k a) where
--   toJSON = toJSON . Map.fromList . AssocMap.toList

-- instance ToJSON PV1.TxOutRef where
--   toJSON (PV1.TxOutRef txid idx) = toJSON
--     [ toJSON (txid  :: PV1.TxId)
--     , toJSON (idx   :: Integer)
--     ]

-- scriptPurposeToJson :: PV1.ScriptPurpose -> Value
-- scriptPurposeToJson = \case
--   PV1.Minting currencySymbol -> Aeson.object
--     [ "kind"  .= String "Minting"
--     , "value" .= (currencySymbol :: PV1.CurrencySymbol)
--     ]
--   PV1.Spending outRef -> Aeson.object
--     [ "kind"  .= String "Spending"
--     , "value" .= toJSON outRef
--     ]
--   PV1.Rewarding stakingCredential -> Aeson.object
--     [ "kind"  .= String "Rewarding"
--     , "value" .= (stakingCredential :: PV1.StakingCredential)
--     ]
--   PV1.Certifying dCert -> Aeson.object
--     [ "kind"  .= String "Certifying"
--     , "value" .= (dCert :: PV1.DCert)
--     ]

-- scriptHashOf :: Alonzo.Language -> Short.ShortByteString -> Text
-- scriptHashOf lang sbs = Text.pack $ Hash.hashToStringAsHex h
--   where Ledger.ScriptHash h = case lang of
--           Alonzo.PlutusV1 -> Ledger.hashScript @Consensus.StandardAlonzo (Ledger.PlutusScript lang sbs)
--           Alonzo.PlutusV2 -> error "not implemented"

-- instance ToJSON Plutus.EvaluationError where
--   toJSON = \case
--     Plutus.CekError e -> object
--       [ "kind"    .= String "CekError"
--       , "error"   .= (show e  :: Text)
--       , "value"   .= (e       :: (Cek.ErrorWithCause
--                                       (Cek.EvaluationError Cek.CekUserError (PlutusCore.MachineError PlutusCore.DefaultFun))
--                                       (UntypedPlutusCore.Core.Type.Term
--                                         PlutusCore.DeBruijn.NamedDeBruijn
--                                         PlutusCore.DefaultUni
--                                         PlutusCore.DefaultFun
--                                         ())))
--       ]
--     Plutus.DeBruijnError e -> object
--       [ "kind"    .= String "DeBruijnError"
--       , "error"   .= (show e  :: Text)
--       ]
--     Plutus.CodecError e -> object
--       [ "kind"    .= String "CodecError"
--       , "error"   .= (show e  :: Text)
--       ]
--     Plutus.IncompatibleVersionError actual -> object
--       [ "kind"    .= String "IncompatibleVersionError"
--       , "actual"  .= (actual  :: UntypedPlutusCore.Core.Type.Version ())
--       ]
--     Plutus.CostModelParameterMismatch -> object
--       [ "kind"    .= String "CostModelParameterMismatch"
--       ]

-- instance ToJSON (Plutus.Version ann) where
--   toJSON (Plutus.Version _ i j k) = object
--     [ "i" .= (i :: Natural)
--     , "j" .= (j :: Natural)
--     , "k" .= (k :: Natural)
--     ]

-- instance ToJSON Plutus.Data where
--   toJSON = \case
--     Plutus.Constr t as -> object
--       [ "Constr" .= (toJSON (t :: Integer):fmap toJSON (as :: [Plutus.Data]) :: [Value])
--       ]
--     Plutus.Map es -> object
--       [ "Map" .= (fmap dataEntryToJson es :: [Value])
--       ]
--     Plutus.List es  -> toJSON (es :: [Plutus.Data])
--     Plutus.I n      -> toJSON (n :: Integer)
--     Plutus.B bs     -> toJSON (Text.decodeLatin1 (B16.encode bs) :: Text)

-- dataEntryToJson :: (Plutus.Data, Plutus.Data) -> Value
-- dataEntryToJson (k, v) = toJSON [toJSON k, toJSON v]

-- instance ToJSON Cek.CekUserError where
--   toJSON = \case
--     Cek.CekOutOfExError (Cek.ExRestrictingBudget res) -> object
--       [ "kind"    .= String "CekOutOfExError"
--       , "budget"  .= (res :: Cek.ExBudget)
--       ]
--     Cek.CekEvaluationFailure -> object
--       [ "kind"  .= String "CekEvaluationFailure"
--       ]

-- instance (ToJSON name, ToJSON fun) => ToJSON (Cek.CekEvaluationException name uni fun) where

-- instance (ToJSON name, ToJSON fun) => ToJSON (UntypedPlutusCore.Core.Type.Term name uni fun ann) where
--   toJSON = \case
--     UntypedPlutusCore.Core.Type.Var {} -> Aeson.object
--       [ "kind" .= String "Var"
--       ]
--     UntypedPlutusCore.Core.Type.LamAbs {} -> Aeson.object
--       [ "kind" .= String "LamAbs"
--       ]
--     UntypedPlutusCore.Core.Type.Apply {} -> Aeson.object
--       [ "kind" .= String "Apply"
--       ]
--     UntypedPlutusCore.Core.Type.Force {} -> Aeson.object
--       [ "kind" .= String "Force"
--       ]
--     UntypedPlutusCore.Core.Type.Delay {} -> Aeson.object
--       [ "kind" .= String "Delay"
--       ]
--     UntypedPlutusCore.Core.Type.Constant {} -> Aeson.object
--       [ "kind" .= String "Constant"
--       ]
--     UntypedPlutusCore.Core.Type.Builtin {} -> Aeson.object
--       [ "kind" .= String "Builtin"
--       ]
--     UntypedPlutusCore.Core.Type.Error {} -> Aeson.object
--       [ "kind" .= String "Error"
--       ]

-- -- Used by ToJSON (Cek.CekEvaluationException name uni fun)
-- instance ToJSON fun => ToJSON (Cek.EvaluationError Cek.CekUserError (PlutusCore.MachineError fun)) where
--   toJSON = \case
--     PlutusCore.InternalEvaluationError internal -> object
--       [ "InternalEvaluationError" .= (internal  :: PlutusCore.MachineError fun)
--       ]
--     PlutusCore.UserEvaluationError user -> object
--       [ "UserEvaluationError"     .= (user      :: Cek.CekUserError)
--       ]

-- instance ToJSON PlutusCore.NamedDeBruijn where

-- instance ToJSON PlutusCore.DeBruijn.Index where

-- instance ToJSON PlutusCore.DefaultFun where

-- instance (forall a. ToJSON (f a)) => ToJSON (PlutusCore.Some f) where
--   toJSON (PlutusCore.Some a) = object
--     [ "kind"  .= String "Some"
--     , "value" .= (a)
--     ]

-- instance (ToJSON (uni (PlutusCore.Esc a)), ToJSON a) => ToJSON (PlutusCore.ValueOf uni a) where
--   toJSON (PlutusCore.ValueOf u a) = object
--     [ "kind"  .= String "ValueOf"
--     , "uni"   .= (u :: uni (PlutusCore.Esc a))
--     , "a"     .= (a :: a)
--     ]

-- instance ToJSON fun => ToJSON (PlutusCore.MachineError fun) where
--   toJSON = \case
--     PlutusCore.NonPolymorphicInstantiationMachineError -> "NonPolymorphicInstantiationMachineError"
--     PlutusCore.NonWrapUnwrappedMachineError -> "NonWrapUnwrappedMachineError"
--     PlutusCore.NonFunctionalApplicationMachineError -> "NonFunctionalApplicationMachineError"
--     PlutusCore.OpenTermEvaluatedMachineError -> "OpenTermEvaluatedMachineError"
--     PlutusCore.UnliftingMachineError (PlutusCore.UnliftingErrorE t) -> object
--       [ "UnliftingMachineError" .= object
--         [ "UnliftingError" .= (t :: Text)
--         ]
--       ]
--     PlutusCore.BuiltinTermArgumentExpectedMachineError -> "BuiltinTermArgumentExpectedMachineError"
--     PlutusCore.UnexpectedBuiltinTermArgumentMachineError -> "UnexpectedBuiltinTermArgumentMachineError"
--     PlutusCore.EmptyBuiltinArityMachineError -> "EmptyBuiltinArityMachineError"
--     PlutusCore.UnknownBuiltin fun -> object
--       [ "UnknownBuiltin" .= (fun :: fun)
--       ]

_textShow :: Show a => a -> Text
_textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =
  case withOriginToMaybe wOblk of
    Nothing -> "Genesis Block"
    Just blk -> textShow . unBlockNo $ labBlockNo blk

-- The following instances aren't used above

instance ToJSON Alonzo.TagMismatchDescription where
  toJSON = \case
    Alonzo.PassedUnexpectedly -> object
      [ "kind"  .= toJSON @String "TagMismatchDescription"
      , "error" .= toJSON @String "PassedUnexpectedly"
      ]
    Alonzo.FailedUnexpectedly forReasons -> object
      [ "kind"            .= toJSON @String "TagMismatchDescription"
      , "error"           .= toJSON @String "FailedUnexpectedly"
      , "reconstruction"  .= (NEL.toList forReasons :: [Alonzo.FailureDescription])
      ]

renderBadInputsUTxOErr ::  Set (Ledger.TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = toJSON @String "The transaction contains no inputs."
  | otherwise = toJSON @String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = toJSON @String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance ToJSON TicknPredicateFailure where
  toJSON x = case x of {} -- no constructors

-- instance ToJSON (ShelleyPoolreapPredFailure era) where
--   toJSON x = case x of {} -- no constructors

-- instance ToJSON (ShelleySnapPredFailure era) where
--   toJSON x = case x of {} -- no constructors

-- instance ToJSON (ShelleyMirPredFailure era) where
--   toJSON x = case x of {} -- no constructors

-- instance ToJSON (ShelleyRupdPredFailure era) where
--   toJSON x = case x of {} -- no constructors

instance ToJSON (UpdnPredicateFailure crypto) where
  toJSON x = case x of {} -- no constructors

instance ( ShelleyBasedEra era
         , ToJSON Allegra.ValidityInterval
         , ToJSON (PPUPPredFailure era)
         ) => ToJSON (AllegraUtxoPredFailure era) where
  toJSON (Allegra.BadInputsUTxO badInputs) =
    object
      [ "kind" .= String "BadInputsUTxO"
      , "badInputs" .= badInputs
      , "error" .= renderBadInputsUTxOErr badInputs
      ]
  toJSON (Allegra.OutsideValidityIntervalUTxO validityInterval slot) =
    object [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  toJSON (Allegra.MaxTxSizeUTxO txsize maxtxsize) =
    object [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  toJSON Allegra.InputSetEmptyUTxO =
    object [ "kind" .= String "InputSetEmptyUTxO" ]
  toJSON (Allegra.FeeTooSmallUTxO minfee txfee) =
    object [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toJSON (Allegra.ValueNotConservedUTxO consumed produced) =
    object [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toJSON (Allegra.WrongNetwork network addrs) =
    object [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toJSON (Allegra.WrongNetworkWithdrawal network addrs) =
    object [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (Allegra.OutputTooSmallUTxO badOutputs) =
    object [ "kind" .= String "OutputTooSmallUTxO"
            , "outputs" .= badOutputs
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
            ]
  toJSON (Allegra.UpdateFailure f) = toJSON f
  toJSON (Allegra.OutputBootAddrAttrsTooBig badOutputs) =
    object [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  toJSON Allegra.TriesToForgeADA =
    object [ "kind" .= String "TriesToForgeADA" ]
  toJSON (Allegra.OutputTooBigUTxO badOutputs) =
    object [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]
