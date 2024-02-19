{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Shelley () where

import           Cardano.Api (textShow)
import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import           Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import qualified Cardano.Ledger.Alonzo.Plutus.Evaluate as Alonzo
import           Cardano.Ledger.Alonzo.Rules (AlonzoBbodyPredFailure (..), AlonzoUtxoPredFailure,
                   AlonzoUtxosPredFailure, AlonzoUtxowPredFailure (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import           Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import           Cardano.Ledger.BaseTypes (activeSlotLog, strictMaybeToMaybe)
import           Cardano.Ledger.Chain
import           Cardano.Ledger.Conway.Governance (govActionIdToText)
import           Cardano.Ledger.Conway.Rules ()
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Ledger.Shelley.API
import           Cardano.Ledger.Shelley.Rules
import           Cardano.Node.Tracing.Render (renderMissingRedeemers, renderScriptHash,
                   renderScriptIntegrityHash)
import           Cardano.Node.Tracing.Tracers.KESInfo ()
import           Cardano.Protocol.TPraos.API (ChainTransitionError (ChainTransitionError))
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock, labBlockNo)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import           Cardano.Protocol.TPraos.Rules.OCert
import           Cardano.Protocol.TPraos.Rules.Overlay
import           Cardano.Protocol.TPraos.Rules.Prtcl
import           Cardano.Protocol.TPraos.Rules.Tickn
import           Cardano.Protocol.TPraos.Rules.Updn
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.Render (renderTxId)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosChainSelectView (..))
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import qualified Ouroboros.Consensus.Shelley.Protocol.Praos as Praos
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)

import           Data.Aeson (Value (..), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

{- HLINT ignore "Use :" -}

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted in roughly topological order.

instance
  ( Consensus.ShelleyBasedEra ledgerera
  ) => ToObject (GenTx (ShelleyBlock protocol ledgerera)) where
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

instance
  ( ToObject (PredicateFailure (Core.EraRule "LEDGER" ledgerera))
  ) => ToObject (ApplyTxError ledgerera) where
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

instance
  ( ToObject (PredicateFailure (Ledger.EraRule "BBODY" ledgerera))
  ) => ToObject (ShelleyLedgerError ledgerera) where
  toObject verb (BBodyError (BlockTransitionError fs)) =
    mconcat [ "kind" .= String "BBodyError"
            , "failures" .= map (toObject verb) fs
            ]

instance
  ( Ledger.Era ledgerera
  , ToJSON (Ledger.PParamsUpdate ledgerera)
  ) => ToObject (ShelleyLedgerUpdate ledgerera) where
  toObject verb (ShelleyUpdatedProtocolUpdates updates) =
    mconcat [ "kind" .= String "ShelleyUpdatedProtocolUpdates"
             , "updates" .= map (toObject verb) updates
             ]
instance
  ( ToObject (PredicateFailure (Ledger.EraRule "DELEG" era))
  , ToObject (PredicateFailure (Ledger.EraRule "POOL" era))
  , ToObject (PredicateFailure (Ledger.EraRule "GOVCERT" era))
  ) => ToObject (Conway.ConwayCertPredFailure era) where
  toObject verb = mconcat . \case
    Conway.DelegFailure f ->
      [ "kind" .= String "DelegFailure " , "failure" .= toObject verb f ]
    Conway.PoolFailure f ->
      [ "kind" .= String "PoolFailure" , "failure" .= toObject verb f ]
    Conway.GovCertFailure f ->
      [ "kind" .= String "GovCertFailure" , "failure" .= toObject verb f ]

instance ToObject (Conway.ConwayGovCertPredFailure era) where
  toObject _verb = mconcat . \case
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
    Conway.ConwayDRepIncorrectDeposit givenCoin expectedCoin  ->
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

instance ToObject (Conway.ConwayDelegPredFailure era) where
  toObject _verb = mconcat . \case
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
      [ "kind" .= String "StakeKeyHasNonZeroAccountBalanceDELEG"
      , "amount" .= coin
      , "error" .= String "Stake key has non-zero account balance"
      ]
    Conway.DRepAlreadyRegisteredForStakeKeyDELEG credential ->
      [ "kind" .= String "DRepAlreadyRegisteredForStakeKeyDELEG"
      , "amount" .= String (textShow credential)
      , "error" .= String "DRep already registered for the stake key"
      ]

instance ToObject (Set (Credential 'Staking StandardCrypto)) where
  toObject _verb creds =
    mconcat [ "kind" .= String "StakeCreds"
             , "stakeCreds" .= map toJSON (Set.toList creds)
             ]

instance
  ( Ledger.Era ledgerera
  , ToJSON (Ledger.PParamsUpdate ledgerera)
  ) => ToObject (ProtocolUpdate ledgerera) where
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
        explanation = mconcat
          [ "A scheduled major protocol version change (hard fork) "
          , "has taken place on the chain, but this node does not "
          , "understand the new major protocol version. This node "
          , "must be upgraded before it can continue with the new "
          , "protocol version."
          ]

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

instance
  ( ToObject (PredicateFailure (Ledger.EraRule "LEDGERS" ledgerera))
  ) => ToObject (ShelleyBbodyPredFailure ledgerera) where
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


instance
  ( ToObject (PredicateFailure (ShelleyUTXO ledgerera))
  , ToObject (PredicateFailure (ShelleyUTXOW ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "LEDGER" ledgerera))
  ) => ToObject (ShelleyLedgersPredFailure ledgerera) where
  toObject verb (LedgerFailure f) = toObject verb f


instance
  ( ToObject (PredicateFailure (ShelleyUTXO ledgerera))
  , ToObject (PredicateFailure (ShelleyUTXOW ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "DELEGS" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "UTXOW" ledgerera))
  ) => ToObject (ShelleyLedgerPredFailure ledgerera) where
  toObject verb (UtxowFailure f) = toObject verb f
  toObject verb (DelegsFailure f) = toObject verb f

instance
  ( ToObject (PredicateFailure (Core.EraRule "CERTS" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "UTXOW" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "GOV" ledgerera))
  , ToObject (Set (Credential 'Staking (Consensus.EraCrypto ledgerera)))
  ) => ToObject (Conway.ConwayLedgerPredFailure ledgerera) where
  toObject verb (Conway.ConwayUtxowFailure f) = toObject verb f
  toObject verb (Conway.ConwayCertsFailure f) = toObject verb f
  toObject verb (Conway.ConwayGovFailure f) = toObject verb f
  toObject verb (Conway.ConwayWdrlNotDelegatedToDRep f) = toObject verb f
  toObject _    (Conway.ConwayTreasuryValueMismatch actual inTx) =
    mconcat [ "kind" .= String "ConwayTreasuryValueMismatch"
            , "actual" .= actual
            , "submittedInTx" .= inTx
            ]


instance Ledger.EraPParams era => ToObject (Conway.ConwayGovPredFailure era) where
  toObject _ (Conway.GovActionsDoNotExist govActionIds) =
    mconcat [ "kind" .= String "GovActionsDoNotExist"
            , "govActionIds" .= map govActionIdToText (NonEmpty.toList govActionIds)
            ]
  toObject _ (Conway.MalformedProposal govAction) =
    mconcat [ "kind" .= String "MalformedProposal"
            , "govAction" .= govAction
            ]
  toObject _ (Conway.ProposalProcedureNetworkIdMismatch rewardAcnt network) =
    mconcat [ "kind" .= String "ProposalProcedureNetworkIdMismatch"
            , "rewardAccount" .= toJSON rewardAcnt
            , "expectedNetworkId" .= toJSON network
            ]
  toObject _ (Conway.TreasuryWithdrawalsNetworkIdMismatch rewardAcnts network) =
    mconcat [ "kind" .= String "TreasuryWithdrawalsNetworkIdMismatch"
            , "rewardAccounts" .= toJSON rewardAcnts
            , "expectedNetworkId" .= toJSON network
            ]
  toObject _ (Conway.ProposalDepositIncorrect deposit expectedDeposit) =
    mconcat [ "kind" .= String "ProposalDepositIncorrect"
            , "deposit" .= deposit
            , "expectedDeposit" .= expectedDeposit
            ]
  toObject _ (Conway.DisallowedVoters govActionIdToVoter) =
    mconcat [ "kind" .= String "DisallowedVoters"
            , "govActionIdToVoter" .= NonEmpty.toList govActionIdToVoter
            ]
  toObject _ (Conway.ConflictingCommitteeUpdate creds) =
    mconcat [ "kind" .= String "ConflictingCommitteeUpdate"
            , "credentials" .= creds
            ]
  toObject _ (Conway.ExpirationEpochTooSmall credsToEpoch) =
    mconcat [ "kind" .= String "ExpirationEpochTooSmall"
            , "credentialsToEpoch" .= credsToEpoch
            ]
  toObject _ (Conway.InvalidPrevGovActionId proposalProcedure) =
    mconcat [ "kind" .= String "InvalidPrevGovActionId"
            , "proposalProcedure" .= proposalProcedure
            ]
  toObject _ (Conway.VotingOnExpiredGovAction actions) =
    mconcat [ "kind" .= String "VotingOnExpiredGovAction"
            , "action" .= actions
            ]
  toObject _ (Conway.ProposalCantFollow prevGovActionId protVer prevProtVer) =
    mconcat [ "kind" .= String "ProposalCantFollow"
            , "prevGovActionId" .= prevGovActionId
            , "protVer" .= protVer
            , "prevProtVer" .= prevProtVer
            ]
  toObject _ (Conway.InvalidPolicyHash actualPolicyHash expectedPolicyHash) =
    mconcat [ "kind" .= String "InvalidPolicyHash"
            , "actualPolicyHash" .= actualPolicyHash
            , "expectedPolicyHash" .= expectedPolicyHash
            ]

instance
  ( Core.Crypto (Consensus.EraCrypto era)
  , ToObject (PredicateFailure (Ledger.EraRule "CERT" era))
  ) => ToObject (Conway.ConwayCertsPredFailure era) where
  toObject verb = \case
    Conway.DelegateeNotRegisteredDELEG targetPool ->
      mconcat [ "kind" .= String "DelegateeNotRegisteredDELEG" , "targetPool" .= targetPool ]
    Conway.WithdrawalsNotInRewardsCERTS incorrectWithdrawals ->
      mconcat [ "kind" .= String "WithdrawalsNotInRewardsCERTS" , "incorrectWithdrawals" .= incorrectWithdrawals ]
    Conway.CertFailure f -> toObject verb f


instance
  ( Api.ShelleyLedgerEra era ~ ledgerera
  , Api.IsShelleyBasedEra era
  , ToObject (PPUPPredFailure ledgerera)
  , ToObject (PredicateFailure (Ledger.EraRule "UTXO" ledgerera))
  , Ledger.EraCrypto ledgerera ~ StandardCrypto
  , Show (Ledger.Value ledgerera)
  , ToJSON (Ledger.Value ledgerera)
  , ToJSON (Ledger.TxOut ledgerera)
  ) => ToObject (AlonzoUtxowPredFailure ledgerera) where
  toObject v (ShelleyInAlonzoUtxowPredFailure utxoPredFail) =
    toObject v utxoPredFail
  toObject _ (MissingRedeemers scripts) =
    mconcat [ "kind" .= String "MissingRedeemers"
             , "scripts" .= renderMissingRedeemers Api.shelleyBasedEra scripts
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
  toObject _ (NotAllowedSupplementalDatums disallowed acceptable) =
    mconcat [ "kind" .= String "NotAllowedSupplementalDatums"
             , "disallowed" .= Set.toList disallowed
             , "acceptable" .= Set.toList acceptable
             ]
  toObject _ (ExtraRedeemers rdmrs) =
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
  ( ToObject (PredicateFailure (ShelleyUTXO ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "UTXO" ledgerera))
  , Ledger.EraCrypto ledgerera ~ StandardCrypto
  , Core.Crypto (Ledger.EraCrypto ledgerera)
  ) => ToObject (ShelleyUtxowPredFailure ledgerera) where
  toObject _verb (ExtraneousScriptWitnessesUTXOW extraneousScripts) =
    mconcat [ "kind" .= String "InvalidWitnessesUTXOW"
             , "extraneousScripts" .= Set.map renderScriptHash extraneousScripts
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

instance
  ( ToObject (PPUPPredFailure ledgerera)
  , Show (Ledger.Value ledgerera)
  , ToJSON (Ledger.Value ledgerera)
  , ToJSON (Ledger.TxOut ledgerera)
  , Core.Crypto (Ledger.EraCrypto ledgerera)
  ) => ToObject (ShelleyUtxoPredFailure ledgerera) where
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
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
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


instance ToJSON Allegra.ValidityInterval where
  toJSON vi =
    Aeson.object $
        [ "invalidBefore"    .= x | x <- mbfield (Allegra.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield (Allegra.invalidHereafter vi) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance
  ( ToObject (PPUPPredFailure ledgerera)
  , ToJSON (Ledger.TxOut ledgerera)
  , Show (Ledger.Value ledgerera)
  , ToJSON (Ledger.Value ledgerera)
  , Core.Crypto (Ledger.EraCrypto ledgerera)
  ) => ToObject (AllegraUtxoPredFailure ledgerera) where
  toObject _verb (Allegra.BadInputsUTxO badInputs) =
    mconcat [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (Allegra.OutsideValidityIntervalUTxO validityInterval slot) =
    mconcat [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  toObject _verb (Allegra.MaxTxSizeUTxO txsize maxtxsize) =
    mconcat [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  toObject _verb Allegra.InputSetEmptyUTxO =
    mconcat [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (Allegra.FeeTooSmallUTxO minfee txfee) =
    mconcat [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (Allegra.ValueNotConservedUTxO consumed produced) =
    mconcat [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject _verb (Allegra.WrongNetwork network addrs) =
    mconcat [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (Allegra.WrongNetworkWithdrawal network addrs) =
    mconcat [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (Allegra.OutputTooSmallUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooSmallUTxO"
            , "outputs" .= badOutputs
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
            ]
  toObject verb (Allegra.UpdateFailure f) = toObject verb f
  toObject _verb (Allegra.OutputBootAddrAttrsTooBig badOutputs) =
    mconcat [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  toObject _verb Allegra.TriesToForgeADA =
    mconcat [ "kind" .= String "TriesToForgeADA" ]
  toObject _verb (Allegra.OutputTooBigUTxO badOutputs) =
    mconcat [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]

renderBadInputsUTxOErr :: Set (TxIn era) -> Aeson.Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Aeson.Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> textShow consumed <> " but produced " <> textShow produced

instance Ledger.Era era => ToObject (ShelleyPpupPredFailure era) where
  toObject _verb (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mconcat [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  toObject _verb (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mconcat [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (textShow votingPeriod)
             ]
  toObject _verb (PVCannotFollowPPUP badPv) =
    mconcat [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance
  ( ToObject (PredicateFailure (Core.EraRule "DELPL" ledgerera))
  , Core.Crypto (Ledger.EraCrypto ledgerera)
  ) => ToObject (ShelleyDelegsPredFailure ledgerera) where
  toObject _verb (DelegateeNotRegisteredDELEG targetPool) =
    mconcat [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  toObject _verb (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mconcat [ "kind" .= String "WithdrawalsNotInRewardsCERTS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  toObject verb (DelplFailure f) = toObject verb f


instance
  ( ToObject (PredicateFailure (Core.EraRule "POOL" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "DELEG" ledgerera))
  ) => ToObject (ShelleyDelplPredFailure ledgerera) where
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
  -- toObject _verb (WrongCertificateTypePOOL index) =
  --   case index of
  --     0 -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
  --                   , "error" .= String "Wrong certificate type: Delegation certificate"
  --                   ]
  --     1 -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
  --                   , "error" .= String "Wrong certificate type: MIR certificate"
  --                   ]
  --     2 -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
  --                   , "error" .= String "Wrong certificate type: Genesis certificate"
  --                   ]
  --     k -> mconcat [ "kind" .= String "WrongCertificateTypePOOL"
  --                   , "certificateType" .= k
  --                   , "error" .= String "Wrong certificate type: Unknown certificate type"
  --                   ]

  toObject _verb (WrongNetworkPOOL networkId listedNetworkId poolId) =
    mconcat [ "kind" .= String "WrongNetworkPOOL"
             , "networkId" .= String (textShow networkId)
             , "listedNetworkId" .= String (textShow listedNetworkId)
             , "poolId" .= String (textShow poolId)
             , "error" .= String "Wrong network ID in pool registration certificate"
             ]

instance
  ( ToObject (PredicateFailure (Core.EraRule "NEWEPOCH" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "RUPD" ledgerera))
  ) => ToObject (ShelleyTickPredFailure ledgerera) where
  toObject verb (NewEpochFailure f) = toObject verb f
  toObject verb (RupdFailure f) = toObject verb f

instance ToObject TicknPredicateFailure where
  toObject _verb x = case x of {} -- no constructors

instance
  ( ToObject (PredicateFailure (Core.EraRule "EPOCH" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "MIR" ledgerera))
  ) => ToObject (ShelleyNewEpochPredFailure ledgerera) where
  toObject verb (EpochFailure f) = toObject verb f
  toObject verb (MirFailure f) = toObject verb f
  toObject _verb (CorruptRewardUpdate update) =
    mconcat [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (textShow update) ]


instance
  ( ToObject (PredicateFailure (Core.EraRule "POOLREAP" ledgerera))
  , ToObject (PredicateFailure (Core.EraRule "SNAP" ledgerera))
  , ToObject (UpecPredFailure ledgerera)
  ) => ToObject (ShelleyEpochPredFailure ledgerera) where
  toObject verb (PoolReapFailure f) = toObject verb f
  toObject verb (SnapFailure f) = toObject verb f
  toObject verb (UpecFailure f) = toObject verb f


instance ToObject (ShelleyPoolreapPredFailure ledgerera) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (ShelleySnapPredFailure ledgerera) where
  toObject _verb x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance ToObject (ShelleyNewppPredFailure ledgerera) where
  toObject _verb (UnexpectedDepositPot outstandingDeposits depositPot) =
    mconcat [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance ToObject (ShelleyMirPredFailure ledgerera) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (ShelleyRupdPredFailure ledgerera) where
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
            , "error" .= String
              ( mconcat
                [ "Your operational certificate's KES start period "
                , "is before the KES current period."
                ]
              )
            ]
  toObject _verb (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
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
  toObject _verb (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
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


instance
  ( Ledger.Era ledgerera
  , ToObject (PredicateFailure (Ledger.EraRule "UTXOS" ledgerera))
  , ToObject (PPUPPredFailure ledgerera)
  , ToJSON (Ledger.TxOut ledgerera)
  , Show (Ledger.Value ledgerera)
  , ToJSON (Ledger.Value ledgerera)
  ) => ToObject (AlonzoUtxoPredFailure ledgerera) where
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
            , "error" .= String
              ( mconcat
                [ "The output is smaller than the allow minimum "
                , "UTxO value defined in the protocol parameters"
                ]
              )
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

instance
  ( ToJSON (Alonzo.CollectError ledgerera)
  , ToObject (PPUPPredFailure ledgerera)
  ) => ToObject (AlonzoUtxosPredFailure ledgerera) where
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

instance
  ( Ledger.Era ledgerera
  , Show (PredicateFailure (Ledger.EraRule "LEDGERS" ledgerera))
  ) => ToObject (AlonzoBbodyPredFailure ledgerera) where
  toObject _ err = mconcat [ "kind" .= String "AlonzoBbodyPredFail"
                            , "error" .= String (textShow err)
                            ]

--------------------------------------------------------------------------------
-- Babbage related
--------------------------------------------------------------------------------

instance
  ( Ledger.Era ledgerera
  , ToObject (ShelleyUtxowPredFailure ledgerera)
  , ToObject (PredicateFailure (Ledger.EraRule "UTXOS" ledgerera))
  , ToObject (PPUPPredFailure ledgerera)
  , ToJSON (Ledger.TxOut ledgerera)
  , Show (Ledger.Value ledgerera)
  , ToJSON (Ledger.Value ledgerera)
  ) => ToObject (BabbageUtxoPredFailure ledgerera) where
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

instance
  ( Api.ShelleyLedgerEra era ~ ledgerera
  , Api.IsShelleyBasedEra era
  , Ledger.Era ledgerera
  , Ledger.EraCrypto ledgerera ~ StandardCrypto
  , Show (Ledger.Value ledgerera)
  , ToObject (PPUPPredFailure ledgerera)
  , ToObject (PredicateFailure (Ledger.EraRule "UTXO" ledgerera))
  , ToJSON (Ledger.Value ledgerera)
  , ToJSON (Ledger.TxOut ledgerera)
  ) => ToObject (BabbageUtxowPredFailure ledgerera) where
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

instance ToJSON ShelleyNodeToNodeVersion where
  toJSON ShelleyNodeToNodeVersion1 = String "ShelleyNodeToNodeVersion1"

instance ToJSON ShelleyNodeToClientVersion where
  toJSON ShelleyNodeToClientVersion1 = String "ShelleyNodeToClientVersion1"
  toJSON ShelleyNodeToClientVersion2 = String "ShelleyNodeToClientVersion2"
  toJSON ShelleyNodeToClientVersion3 = String "ShelleyNodeToClientVersion3"
  toJSON ShelleyNodeToClientVersion4 = String "ShelleyNodeToClientVersion4"
  toJSON ShelleyNodeToClientVersion5 = String "ShelleyNodeToClientVersion5"
  toJSON ShelleyNodeToClientVersion6 = String "ShelleyNodeToClientVersion6"
  toJSON ShelleyNodeToClientVersion7 = String "ShelleyNodeToClientVersion7"
  toJSON ShelleyNodeToClientVersion8 = String "ShelleyNodeToClientVersion8"

instance Ledger.Crypto c => ToObject (PraosChainSelectView c) where
  toObject _ PraosChainSelectView {
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
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk
