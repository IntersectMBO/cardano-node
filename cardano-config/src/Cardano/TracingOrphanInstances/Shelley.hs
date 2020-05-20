{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE DerivingStrategies    #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TracingOrphanInstances.Shelley () where

import           Cardano.Prelude

import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Slotting.Block (BlockNo(..))
import           Cardano.TracingOrphanInstances.Common
import           Cardano.TracingOrphanInstances.Consensus ()
import           Cardano.Config.Shelley.Orphans ()

import           Ouroboros.Network.Block
                   (SlotNo(..), blockHash, blockSlot, blockNo, blockPrevHash)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Mempool.API (GenTx, TxId, txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Ouroboros.Consensus.Shelley.Ledger
import           Shelley.Spec.Ledger.API
import           Shelley.Spec.Ledger.BlockChain (LastAppliedBlock(..))
import           Shelley.Spec.Ledger.Keys (KeyHash(..))
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
import           Shelley.Spec.Ledger.STS.Prtcl
import           Shelley.Spec.Ledger.STS.Rupd
import           Shelley.Spec.Ledger.STS.Snap
import           Shelley.Spec.Ledger.STS.Tick
import           Shelley.Spec.Ledger.STS.Updn
import           Shelley.Spec.Ledger.STS.Utxo
import           Shelley.Spec.Ledger.STS.Utxow


--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted in roughly topological order.

instance Crypto c => ToObject (GenTx (ShelleyBlock c)) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]

instance ToJSON (TxId (GenTx (ShelleyBlock c))) where
  toJSON i = toJSON (condense i)

instance Crypto c => ToObject (Header (ShelleyBlock c)) where
  toObject _verb b =
    mkObject $
        [ "kind" .= String "ShelleyBlock"
        , "hash" .= condense (blockHash b)
        , "prevhash" .= condense (blockPrevHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]


-- This instance is completly insane. We realy need to have lists as a generic
-- instance.
instance ToObject (PredicateFailure r) => ToObject [[PredicateFailure r]] where
  toObject verb fss =
    mkObject
      [ "kind" .= String "PredicateFailure"
      , "failures" .= map (toObject verb) (concat fss)
      ]


instance Crypto c =>  ToObject (ShelleyLedgerError c) where
  toObject verb (BBodyError (BlockTransitionError fs)) =
    mkObject [ "kind" .= String "BBodyError"
             , "failures" .= map (toObject verb) fs
             ]
  toObject verb (TickError (TickTransitionError fs)) =
    mkObject [ "kind" .= String "TickError"
             , "failures" .= map (toObject verb) fs
             ]


instance Crypto c => ToObject (PredicateFailure (CHAIN c)) where
  toObject _verb HeaderSizeTooLargeCHAIN =
    mkObject [ "kind" .= String "HeaderSizeTooLarge" ]
  toObject _verb BlockSizeTooLargeCHAIN =
    mkObject [ "kind" .= String "BlockSizeTooLarge" ]
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
  toObject verb (PrtclFailure f) = toObject verb f
  toObject verb (PrtclSeqFailure f) = toObject verb f

instance Crypto c => ToObject (PrtlSeqFailure c) where
  toObject _verb (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mkObject [ "kind" .= String "WrongSlotInterval"
             , "Conflicting slot numbers - Last slot number: " .= lastSlot
             , "Current slot number: " .= currSlot
             ]
  toObject _verb (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mkObject [ "kind" .= String "WrongBlockNo"
             , "Last applied block number" .= showLastAppBlockNo lab
             , "Current block number" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  toObject _verb (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mkObject [ "kind" .= String "WrongBlockSequence"
             , "Last applied block hash: " .= (String $ textShow lastAppliedHash)
             , "Current block hash: " .= (String $ textShow currentHash)
             ]

instance Crypto c =>  ToObject (PredicateFailure (BBODY c)) where
  toObject _verb WrongBlockBodySizeBBODY =
    mkObject [ "kind" .= String "WrongBlockBodySizeBBODY" ]
  toObject _verb InvalidBodyHashBBODY =
    mkObject [ "kind" .= String "InvalidBodyHashBBODY" ]
  toObject verb (LedgersFailure f) = toObject verb f


instance Crypto c =>  ToObject (PredicateFailure (LEDGERS c)) where
  toObject verb (LedgerFailure f) = toObject verb f


instance Crypto c =>  ToObject (PredicateFailure (LEDGER c)) where
  toObject verb (UtxowFailure f) = toObject verb f
  toObject verb (DelegsFailure f) = toObject verb f


instance Crypto c => ToObject (PredicateFailure (UTXOW c)) where
  toObject _verb (InvalidWitnessesUTXOW wits) =
    mkObject [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalid witnesses" .= (String $ show wits)
             ]
  toObject _verb (MissingVKeyWitnessesUTXOW wits) =
    mkObject [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missing witnesses" .= (String $ show wits)
             ]
  toObject _verb MissingScriptWitnessesUTXOW =
    mkObject [ "kind" .= String "MissingScriptWitnessesUTXOW" ]
  toObject _verb ScriptWitnessNotValidatingUTXOW =
    mkObject [ "kind" .= String "ScriptWitnessNotValidatingUTXOW" ]
  toObject verb (UtxoFailure f) = toObject verb f
  toObject _verb MIRInsufficientGenesisSigsUTXOW =
    mkObject [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW" ]
  toObject _verb MIRImpossibleInDecentralizedNetUTXOW =
    mkObject [ "kind" .= String "MIRImpossibleInDecentralizedNetUTXOW" ]
  toObject _verb BadMetaDataHashUTXOW =
    mkObject [ "kind" .= String "BadMetaDataHashUTXOW" ]


instance ToObject (PredicateFailure (UTXO c)) where
  toObject _verb BadInputsUTxO =
    mkObject [ "kind" .= String "BadInputsUTxO" ]
  toObject _verb (ExpiredUTxO ttl slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  toObject _verb (MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb OutputTooSmallUTxO =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
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
             , "produced" .= produced ]
  toObject verb (UpdateFailure f) = toObject verb f


instance ToObject (PredicateFailure (PPUP c)) where
  toObject _verb (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mkObject [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  toObject _verb PPUpdateTooLatePPUP =
    mkObject [ "kind" .= String "PPUpdateTooLatePPUP" ]
  toObject _verb (PPUpdateWrongEpoch te) =
    mkObject [ "kind" .= String "PPUpdateWrongEpoch"
             , "targetEpoch" .= te ]
  toObject _verb PVCannotFollowPPUP =
    mkObject [ "kind" .= String "PVCannotFollowPPUP" ]


instance ToObject (PredicateFailure (DELEGS c)) where
  toObject _verb DelegateeNotRegisteredDELEG =
    mkObject [ "kind" .= String "DelegateeNotRegisteredDELEG" ]
  toObject _verb WithdrawalsNotInRewardsDELEGS =
    mkObject [ "kind" .= String "WithdrawalsNotInRewardsDELEGS" ]
  toObject verb (DelplFailure f) = toObject verb f


instance ToObject (PredicateFailure (DELPL c)) where
  toObject verb (PoolFailure f) = toObject verb f
  toObject verb (DelegFailure f) = toObject verb f
  toObject _verb ScriptNotInWitnessDELPL =
    mkObject [ "kind" .= String "ScriptNotInWitnessDELPL" ]
  toObject _verb ScriptHashNotMatchDELPL =
    mkObject [ "kind" .= String "ScriptHashNotMatchDELPL" ]
  toObject _verb ScriptDoesNotValidateDELPL =
    mkObject [ "kind" .= String "ScriptDoesNotValidateDELPL" ]


instance ToObject (PredicateFailure (DELEG c)) where
  toObject _verb StakeKeyAlreadyRegisteredDELEG =
    mkObject [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG" ]
  toObject _verb StakeKeyNotRegisteredDELEG =
    mkObject [ "kind" .= String "StakeKeyNotRegisteredDELEG" ]
  toObject _verb StakeKeyNonZeroAccountBalanceDELEG =
    mkObject [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG" ]
  toObject _verb StakeDelegationImpossibleDELEG =
    mkObject [ "kind" .= String "StakeDelegationImpossibleDELEG" ]
  toObject _verb WrongCertificateTypeDELEG =
    mkObject [ "kind" .= String "WrongCertificateTypeDELEG" ]
  toObject _verb GenesisKeyNotInpMappingDELEG =
    mkObject [ "kind" .= String "GenesisKeyNotInpMappingDELEG" ]
  toObject _verb DuplicateGenesisDelegateDELEG =
    mkObject [ "kind" .= String "DuplicateGenesisDelegateDELEG" ]
  toObject _verb InsufficientForInstantaneousRewardsDELEG =
    mkObject [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG" ]
  toObject _verb MIRCertificateTooLateinEpochDELEG =
    mkObject [ "kind" .= String "MIRCertificateTooLateinEpochDELEG" ]


instance ToObject (PredicateFailure (POOL c)) where
  toObject _verb StakePoolNotRegisteredOnKeyPOOL =
    mkObject [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL" ]
  toObject _verb StakePoolRetirementWrongEpochPOOL =
    mkObject [ "kind" .= String "StakePoolRetirementWrongEpochPOOL" ]
  toObject _verb WrongCertificateTypePOOL =
    mkObject [ "kind" .= String "WrongCertificateTypePOOL" ]


instance ToObject (PredicateFailure (TICK c)) where
  toObject verb (NewEpochFailure f) = toObject verb f
  toObject verb (RupdFailure f) = toObject verb f


instance ToObject (PredicateFailure (NEWEPOCH c)) where
  toObject verb (EpochFailure f) = toObject verb f
  toObject verb (MirFailure f) = toObject verb f
  toObject _verb (CorruptRewardUpdate update) =
    mkObject [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (show update) ]


instance ToObject (PredicateFailure (EPOCH c)) where
  toObject verb (PoolReapFailure f) = toObject verb f
  toObject verb (SnapFailure f) = toObject verb f
  toObject verb (NewPpFailure f) = toObject verb f


instance ToObject (PredicateFailure (POOLREAP c)) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (PredicateFailure (SNAP c)) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (PredicateFailure (NEWPP c)) where
  toObject _verb UnexpectedDepositPot =
    mkObject [ "kind" .= String "UnexpectedDepositPot" ]


instance ToObject (PredicateFailure (MIR c)) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (PredicateFailure (RUPD c)) where
  toObject _verb x = case x of {} -- no constructors


instance Crypto c => ToObject (PredicateFailure (PRTCL c)) where
  toObject  verb (OverlayFailure f) = toObject verb f
  toObject  verb (UpdnFailure f) = toObject verb f


instance Crypto c => ToObject (PredicateFailure (OVERLAY c)) where
  toObject _verb (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) =
    mkObject [ "kind" .= String "UnknownGenesisKeyOVERLAY"
             , "Unknown genesis key hash" .= (String $ textShow genKeyHash)
             ]
  toObject _verb (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mkObject [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "Seed nonce" .= (String $ textShow seedNonce)
             , "Current slot number" .= (String $ textShow currSlotNo)
             , "Previous hash as nonce" .= (String $ textShow prevHashNonce)
             , "Leader election value" .= (String $ textShow leaderElecVal)
             ]
  toObject _verb (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mkObject [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "Seed nonce" .= (String $ textShow seedNonce)
             , "Current slot number" .= (String $ textShow currSlotNo)
             , "Previous hash as nonce" .= (String $ textShow prevHashNonce)
             , "Block nonce" .= (String $ textShow blockNonce)
             ]
  toObject _verb (VRFKeyWrongVRFKey regVRFKeyHash unregVRFKeyHash) =
    mkObject [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "registered/correct VRF key hash (exists in stake pool distribution)" .= (String $ textShow regVRFKeyHash )
             , "unregistered/incorrect VRF key hash (does not exist in stake pool distribution)" .= (String $ textShow unregVRFKeyHash)
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  toObject _verb (VRFKeyUnknown (KeyHash kHash)) =
    mkObject [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "key hash" .= (String $ textShow kHash)
             ]
  toObject _verb (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mkObject [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "Leader election value" .= (String $ textShow leadElecVal)
             , "Weight of delegation pool" .= (String $ textShow weightOfDelegPool)
             , "Active slot coefficient" .= (String $ textShow actSlotCoefff)
             ]
  toObject _verb (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mkObject [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= (String $ textShow notActiveSlotNo)
             ]
  toObject _verb (WrongGenesisColdKeyOVERLAY actual expected) =
    mkObject [ "kind" .= String "WrongGenesisColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  toObject verb (OcertFailure f) = toObject verb f


instance ToObject (PredicateFailure (OCERT c)) where
  toObject _verb KESBeforeStartOCERT =
    mkObject [ "kind" .= String "KESBeforeStartOCERT" ]
  toObject _verb KESAfterEndOCERT =
    mkObject [ "kind" .= String "KESAfterEndOCERT" ]
  toObject _verb KESPeriodWrongOCERT =
    mkObject [ "kind" .= String "KESPeriodWrongOCERT" ]
  toObject _verb InvalidSignatureOCERT =
    mkObject [ "kind" .= String "InvalidSignatureOCERT" ]
  toObject _verb (InvalidKesSignatureOCERT _ _ _ err) =
    -- TODO: Include fields for the ignored parameters.
    mkObject [ "kind" .= String "InvalidKesSignatureOCERT"
             , "err"  .= err ]
  toObject _verb NoCounterForKeyHashOCERT =
    mkObject [ "kind" .= String "NoCounterForKeyHashOCERT" ]


instance ToObject (PredicateFailure (UPDN c)) where
  toObject _verb x = case x of {} -- no constructors

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk
