{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Byron () where

import           Cardano.Api (textShow)

import           Data.Aeson (Value (..))
import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.Render (renderTxId)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Network.Block (blockHash, blockNo, blockSlot)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..),
                   ByronOtherHeaderEnvelopeError (..), TxId (..), byronHeaderRaw)
import           Ouroboros.Consensus.Byron.Ledger.Inspect (ByronLedgerUpdate (..),
                   ProtocolUpdate (..), UpdateState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Cardano.Chain.Block (ABlockOrBoundaryHdr (..), AHeader (..),
                   ChainValidationError (..), delegationCertificate)
import           Cardano.Chain.Byron.API (ApplyMempoolPayloadErr (..))
import           Cardano.Chain.Delegation (delegateVK)
import           Cardano.Crypto.Signing (VerificationKey)

{- HLINT ignore "Use :" -}

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ToObject ApplyMempoolPayloadErr where
  toObject _verb (MempoolTxErr utxoValidationErr) =
    mconcat
      [ "kind" .= String "MempoolTxErr"
      , "error" .= String (textShow utxoValidationErr)
      ]
  toObject _verb (MempoolDlgErr delegScheduleError) =
    mconcat
      [ "kind" .= String "MempoolDlgErr"
      , "error" .= String (textShow delegScheduleError)
      ]
  toObject _verb (MempoolUpdateProposalErr iFaceErr) =
    mconcat
      [ "kind" .= String "MempoolUpdateProposalErr"
      , "error" .= String (textShow iFaceErr)
      ]
  toObject _verb (MempoolUpdateVoteErr iFaceErrr) =
    mconcat
      [ "kind" .= String "MempoolUpdateVoteErr"
      , "error" .= String (textShow iFaceErrr)
      ]

instance ToObject ByronLedgerUpdate where
  toObject verb (ByronUpdatedProtocolUpdates protocolUpdates) =
    mconcat
      [ "kind"            .= String "ByronUpdatedProtocolUpdates"
      , "protocolUpdates" .= map (toObject verb) protocolUpdates
      ]

instance ToObject ProtocolUpdate where
  toObject verb (ProtocolUpdate updateVersion updateState) =
    mconcat
      [ "kind"                  .= String "ProtocolUpdate"
      , "protocolUpdateVersion" .= updateVersion
      , "protocolUpdateState"   .= toObject verb updateState
      ]

instance ToObject UpdateState where
  toObject _verb updateState = case updateState of
      UpdateRegistered slot ->
        mconcat
          [ "kind" .= String "UpdateRegistered"
          , "slot" .= slot
          ]
      UpdateActive votes ->
        mconcat
          [ "kind"  .= String "UpdateActive"
          , "votes" .= map (Text.pack . show) (Set.toList votes)
          ]
      UpdateConfirmed slot ->
        mconcat
          [ "kind" .= String "UpdateConfirmed"
          , "slot" .= slot
          ]
      UpdateStablyConfirmed endorsements ->
        mconcat
          [ "kind"         .= String "UpdateStablyConfirmed"
          , "endorsements" .= map (Text.pack . show) (Set.toList endorsements)
          ]
      UpdateCandidate slot epoch ->
        mconcat
          [ "kind" .= String "UpdateCandidate"
          , "slot" .= slot
          , "epoch" .= epoch
          ]
      UpdateStableCandidate transitionEpoch ->
        mconcat
          [ "kind"            .= String "UpdateStableCandidate"
          , "transitionEpoch" .= transitionEpoch
          ]

instance ToObject (GenTx ByronBlock) where
  toObject _ tx = mconcat [ "txid" .= Text.take 8 (renderTxId (txId tx)) ]


instance ToJSON (TxId (GenTx ByronBlock)) where
  toJSON = String . Text.take 8 . renderTxId


instance ToObject ChainValidationError where
  toObject _verb ChainValidationBoundaryTooLarge =
    mconcat
      [ "kind" .= String "ChainValidationBoundaryTooLarge" ]
  toObject _verb ChainValidationBlockAttributesTooLarge =
    mconcat
      [ "kind" .= String "ChainValidationBlockAttributesTooLarge" ]
  toObject _verb (ChainValidationBlockTooLarge _ _) =
    mconcat
      [ "kind" .= String "ChainValidationBlockTooLarge" ]
  toObject _verb ChainValidationHeaderAttributesTooLarge =
    mconcat
      [ "kind" .= String "ChainValidationHeaderAttributesTooLarge" ]
  toObject _verb (ChainValidationHeaderTooLarge _ _) =
    mconcat
      [ "kind" .= String "ChainValidationHeaderTooLarge" ]
  toObject _verb (ChainValidationDelegationPayloadError err) =
    mconcat
      [ "kind" .= String err ]
  toObject _verb (ChainValidationInvalidDelegation _ _) =
    mconcat
      [ "kind" .= String "ChainValidationInvalidDelegation" ]
  toObject _verb (ChainValidationGenesisHashMismatch _ _) =
    mconcat
      [ "kind" .= String "ChainValidationGenesisHashMismatch" ]
  toObject _verb (ChainValidationExpectedGenesisHash _ _) =
    mconcat
      [ "kind" .= String "ChainValidationExpectedGenesisHash" ]
  toObject _verb (ChainValidationExpectedHeaderHash _ _) =
    mconcat
      [ "kind" .= String "ChainValidationExpectedHeaderHash" ]
  toObject _verb (ChainValidationInvalidHash _ _) =
    mconcat
      [ "kind" .= String "ChainValidationInvalidHash" ]
  toObject _verb (ChainValidationMissingHash _) =
    mconcat
      [ "kind" .= String "ChainValidationMissingHash" ]
  toObject _verb (ChainValidationUnexpectedGenesisHash _) =
    mconcat
      [ "kind" .= String "ChainValidationUnexpectedGenesisHash" ]
  toObject _verb (ChainValidationInvalidSignature _) =
    mconcat
      [ "kind" .= String "ChainValidationInvalidSignature" ]
  toObject _verb (ChainValidationDelegationSchedulingError _) =
    mconcat
      [ "kind" .= String "ChainValidationDelegationSchedulingError" ]
  toObject _verb (ChainValidationProtocolMagicMismatch _ _) =
    mconcat
      [ "kind" .= String "ChainValidationProtocolMagicMismatch" ]
  toObject _verb ChainValidationSignatureLight =
    mconcat
      [ "kind" .= String "ChainValidationSignatureLight" ]
  toObject _verb (ChainValidationTooManyDelegations _) =
    mconcat
      [ "kind" .= String "ChainValidationTooManyDelegations" ]
  toObject _verb (ChainValidationUpdateError _ _) =
    mconcat
      [ "kind" .= String "ChainValidationUpdateError" ]
  toObject _verb (ChainValidationUTxOValidationError _) =
    mconcat
      [ "kind" .= String "ChainValidationUTxOValidationError" ]
  toObject _verb (ChainValidationProofValidationError _) =
    mconcat
      [ "kind" .= String "ChainValidationProofValidationError" ]


instance ToObject (Header ByronBlock) where
  toObject _verb b =
    mconcat $
        [ "kind" .= String "ByronBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
        ] <>
        case byronHeaderRaw b of
          ABOBBoundaryHdr{} -> []
          ABOBBlockHdr h ->
            [ "delegate" .= condense (headerSignerVk h) ]
   where
     headerSignerVk :: AHeader ByteString -> VerificationKey
     headerSignerVk =
       delegateVK . delegationCertificate . headerSignature


instance ToObject ByronOtherHeaderEnvelopeError where
  toObject _verb (UnexpectedEBBInSlot slot) =
    mconcat
      [ "kind" .= String "UnexpectedEBBInSlot"
      , "slot" .= slot
      ]
