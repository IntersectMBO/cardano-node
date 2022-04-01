{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

module Cardano.Node.Tracing.Era.Byron () where

-- TODO: Temporary hack for toJSON instances
-- Will be moved when old tracing will be removed
import           Cardano.Tracing.OrphanInstances.Byron ()

import           Cardano.Logging
import           Cardano.Prelude
import           Data.Aeson (Value (String), (.=))

import qualified Data.Set as Set
import qualified Data.Text as Text

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
-- | instances of @LogFormatting@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance LogFormatting ApplyMempoolPayloadErr where
  forMachine _dtal (MempoolTxErr utxoValidationErr) =
    mconcat
      [ "kind" .= String "MempoolTxErr"
      , "error" .= String (show utxoValidationErr)
      ]
  forMachine _dtal (MempoolDlgErr delegScheduleError) =
    mconcat
      [ "kind" .= String "MempoolDlgErr"
      , "error" .= String (show delegScheduleError)
      ]
  forMachine _dtal (MempoolUpdateProposalErr iFaceErr) =
    mconcat
      [ "kind" .= String "MempoolUpdateProposalErr"
      , "error" .= String (show iFaceErr)
      ]
  forMachine _dtal (MempoolUpdateVoteErr iFaceErrr) =
    mconcat
      [ "kind" .= String "MempoolUpdateVoteErr"
      , "error" .= String (show iFaceErrr)
      ]

instance LogFormatting ByronLedgerUpdate where
  forMachine dtal (ByronUpdatedProtocolUpdates protocolUpdates) =
    mconcat
      [ "kind"            .= String "ByronUpdatedProtocolUpdates"
      , "protocolUpdates" .= map (forMachine dtal) protocolUpdates
      ]

instance LogFormatting ProtocolUpdate where
  forMachine dtal (ProtocolUpdate updateVersion updateState) =
    mconcat
      [ "kind"                  .= String "ProtocolUpdate"
      , "protocolUpdateVersion" .= updateVersion
      , "protocolUpdateState"   .= forMachine dtal updateState
      ]

instance LogFormatting UpdateState where
  forMachine _dtal updateState = case updateState of
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

instance LogFormatting (GenTx ByronBlock) where
  forMachine dtal tx =
    mconcat $
        ( "txid" .= txId tx )
     :  [ "tx"   .= condense tx | dtal == DDetailed ]

instance LogFormatting ChainValidationError where
  forMachine _dtal ChainValidationBoundaryTooLarge =
    mconcat
      [ "kind" .= String "ChainValidationBoundaryTooLarge" ]
  forMachine _dtal ChainValidationBlockAttributesTooLarge =
    mconcat
      [ "kind" .= String "ChainValidationBlockAttributesTooLarge" ]
  forMachine _dtal (ChainValidationBlockTooLarge _ _) =
    mconcat
      [ "kind" .= String "ChainValidationBlockTooLarge" ]
  forMachine _dtal ChainValidationHeaderAttributesTooLarge =
    mconcat
      [ "kind" .= String "ChainValidationHeaderAttributesTooLarge" ]
  forMachine _dtal (ChainValidationHeaderTooLarge _ _) =
    mconcat
      [ "kind" .= String "ChainValidationHeaderTooLarge" ]
  forMachine _dtal (ChainValidationDelegationPayloadError err) =
    mconcat
      [ "kind" .= String err ]
  forMachine _dtal (ChainValidationInvalidDelegation _ _) =
    mconcat
      [ "kind" .= String "ChainValidationInvalidDelegation" ]
  forMachine _dtal (ChainValidationGenesisHashMismatch _ _) =
    mconcat
      [ "kind" .= String "ChainValidationGenesisHashMismatch" ]
  forMachine _dtal (ChainValidationExpectedGenesisHash _ _) =
    mconcat
      [ "kind" .= String "ChainValidationExpectedGenesisHash" ]
  forMachine _dtal (ChainValidationExpectedHeaderHash _ _) =
    mconcat
      [ "kind" .= String "ChainValidationExpectedHeaderHash" ]
  forMachine _dtal (ChainValidationInvalidHash _ _) =
    mconcat
      [ "kind" .= String "ChainValidationInvalidHash" ]
  forMachine _dtal (ChainValidationMissingHash _) =
    mconcat
      [ "kind" .= String "ChainValidationMissingHash" ]
  forMachine _dtal (ChainValidationUnexpectedGenesisHash _) =
    mconcat
      [ "kind" .= String "ChainValidationUnexpectedGenesisHash" ]
  forMachine _dtal (ChainValidationInvalidSignature _) =
    mconcat
      [ "kind" .= String "ChainValidationInvalidSignature" ]
  forMachine _dtal (ChainValidationDelegationSchedulingError _) =
    mconcat
      [ "kind" .= String "ChainValidationDelegationSchedulingError" ]
  forMachine _dtal (ChainValidationProtocolMagicMismatch _ _) =
    mconcat
      [ "kind" .= String "ChainValidationProtocolMagicMismatch" ]
  forMachine _dtal ChainValidationSignatureLight =
    mconcat
      [ "kind" .= String "ChainValidationSignatureLight" ]
  forMachine _dtal (ChainValidationTooManyDelegations _) =
    mconcat
      [ "kind" .= String "ChainValidationTooManyDelegations" ]
  forMachine _dtal (ChainValidationUpdateError _ _) =
    mconcat
      [ "kind" .= String "ChainValidationUpdateError" ]
  forMachine _dtal (ChainValidationUTxOValidationError _) =
    mconcat
      [ "kind" .= String "ChainValidationUTxOValidationError" ]
  forMachine _dtal (ChainValidationProofValidationError _) =
    mconcat
      [ "kind" .= String "ChainValidationProofValidationError" ]


instance LogFormatting (Header ByronBlock) where
  forMachine _dtal b =
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


instance LogFormatting ByronOtherHeaderEnvelopeError where
  forMachine _dtal (UnexpectedEBBInSlot slot) =
    mconcat
      [ "kind" .= String "UnexpectedEBBInSlot"
      , "slot" .= slot
      ]
