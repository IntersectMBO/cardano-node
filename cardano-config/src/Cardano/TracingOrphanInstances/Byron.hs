{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.TracingOrphanInstances.Byron () where

import           Cardano.Prelude

import           Cardano.TracingOrphanInstances.Common
import           Cardano.TracingOrphanInstances.Consensus ()

import           Ouroboros.Network.Block
                   (blockHash, blockSlot, blockNo, blockPrevHash)
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Byron.Ledger
                   (ByronBlock(..), byronHeaderRaw)
import           Ouroboros.Consensus.Mempool.API (GenTx, TxId, txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Cardano.Chain.Block
                  (ChainValidationError(..), ABlockOrBoundaryHdr(..),
                   AHeader(..), delegationCertificate)
import           Cardano.Chain.Delegation (delegateVK)
import           Cardano.Crypto.Signing (VerificationKey)

import           Ouroboros.Consensus.Byron.Ledger (TxId(..))


--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ToObject (GenTx ByronBlock) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]


instance ToJSON (TxId (GenTx ByronBlock)) where
  toJSON (ByronTxId             i) = toJSON (condense i)
  toJSON (ByronDlgId            i) = toJSON (condense i)
  toJSON (ByronUpdateProposalId i) = toJSON (condense i)
  toJSON (ByronUpdateVoteId     i) = toJSON (condense i)


instance ToObject ChainValidationError where
  toObject _verb ChainValidationBoundaryTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBoundaryTooLarge" ]
  toObject _verb ChainValidationBlockAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBlockAttributesTooLarge" ]
  toObject _verb (ChainValidationBlockTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationBlockTooLarge" ]
  toObject _verb ChainValidationHeaderAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationHeaderAttributesTooLarge" ]
  toObject _verb (ChainValidationHeaderTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationHeaderTooLarge" ]
  toObject _verb (ChainValidationDelegationPayloadError err) =
    mkObject
      [ "kind" .= String err ]
  toObject _verb (ChainValidationInvalidDelegation _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidDelegation" ]
  toObject _verb (ChainValidationGenesisHashMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationGenesisHashMismatch" ]
  toObject _verb (ChainValidationExpectedGenesisHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedGenesisHash" ]
  toObject _verb (ChainValidationExpectedHeaderHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedHeaderHash" ]
  toObject _verb (ChainValidationInvalidHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidHash" ]
  toObject _verb (ChainValidationMissingHash _) =
    mkObject
      [ "kind" .= String "ChainValidationMissingHash" ]
  toObject _verb (ChainValidationUnexpectedGenesisHash _) =
    mkObject
      [ "kind" .= String "ChainValidationUnexpectedGenesisHash" ]
  toObject _verb (ChainValidationInvalidSignature _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidSignature" ]
  toObject _verb (ChainValidationDelegationSchedulingError _) =
    mkObject
      [ "kind" .= String "ChainValidationDelegationSchedulingError" ]
  toObject _verb (ChainValidationProtocolMagicMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationProtocolMagicMismatch" ]
  toObject _verb ChainValidationSignatureLight =
    mkObject
      [ "kind" .= String "ChainValidationSignatureLight" ]
  toObject _verb (ChainValidationTooManyDelegations _) =
    mkObject
      [ "kind" .= String "ChainValidationTooManyDelegations" ]
  toObject _verb (ChainValidationUpdateError _ _) =
    mkObject
      [ "kind" .= String "ChainValidationUpdateError" ]
  toObject _verb (ChainValidationUTxOValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationUTxOValidationError" ]
  toObject _verb (ChainValidationProofValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationProofValidationError" ]


instance ToObject (Header ByronBlock) where
  toObject _verb b =
    mkObject $
        [ "kind" .= String "ByronBlock"
        , "hash" .= condense (blockHash b)
        , "prevhash" .= condense (blockPrevHash b)
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

