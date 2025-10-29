{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Node.TraceConstraints (TraceConstraints) where

import           Cardano.BM.Tracing (ToObject)
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Keys
import           Cardano.Logging (LogFormatting)
import           Cardano.Node.Queries (ConvertTxId, GetKESInfo (..), HasKESInfo (..),
                   HasKESMetricsData (..), LedgerQueries)
import           Cardano.Protocol.Crypto (StandardCrypto)
import           Cardano.Tracing.HasIssuer (HasIssuer)
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ForgeStateUpdateError,
                   GetHeader, HasHeader, Header)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent, LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, HasTxId, HasTxs (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (HasNetworkProtocolVersion (BlockNodeToClientVersion, BlockNodeToNodeVersion))
import           Ouroboros.Consensus.Node.Run (RunNode, SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Peras.SelectView
import           Ouroboros.Consensus.Protocol.Abstract (SelectView, ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)
import           Ouroboros.Network.Block (Serialised)

import           Data.Aeson
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set

-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId blk
    , HasIssuer blk
    , HasKESMetricsData blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , ToJSON   (TxId (GenTx blk))
    , HasKESMetricsData blk
    , HasKESInfo blk
    , GetKESInfo blk
    , RunNode blk

    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (WeightedSelectView (BlockProtocol blk))
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotForge blk)
    , ToObject (ForgeStateUpdateError blk)

    , ToJSON (BlockNodeToClientVersion blk)
    , ToJSON (BlockNodeToNodeVersion blk)

    , LogFormatting (ApplyTxErr blk)
    , LogFormatting (GenTx blk)
    , LogFormatting (Header blk)
    , LogFormatting (LedgerError blk)
    , LogFormatting (LedgerUpdate blk)
    , LogFormatting (LedgerWarning blk)
    , LogFormatting (OtherHeaderEnvelopeError blk)
    , LogFormatting (WeightedSelectView (BlockProtocol blk))
    , LogFormatting (ValidationErr (BlockProtocol blk))
    , LogFormatting (CannotForge blk)
    , LogFormatting (ForgeStateUpdateError blk)
    , LogFormatting (Set (Credential 'Staking))
    , LogFormatting (NonEmpty.NonEmpty (KeyHash 'Staking))
    )
