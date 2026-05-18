{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Node.TraceConstraints (TraceConstraints) where

import           Cardano.Ledger.Credential
import           Cardano.Ledger.Keys
import           Cardano.Logging (LogFormatting)
import           Cardano.Node.Queries (ConvertTxId, GetKESInfo (..), HasKESInfo (..),
                   HasKESMetricsData (..), LedgerQueries)
import           Cardano.Node.Tracing.Tracers.HasIssuer (HasIssuer)
import           Cardano.Node.Tracing.Tracers.KESInfo ()
import qualified Cardano.Node.Tracing.Tracers.Consensus as ConsensusTracers
import           Cardano.Protocol.Crypto (StandardCrypto)
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ForgeStateUpdateError,
                   GetHeader, HasHeader, Header, HeaderHash)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent, LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, HasTxId, HasTxs (..))
import           Ouroboros.Consensus.Node.Run (RunNode, SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Peras.SelectView
import           Ouroboros.Consensus.Protocol.Abstract (ReasonForSwitch, SelectView,
                   SelectViewReasonForSwitch, TiebreakerView, ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx)
import           Ouroboros.Network.Block (Serialised)

import           Data.Aeson
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set

-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId blk
    , HasKESMetricsData blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , HasKESMetricsData blk
    , HasKESInfo blk
    , GetKESInfo blk
    , RunNode blk
    , HasIssuer blk

    , ToJSON (HeaderHash blk)

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
    , LogFormatting (Set (Credential Staking))
    , LogFormatting (NonEmpty.NonEmpty (KeyHash Staking))
    , LogFormatting (Either (WithEmptyFragmentReasonForSwitch (WeightedSelectView (BlockProtocol blk))) (SelectViewReasonForSwitch (BlockProtocol blk)))
    , LogFormatting (ReasonForSwitch (TiebreakerView (BlockProtocol blk)))
    )
