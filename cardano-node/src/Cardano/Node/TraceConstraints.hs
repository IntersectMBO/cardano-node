{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Cardano.Node.TraceConstraints (TraceConstraints) where

import           Prelude (Show)

import           Data.Aeson

import           Cardano.BM.Tracing (ToObject)
import           Cardano.Logging (LogFormatting)
import           Cardano.Node.Queries (ConvertTxId, GetKESInfo (..), HasKESInfo (..),
                   HasKESMetricsData (..), LedgerQueries)

import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure)
import           Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail)
import           Cardano.Ledger.Crypto (StandardCrypto)

import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ConvertRawHash,
                   ForgeStateUpdateError, Header)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent, LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, HasTxId, HasTxs (..))
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Network.Block (StandardHash)


-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , StandardHash blk
    , ToJSON   (TxId (GenTx blk))
    , HasKESMetricsData blk
    , HasKESInfo blk
    , ConvertRawHash blk
    , GetKESInfo blk
    , Show blk
    , Show (Header blk)

    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotForge blk)
    , ToObject (ForgeStateUpdateError blk)
    , ToObject (UtxoPredicateFailure (AlonzoEra StandardCrypto))
    , ToObject (AlonzoBbodyPredFail (AlonzoEra StandardCrypto))
    , ToObject (UtxowPredicateFail (AlonzoEra StandardCrypto))

    , LogFormatting (LedgerUpdate blk)
    , LogFormatting (LedgerWarning blk)
    , LogFormatting (ApplyTxErr blk)
    , LogFormatting (GenTx blk)
    , LogFormatting (Header blk)
    , LogFormatting (ChainDB.InvalidBlockReason blk)
    , LogFormatting (LedgerError blk)
    , LogFormatting (OtherHeaderEnvelopeError blk)
    , LogFormatting (ValidationErr (BlockProtocol blk))
    , LogFormatting (CannotForge blk)
    , LogFormatting (ForgeStateUpdateError blk)
    , LogFormatting (UtxoPredicateFailure (AlonzoEra StandardCrypto))
    , LogFormatting (AlonzoBbodyPredFail (AlonzoEra StandardCrypto))
    , LogFormatting (UtxowPredicateFail (AlonzoEra StandardCrypto))
    )
