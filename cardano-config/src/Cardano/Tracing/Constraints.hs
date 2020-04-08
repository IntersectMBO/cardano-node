{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}

module Cardano.Tracing.Constraints (TraceConstraints) where

import           Prelude (Show)

import           Data.Aeson (ToJSON)
import           Cardano.BM.Tracing

import           Ouroboros.Network.Block
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
                   (LedgerError, BlockProtocol)
import           Ouroboros.Consensus.Mempool.API
                   (GenTx, GenTxId, HasTxId, HasTxs(..), ApplyTxErr, TxId)
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)


-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Condense (TxId (GenTx blk))
    , HasTxs blk
    , HasTxId (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (Header blk)
    , Show (TxId (GenTx blk))
    , ToJSON   (TxId (GenTx blk))
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    )

