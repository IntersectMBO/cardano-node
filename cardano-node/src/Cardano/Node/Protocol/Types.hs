{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Protocol.Types
  ( Protocol(..)
  , SomeConsensusProtocol(..)
  ) where

import qualified Cardano.Api as Api

import           Cardano.Node.Orphans ()
import           Cardano.Node.Queries (HasKESInfo, HasKESMetricsData)
import           Cardano.Node.TraceConstraints (TraceConstraints)

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           GHC.Generics (Generic)

import           NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Util.TypeLevel
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Consensus.Storage.LedgerDB.API (LedgerSupportsLedgerDB)
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM (SerialiseTable, MemAndDiskTable)
import Data.SOP.Constraint


data Protocol = CardanoProtocol
  deriving (Eq, Generic)

instance Show Protocol where
  show CardanoProtocol = "Byron; Shelley"

deriving instance NFData Protocol
deriving instance NoThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of
      "Cardano" -> pure CardanoProtocol
      _ -> fail $ "Parsing of Protocol failed. " <> show str <> " is not a valid protocol"

data SomeConsensusProtocol where

     SomeConsensusProtocol :: forall blk. ( Api.Protocol IO blk
                                          , HasKESMetricsData blk
                                          , HasKESInfo blk
                                          , TraceConstraints blk
                                          , LedgerSupportsProtocol blk
        , LedgerSupportsLedgerDB blk
        , IndexedMemPack LedgerState blk UTxOTable
        , IndexedValue
            LedgerState
            UTxOTable
            blk
            ~ Ouroboros.Consensus.Ledger.Basics.Value UTxOTable blk
        , All
                              (SerialiseTable LedgerState blk) (TablesForBlock blk)
        , ToAllDict
                          (MemAndDiskTable LedgerState blk) (TablesForBlock blk)

                                          )
                           => Api.BlockType blk
                           -> Api.ProtocolInfoArgs blk
                           -> SomeConsensusProtocol
