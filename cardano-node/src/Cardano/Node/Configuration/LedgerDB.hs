{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Configuration.LedgerDB (
    DeprecatedOptions (..),
    LedgerDbConfiguration (..),
    LedgerDbSelectorFlag (..),
    noDeprecatedOptions,
    selectorToArgs,
) where

import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM

import           Data.Proxy
import           System.Random (StdGen)

-- | Choose the LedgerDB Backend
--
-- As of UTxO-HD, the LedgerDB uses either an in-memory backend or an LSM-tree
-- backend to keep track of differences in the UTxO set.
--
-- - 'V2InMemory': uses more memory than the minimum requirements but is somewhat
--   faster.
--
-- - 'V2LSM': keeps the UTxO set on disk in an LSM tree, which uses less memory
--   but is somewhat slower.
data LedgerDbSelectorFlag =
    V2InMemory
  | V2LSM
      (Maybe FilePath)
      -- ^ Maybe a custom path to the LSM database. If not provided the default
      -- will be used (@<fast-storage>/lsm@).
      (Maybe FilePath)
      -- ^ Maybe a path to which the LSM backend will export standalone
      -- snapshots on every snapshot. If not provided, no standalone snapshots
      -- are exported.

  deriving (Eq, Show)

-- | Some options that existed in the TopLevel were now moved to a
-- subsection. We use this field to propagate the results from parsing those
-- into the monadic part of the node so that we can emit warnings.
newtype DeprecatedOptions = DeprecatedOptions [String]
  deriving (Eq, Show)

noDeprecatedOptions :: DeprecatedOptions
noDeprecatedOptions = DeprecatedOptions []

data LedgerDbConfiguration =
    LedgerDbConfiguration
      SnapshotPolicyArgs
      QueryBatchSize
      LedgerDbSelectorFlag
      DeprecatedOptions
  deriving (Eq, Show)

-- | A number of gigabytes.
newtype Gigabytes = Gigabytes Int
  deriving stock (Eq, Show)
  deriving newtype (Read, Aeson.FromJSON)

selectorToArgs ::
    forall blk.
    ( LedgerSupportsProtocol blk
    , LedgerDbSerialiseConstraints blk
    , CanUpgradeLedgerTables LedgerState blk
    ) => LedgerDbSelectorFlag -> FilePath -> StdGen -> (LedgerDbBackendArgs IO blk, StdGen)
selectorToArgs V2InMemory _ =
  InMemory.mkInMemoryArgs
selectorToArgs (V2LSM fp fpExport) fastStoragePath =
  LSM.mkLSMArgsIO
    (Proxy @blk)
    (fromMaybe "lsm" fp)
    fpExport
    fastStoragePath
