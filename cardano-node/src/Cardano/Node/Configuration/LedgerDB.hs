{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.Node.Configuration.LedgerDB (
    DeprecatedOptions (..),
    LedgerDbConfiguration (..),
    LedgerDbSelectorFlag (..),
    Gigabytes,
    noDeprecatedOptions,
    selectorToArgs,
) where

import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM

import qualified Data.Aeson.Types as Aeson (FromJSON)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           System.FilePath
import           System.Random (StdGen)

import Ouroboros.Consensus.Ledger.Basics (LedgerState)

-- | Choose the LedgerDB Backend
--
-- As of UTxO-HD, the LedgerDB now uses either an in-memory backend or LMDB to
-- keep track of differences in the UTxO set.
--
-- - 'V2InMemory': uses more memory than the minimum requirements but is somewhat
--   faster.
--
-- - 'V1LMDB': uses less memory but is somewhat slower.
--
-- - 'V2LSM': Uses the LSM backend.
data LedgerDbSelectorFlag =
    V2InMemory
  | V2LSM
      (Maybe FilePath)
      -- ^ Maybe a custom path to the LSM database. If not provided the default
      -- will be used (@<fast-storage>/lsm@).

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

-- | Convert a number of Gigabytes to the equivalent number of bytes.
toBytes :: Gigabytes -> Int
toBytes (Gigabytes x) = x * 1024 * 1024 * 1024

defaultLMDBPath :: FilePath -> FilePath
defaultLMDBPath = (</> "lmdb")

selectorToArgs ::
    forall blk.
    ( LedgerSupportsProtocol blk
    , LedgerDbSerialiseConstraints blk
    , CanUpgradeLedgerTables (LedgerState blk)
    ) => LedgerDbSelectorFlag -> FilePath -> StdGen -> (LedgerDbBackendArgs IO blk, StdGen)
selectorToArgs V2InMemory _ = InMemory.mkInMemoryArgs
selectorToArgs (V2LSM fp) fastStoragePath = LSM.mkLSMArgsIO (Proxy @blk) (fromMaybe "lsm" fp) fastStoragePath
