{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM

import qualified Data.Aeson.Types as Aeson (FromJSON)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           System.FilePath
import           System.Random (StdGen)

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
    V1LMDB
      V1.FlushFrequency
      -- ^ The frequency at which changes are flushed to the disk.
      (Maybe FilePath)
      -- ^ Path for the live tables. If not provided the default will be used
      -- (@<fast-storage>/lmdb@).
      (Maybe Gigabytes)
      -- ^ A map size can be specified, this is the maximum disk space the LMDB
      -- database can fill. If not provided, the default of 16GB will be used.
      (Maybe Int)
      -- ^ An override to the max number of readers.
  | V2InMemory
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
      NumOfDiskSnapshots
      SnapshotInterval
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

-- | Recommended settings for the LMDB backing store.
--
-- === @'lmdbMapSize'@
-- The default @'LMDBLimits'@ uses an @'lmdbMapSize'@ of @1024 * 1024 * 1024 * 16@
-- bytes, or 16 Gigabytes. @'lmdbMapSize'@ sets the size of the memory map
-- that is used internally by the LMDB backing store, and is also the
-- maximum size of the on-disk database. 16 GB should be sufficient for the
-- medium term, i.e., it is sufficient until a more performant alternative to
-- the LMDB backing store is implemented, which will probably replace the LMDB
-- backing store altogether.
--
-- Note(jdral): It is recommended not to set the @'lmdbMapSize'@ to a value
-- that is much smaller than 16 GB through manual configuration: the node will
-- die with a fatal error as soon as the database size exceeds the
-- @'lmdbMapSize'@. If this fatal error were to occur, we would expect that
-- the node can continue normal operation if it is restarted with a higher
-- @'lmdbMapSize'@ configured. Nonetheless, this situation should be avoided.
--
-- === @'lmdbMaxDatabases'@
-- The @'lmdbMaxDatabases'@ is set to 10, which means that the LMDB backing
-- store will allow up @<= 10@ internal databases. We say /internal/
-- databases, since they are not exposed outside the backing store interface,
-- such that from the outside view there is just one /logical/ database.
-- Two of these internal databases are reserved for normal operation of the
-- backing store, while the remaining databases will be used to store ledger
-- tables. At the moment, there is at most one ledger table that will be
-- stored in an internal database: the UTxO. Nonetheless, we set
-- @'lmdbMaxDatabases'@ to @10@ in order to future-proof these limits.
--
-- === @'lmdbMaxReaders'@
-- The @'lmdbMaxReaders'@ limit sets the maximum number of threads that can
-- read from the LMDB database. Currently, there should only be a single reader
-- active. Again, we set @'lmdbMaxReaders'@ to @16@ in order to future-proof
-- these limits.
--
-- === References
-- For more information about LMDB limits, one should inspect:
-- * The @lmdb-simple@ and @haskell-lmdb@ forked repositories.
-- * The official LMDB API documentation at
--    <http://www.lmdb.tech/doc/group__mdb.html>.
defaultLMDBLimits :: LMDB.LMDBLimits
defaultLMDBLimits = LMDB.LMDBLimits {
    LMDB.lmdbMapSize = 16 * 1024 * 1024 * 1024
  , LMDB.lmdbMaxDatabases = 10
  , LMDB.lmdbMaxReaders = 16
  }

defaultLMDBPath :: FilePath -> FilePath
defaultLMDBPath = (</> "lmdb")

selectorToArgs :: forall blk. (LedgerSupportsProtocol blk, LedgerSupportsLedgerDB blk) => LedgerDbSelectorFlag -> FilePath -> StdGen -> (LedgerDbBackendArgs IO blk, StdGen)
selectorToArgs V2InMemory _ = InMemory.mkInMemoryArgs
selectorToArgs (V1LMDB ff fp l mxReaders) fastStoragePath =
    LMDB.mkLMDBArgs
        ff
        (fromMaybe (defaultLMDBPath fastStoragePath) fp)
        ( maybe id (\overrideMaxReaders lim -> lim{LMDB.lmdbMaxReaders = overrideMaxReaders}) mxReaders $
            maybe id (\ll lim -> lim{LMDB.lmdbMapSize = toBytes ll}) l defaultLMDBLimits
        )
selectorToArgs (V2LSM fp) fastStoragePath = LSM.mkLSMArgs (Proxy @blk) (fromMaybe "lsm" fp) fastStoragePath
