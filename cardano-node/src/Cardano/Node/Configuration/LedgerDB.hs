{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Configuration.LedgerDB (
    DeprecatedOptions (..)
  , LedgerDbConfiguration (..)
  , LedgerDbSelectorFlag(..)
  , Gigabytes
  , noDeprecatedOptions
  , selectorToArgs
  ) where

import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB (LMDBLimits (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import           Ouroboros.Consensus.Util.Args

import qualified Data.Aeson.Types as Aeson (FromJSON)
import           Data.Maybe (fromMaybe)
import           Data.SOP.Dict

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
-- - 'V1InMemory': Not intended for production. It is an in-memory reproduction
--   of the LMDB implementation.
data LedgerDbSelectorFlag =
    V1LMDB
      V1.FlushFrequency
      -- ^ The frequency at which changes are flushed to the disk.
      (Maybe FilePath)
      -- ^ Path for the live tables.
      (Maybe Gigabytes)
      -- ^ A map size can be specified, this is the maximum disk space the LMDB
      -- database can fill. If not provided, the default of 16GB will be used.
      (Maybe Int)
      -- ^ An override to the max number of readers.
  | V1InMemory V1.FlushFrequency
  | V2InMemory
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
defaultLMDBLimits :: LMDBLimits
defaultLMDBLimits = LMDBLimits {
    lmdbMapSize = 16 * 1024 * 1024 * 1024
  , lmdbMaxDatabases = 10
  , lmdbMaxReaders = 16
  }

defaultLMDBPath :: FilePath
defaultLMDBPath = "mainnet/db/lmdb"

selectorToArgs :: LedgerDbSelectorFlag -> Complete LedgerDbFlavorArgs IO
selectorToArgs (V1InMemory ff)  = LedgerDbFlavorArgsV1 $ V1.V1Args ff V1.InMemoryBackingStoreArgs
selectorToArgs V2InMemory      = LedgerDbFlavorArgsV2 $ V2.V2Args V2.InMemoryHandleArgs
selectorToArgs (V1LMDB ff fp l mxReaders) =
    LedgerDbFlavorArgsV1
  $ V1.V1Args ff
  $ V1.LMDBBackingStoreArgs
      (fromMaybe defaultLMDBPath fp)
      (maybe id (\overrideMaxReaders lim -> lim { lmdbMaxReaders = overrideMaxReaders }) mxReaders
       $ maybe id (\ll lim -> lim { lmdbMapSize = toBytes ll }) l defaultLMDBLimits)
      Dict
