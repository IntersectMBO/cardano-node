{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Configuration.LedgerDB (
    BackingStoreSelectorFlag(..)
  , Gigabytes
  , toBytes
  , defaultLMDBLimits
  ) where

import           Prelude

import qualified Data.Aeson.Types as Aeson (FromJSON)
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB (LMDBLimits (..))

-- | Choose the LedgerDB Backend
--
-- As of UTxO-HD, the LedgerDB now uses either an in-memory backend or LMDB to
-- keep track of differences in the UTxO set.
--
-- - 'InMemory': uses more memory than the minimum requirements but is somewhat
--   faster.
-- - 'LMDB': uses less memory but is somewhat slower.
--
-- See 'Ouroboros.Consnesus.Storage.LedgerDB.OnDisk.BackingStoreSelector'.
data BackingStoreSelectorFlag =
    LMDB (Maybe Gigabytes) -- ^ A map size can be specified, this is the maximum
                          -- disk space the LMDB database can fill. If not
                          -- provided, the default of 16GB will be used.
  | InMemory
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
