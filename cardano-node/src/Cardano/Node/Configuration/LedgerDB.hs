module Cardano.Node.Configuration.LedgerDB (
    BackingStoreSelectorFlag(..)
  ) where

import           Prelude

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
    LMDB (Maybe Int) -- ^ A map size can be specified, this is the maximum disk
                     -- space the LMDB database can fill. If not provided, the
                     -- default of 12Gi will be used.
  | InMemory
  deriving (Eq, Show)
