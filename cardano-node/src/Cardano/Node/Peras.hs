{-# LANGUAGE KindSignatures #-}

-- | Temporary node-owned adapter for Peras vote-forging installation.
module Cardano.Node.Peras
  ( PerasVoteForging (..)
  , mkPerasVoteForging
  , setPerasVoteForging
  , PerasVoteForgingCredentialLoadError (..)
  , MkPerasVoteForging (..)
  , temporaryPerasPrivateKey
  , poolIdFromShelleyLeaderCredentials
  ) where

import           Cardano.Api (StandardCrypto)
import           Cardano.Ledger.Keys (coerceKeyRole, hashKey)
import           Control.Concurrent.Class.MonadSTM.Strict (StrictTVar, atomically, writeTVar)
import           Data.Kind (Type)
import qualified Data.ByteString as BS
import           Ouroboros.Consensus.Committee.Types (PoolId (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import           Ouroboros.Consensus.Peras.Crypto.BLS (PerasPrivateKey (..))
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosCanBeLeader (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials (..))

-- Temporary
data PerasVoteForging (m :: Type -> Type) blk =
  PerasVoteForging
  { perasVoteForgingPoolId :: PoolId
  , perasVoteForgingPrivateKey :: PerasPrivateKey
  }

-- Temporary
mkPerasVoteForging :: MkPerasVoteForging -> PerasVoteForging m blk
mkPerasVoteForging mkVoteForging =
  PerasVoteForging
    { perasVoteForgingPoolId = mkPerasVoteForgingPoolId mkVoteForging
    , perasVoteForgingPrivateKey = mkPerasVoteForgingPrivateKey mkVoteForging
    }

-- | The Shelley identity and the independent Peras signing key required to
-- build a vote forger.
data MkPerasVoteForging = MkPerasVoteForging
  { mkPerasVoteForgingPoolId :: PoolId
  , mkPerasVoteForgingPrivateKey :: PerasPrivateKey
  }

-- | Use the cold stake-pool verification key, never the KES or VRF key. Build
-- the consensus PoolId directly from the ledger stake-pool key hash.
poolIdFromShelleyLeaderCredentials
  :: ShelleyLeaderCredentials StandardCrypto
  -> PoolId
poolIdFromShelleyLeaderCredentials =
  PoolId
    . hashKey
    . coerceKeyRole
    . praosCanBeLeaderColdVerKey
    . shelleyLeaderCredentialsCanBeLeader

-- | TEMPORARY NON-PRODUCTION KEY. There is no node configuration path for a
-- Peras BLS key yet. This uses a fixed, valid scalar parsed by consensus's
-- BLS deserializer; replace this provider with configured key loading before
-- enabling production Peras forging.
temporaryPerasPrivateKey :: PerasPrivateKey
temporaryPerasPrivateKey =
  maybe (error "temporary Peras BLS key failed consensus deserialization")
        PerasPrivateKey
        (BLS.rawDeserialisePrivateKey "cardano-node-temporary-peras-key" (BS.replicate 32 1))

setPerasVoteForging
  :: nodeKernel
  -> StrictTVar IO (Maybe MkPerasVoteForging)
  -> Maybe MkPerasVoteForging
  -> IO ()
setPerasVoteForging _nodeKernel perasVoteForgingVar maybeForger =
  atomically $ writeTVar perasVoteForgingVar maybeForger

data PerasVoteForgingCredentialLoadError
  = PerasVoteForgingCredentialsRemoved
  | PerasVoteForgingCredentialsLoadFailed String
  deriving (Eq, Show)

