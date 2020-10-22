{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.HasIssuer
  ( BlockIssuerVerificationKeyHash (..)
  , HasIssuer (..)
  ) where

import           Cardano.Prelude hiding (All)

import           Data.SOP.Strict

import           Cardano.Api.Typed (StandardAllegra, StandardMary, StandardShelley,
                     VerificationKey (..), serialiseToRawBytes, verificationKeyHash)
import qualified Cardano.Chain.Block as Byron
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock, Header (..))
import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock, Header (..),
                     OneEraHeader (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (Header (..), ShelleyBlock)
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley

-- | Block issuer verification key hash.
data BlockIssuerVerificationKeyHash
  = BlockIssuerVerificationKeyHash !ByteString
  -- ^ Serialized block issuer verification key hash.
  | NoBlockIssuer
  -- ^ There is no block issuer.
  --
  -- For example, this could be relevant for epoch boundary blocks (EBBs),
  -- genesis blocks, etc.
  deriving (Eq, Show)

-- | Get the block issuer verification key hash from a block header.
class HasIssuer blk where
  -- | Given a block header, return the serialized block issuer verification
  -- key hash.
  getIssuerVerificationKeyHash :: Header blk -> BlockIssuerVerificationKeyHash

instance HasIssuer ByronBlock where
  getIssuerVerificationKeyHash byronBlkHdr =
    case byronHeaderRaw byronBlkHdr of
      Byron.ABOBBlockHdr hdr ->
        BlockIssuerVerificationKeyHash
          . serialiseToRawBytes
          . verificationKeyHash
          . ByronVerificationKey
          $ Byron.headerIssuer hdr
      Byron.ABOBBoundaryHdr _ -> NoBlockIssuer

instance HasIssuer (ShelleyBlock StandardShelley) where
  getIssuerVerificationKeyHash shelleyBlkHdr =
      BlockIssuerVerificationKeyHash
        . serialiseToRawBytes
        . verificationKeyHash
        . StakePoolVerificationKey
        . toStakePoolKey
        $ Shelley.bheaderVk bhBody
    where
      -- We don't support a "block issuer" key role in @cardano-api@, so we'll
      -- just convert it to a stake pool key.
      toStakePoolKey
        :: Shelley.VKey 'Shelley.BlockIssuer era
        -> Shelley.VKey 'Shelley.StakePool era
      toStakePoolKey vk = Shelley.VKey (Shelley.unVKey vk)

      Shelley.BHeader bhBody _ = shelleyHeaderRaw shelleyBlkHdr

instance HasIssuer (ShelleyBlock StandardAllegra) where
  getIssuerVerificationKeyHash shelleyBlkHdr =
      BlockIssuerVerificationKeyHash
        . serialiseToRawBytes
        . verificationKeyHash
        . StakePoolVerificationKey
        . toStakePoolKey
        $ Shelley.bheaderVk bhBody
    where
      -- We don't support a "block issuer" key role in @cardano-api@, so we'll
      -- just convert it to a stake pool key.
      toStakePoolKey
        :: Shelley.VKey 'Shelley.BlockIssuer era
        -> Shelley.VKey 'Shelley.StakePool era
      toStakePoolKey vk = Shelley.VKey (Shelley.unVKey vk)

      Shelley.BHeader bhBody _ = shelleyHeaderRaw shelleyBlkHdr

instance HasIssuer (ShelleyBlock StandardMary) where
  getIssuerVerificationKeyHash shelleyBlkHdr =
      BlockIssuerVerificationKeyHash
        . serialiseToRawBytes
        . verificationKeyHash
        . StakePoolVerificationKey
        . toStakePoolKey
        $ Shelley.bheaderVk bhBody
    where
      -- We don't support a "block issuer" key role in @cardano-api@, so we'll
      -- just convert it to a stake pool key.
      toStakePoolKey
        :: Shelley.VKey 'Shelley.BlockIssuer era
        -> Shelley.VKey 'Shelley.StakePool era
      toStakePoolKey vk = Shelley.VKey (Shelley.unVKey vk)

      Shelley.BHeader bhBody _ = shelleyHeaderRaw shelleyBlkHdr

instance All HasIssuer xs => HasIssuer (HardForkBlock xs) where
  getIssuerVerificationKeyHash =
    hcollapse
      . hcmap (Proxy @ HasIssuer) (K . getIssuerVerificationKeyHash)
      . getOneEraHeader
      . getHardForkHeader
