{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Blocks in the blockchain
--
module Cardano.Api.Block (

    -- * Blocks in the context of an era
    Block(..),
    BlockHeader,

    -- ** Blocks in the context of a consensus mode
    BlockInMode(..),
    fromConsensusBlock,

    -- * Points on the chain
    ChainPoint(..),
    SlotNo,
    EpochNo,
    toConsensusPoint,
    fromConsensusPoint,
    toConsensusPointInMode,
    fromConsensusPointInMode,

    -- * Tip of the chain
    ChainTip(..),
    BlockNo,
    chainTipToChainPoint,
    fromConsensusTip,

    -- * Data family instances
    Hash(..),
  ) where

import           Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo, EpochNo)

import qualified Ouroboros.Network.Block as Consensus

import qualified Ouroboros.Consensus.Block               as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.Byron.Ledger        as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger      as Consensus
import qualified Ouroboros.Consensus.Cardano.Block       as Consensus
import qualified Ouroboros.Consensus.Cardano.ByronHFC    as Consensus
import qualified Ouroboros.Consensus.Cardano.ShelleyHFC  as Consensus


import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.Modes
import           Cardano.Api.SerialiseRaw

{-# ANN module ("HLint: ignore Use lambda" :: String) #-}


-- ----------------------------------------------------------------------------
-- Blocks in an era
--

-- | A blockchain block in a particular Cardano era.
--
data Block era where

     ByronBlock :: Consensus.ByronBlock
                -> Block ByronEra

     ShelleyBlock :: ShelleyBasedEra era
                  -> Consensus.ShelleyBlock (ShelleyLedgerEra era)
                  -> Block era

-- The GADT in the ShelleyBlock case requires a custom instance
instance Show (Block era) where
    showsPrec p (ByronBlock block) =
      showParen (p >= 11)
        ( showString "ByronBlock "
        . showsPrec 11 block
        )

    showsPrec p (ShelleyBlock ShelleyBasedEraShelley block) =
      showParen (p >= 11)
        ( showString "ShelleyBlock ShelleyBasedEraShelley "
        . showsPrec 11 block
        )

    showsPrec p (ShelleyBlock ShelleyBasedEraAllegra block) =
      showParen (p >= 11)
        ( showString "ShelleyBlock ShelleyBasedEraAllegra "
        . showsPrec 11 block
        )

    showsPrec p (ShelleyBlock ShelleyBasedEraMary block) =
      showParen (p >= 11)
        ( showString "ShelleyBlock ShelleyBasedEraMary "
        . showsPrec 11 block
        )


-- ----------------------------------------------------------------------------
-- Block in a consensus mode
--

-- | A 'Block' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different block types for all the eras. It is used in the ChainSync protocol.
--
data BlockInMode mode where
     BlockInMode :: Block era -> EraInMode era mode -> BlockInMode mode

deriving instance Show (BlockInMode mode)


fromConsensusBlock :: ConsensusBlockForMode mode ~ block
                   => ConsensusMode mode -> block -> BlockInMode mode
fromConsensusBlock ByronMode =
    \b -> case b of
      Consensus.DegenBlock b' ->
        BlockInMode (ByronBlock b') ByronEraInByronMode

fromConsensusBlock ShelleyMode =
    \b -> case b of
      Consensus.DegenBlock b' ->
        BlockInMode (ShelleyBlock ShelleyBasedEraShelley b')
                     ShelleyEraInShelleyMode

fromConsensusBlock CardanoMode =
    \b -> case b of
      Consensus.BlockByron b' ->
        BlockInMode (ByronBlock b') ByronEraInCardanoMode

      Consensus.BlockShelley b' ->
        BlockInMode (ShelleyBlock ShelleyBasedEraShelley b')
                     ShelleyEraInCardanoMode

      Consensus.BlockAllegra b' ->
        BlockInMode (ShelleyBlock ShelleyBasedEraAllegra b')
                     AllegraEraInCardanoMode

      Consensus.BlockMary b' ->
        BlockInMode (ShelleyBlock ShelleyBasedEraMary b')
                     MaryEraInCardanoMode


-- ----------------------------------------------------------------------------
-- Block headers
--

data BlockHeader

-- | For now at least we use a fixed concrete hash type for all modes and era.
-- The different eras do use different types, but it's all the same underlying
-- representation.
newtype instance Hash BlockHeader = HeaderHash SBS.ShortByteString
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash BlockHeader) where
    serialiseToRawBytes (HeaderHash bs) = SBS.fromShort bs

    deserialiseFromRawBytes (AsHash AsBlockHeader) bs
      | BS.length bs == 32 = Just $! HeaderHash (SBS.toShort bs)
      | otherwise          = Nothing

instance HasTypeProxy BlockHeader where
    data AsType BlockHeader = AsBlockHeader
    proxyToAsType _ = AsBlockHeader


-- ----------------------------------------------------------------------------
-- Chain points
--

data ChainPoint = ChainPointAtGenesis
                | ChainPoint !SlotNo !(Hash BlockHeader)
  deriving (Eq, Show)


toConsensusPointInMode :: ConsensusMode mode
                       -> ChainPoint
                       -> Consensus.Point (ConsensusBlockForMode mode)
-- It's the same concrete impl in all cases, but we have to show
-- individually for each case that we satisfy the type equality constraint
-- HeaderHash block ~ OneEraHash xs
toConsensusPointInMode ByronMode   = toConsensusPointHF
toConsensusPointInMode ShelleyMode = toConsensusPointHF
toConsensusPointInMode CardanoMode = toConsensusPointHF

fromConsensusPointInMode :: ConsensusMode mode
                         -> Consensus.Point (ConsensusBlockForMode mode)
                         -> ChainPoint
fromConsensusPointInMode ByronMode   = fromConsensusPointHF
fromConsensusPointInMode ShelleyMode = fromConsensusPointHF
fromConsensusPointInMode CardanoMode = fromConsensusPointHF


-- | Convert a 'Consensus.Point' for multi-era block type
--
toConsensusPointHF :: Consensus.HeaderHash block ~ Consensus.OneEraHash xs
                   => ChainPoint -> Consensus.Point block
toConsensusPointHF  ChainPointAtGenesis = Consensus.GenesisPoint
toConsensusPointHF (ChainPoint slot (HeaderHash h)) =
    Consensus.BlockPoint slot (Consensus.OneEraHash h)

-- | Convert a 'Consensus.Point' for multi-era block type
--
fromConsensusPointHF :: Consensus.HeaderHash block ~ Consensus.OneEraHash xs
                   => Consensus.Point block -> ChainPoint
fromConsensusPointHF Consensus.GenesisPoint = ChainPointAtGenesis
fromConsensusPointHF (Consensus.BlockPoint slot (Consensus.OneEraHash h)) =
    ChainPoint slot (HeaderHash h)

-- | Convert a 'Consensus.Point' for single Shelley-era block type
--
toConsensusPoint :: forall ledgerera.
                      Consensus.ShelleyBasedEra ledgerera
                   => ChainPoint
                   -> Consensus.Point (Consensus.ShelleyBlock ledgerera)
toConsensusPoint ChainPointAtGenesis = Consensus.GenesisPoint
toConsensusPoint (ChainPoint slot (HeaderHash h)) =
    Consensus.BlockPoint slot (Consensus.fromShortRawHash proxy h)
  where
    proxy :: Proxy (Consensus.ShelleyBlock ledgerera)
    proxy = Proxy

-- | Convert a 'Consensus.Point' for single Shelley-era block type
--
fromConsensusPoint :: forall ledgerera.
                      Consensus.ShelleyBasedEra ledgerera
                   => Consensus.Point (Consensus.ShelleyBlock ledgerera)
                   -> ChainPoint
fromConsensusPoint Consensus.GenesisPoint = ChainPointAtGenesis
fromConsensusPoint (Consensus.BlockPoint slot h) =
    ChainPoint slot (HeaderHash (Consensus.toShortRawHash proxy h))
  where
    proxy :: Proxy (Consensus.ShelleyBlock ledgerera)
    proxy = Proxy


-- ----------------------------------------------------------------------------
-- Chain tips
--

-- | This is like a 'ChainPoint' but is conventionally used for the tip of the
-- chain: that is the most recent block at the end of the chain.
--
-- It also carries the 'BlockNo' of the chain tip.
--
data ChainTip = ChainTipAtGenesis
              | ChainTip !SlotNo !(Hash BlockHeader) !BlockNo
  deriving (Eq, Show)

chainTipToChainPoint :: ChainTip -> ChainPoint
chainTipToChainPoint ChainTipAtGenesis = ChainPointAtGenesis
chainTipToChainPoint (ChainTip s h _)  = ChainPoint s h


fromConsensusTip  :: ConsensusBlockForMode mode ~ block
                  => ConsensusMode mode
                  -> Consensus.Tip block
                  -> ChainTip
fromConsensusTip ByronMode = conv
  where
    conv :: Consensus.Tip Consensus.ByronBlockHFC -> ChainTip
    conv Consensus.TipGenesis = ChainTipAtGenesis
    conv (Consensus.Tip slot (Consensus.OneEraHash h) block) =
      ChainTip slot (HeaderHash h) block

fromConsensusTip ShelleyMode = conv
  where
    conv :: Consensus.Tip (Consensus.ShelleyBlockHFC Consensus.StandardShelley)
         -> ChainTip
    conv Consensus.TipGenesis = ChainTipAtGenesis
    conv (Consensus.Tip slot (Consensus.OneEraHash h) block) =
      ChainTip slot (HeaderHash h) block

fromConsensusTip CardanoMode = conv
  where
    conv :: Consensus.Tip (Consensus.CardanoBlock Consensus.StandardCrypto)
         -> ChainTip
    conv Consensus.TipGenesis = ChainTipAtGenesis
    conv (Consensus.Tip slot (Consensus.OneEraHash h) block) =
      ChainTip slot (HeaderHash h) block

{-
TODO: In principle we should be able to use this common implementation rather
      than repeating it for each mode above. It does actually type-check. The
      problem is that (at least with ghc-8.10.x) ghc's pattern match warning
      mechanism cannot see that the OneEraHash is a complete pattern match.
      I'm guessing that while the type checker can use the type equality to
      see that OneEraHash is a valid pattern, the exhaustiveness checker is for
      some reason not able to use it to see that it is indeed the only pattern.
fromConsensusTip =
    \mode -> case mode of
      ByronMode   -> conv
      ShelleyMode -> conv
      CardanoMode -> conv
  where
    conv :: HeaderHash block ~ OneEraHash xs
         => Tip block -> ChainTip
    conv TipGenesis                      = ChainTipAtGenesis
    conv (Tip slot (OneEraHash h) block) = ChainTip slot (HeaderHash h) block
-}

