{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Blocks in the blockchain
--
module Cardano.Api.Block (

    -- * Blocks in the context of an era
    Block(.., Block),
    BlockHeader(..),
    blockToAnyCardanoEra,
    getBlockHeader,
    getBlockHeaderAndTxs,
    getBlockIssuer,
    getBlockOpCertCounter,
    getBlockOpCertKesHotKey,
    getBlockProtocolVersion,
    getBlockSize,
    getBlockTxs,
    getBlockVrfVerificationKey,
    previousBlockHeaderHash,

    -- ** Blocks in the context of a consensus mode
    BlockInMode(..),
    fromConsensusBlock,
    toConsensusBlock,

    -- * Points on the chain
    ChainPoint(..),
    SlotNo(..),
    EpochNo(..),
    toConsensusPoint,
    fromConsensusPoint,
    toConsensusPointInMode,
    fromConsensusPointInMode,
    toConsensusPointHF,

    -- * Tip of the chain
    ChainTip(..),
    BlockNo(..),
    chainTipToChainPoint,
    fromConsensusTip,

    -- * Data family instances
    Hash(..),

    chainPointToHeaderHash,
    chainPointToSlotNo,
    makeChainTip,
  ) where

import           Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import           Data.Foldable (Foldable (toList))
import           Data.Word

import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (EpochNo, SlotNo, WithOrigin (..))

import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing
import qualified Ouroboros.Consensus.Block as Consensus
import qualified Ouroboros.Consensus.Block as ConsensusBlock
import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.ByronHFC as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import qualified Ouroboros.Consensus.Shelley.ShelleyHFC as Consensus
import qualified Ouroboros.Network.Block as Consensus

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Serialization as Ledger
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.BlockChain as SL
import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import qualified Cardano.Api.KeysPraos as ApiPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.Tx

{- HLINT ignore "Use lambda" -}
{- HLINT ignore "Use lambda-case" -}

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

-- | A block consists of a header and a body containing transactions.
--
pattern Block :: BlockHeader -> [Tx era] -> Block era
pattern Block header txs <- (getBlockHeaderAndTxs -> (header, txs))

{-# COMPLETE Block #-}

getBlockHeaderAndTxs :: Block era -> (BlockHeader, [Tx era])
getBlockHeaderAndTxs block = (getBlockHeader block, getBlockTxs block)

blockToAnyCardanoEra :: Block era -> AnyCardanoEra
blockToAnyCardanoEra ByronBlock{} = AnyCardanoEra ByronEra
blockToAnyCardanoEra (ShelleyBlock sbe _) =
   obtainIsCardanoEra sbe $ AnyCardanoEra $ shelleyBasedToCardanoEra sbe
  where
   obtainIsCardanoEra
     :: ShelleyBasedEra era -> (IsCardanoEra era => a) -> a
   obtainIsCardanoEra ShelleyBasedEraShelley f = f
   obtainIsCardanoEra ShelleyBasedEraAllegra f = f
   obtainIsCardanoEra ShelleyBasedEraMary f = f
   obtainIsCardanoEra ShelleyBasedEraAlonzo f = f

previousBlockHeaderHash :: forall era. Block era -> Maybe (Hash BlockHeader)
previousBlockHeaderHash (ByronBlock blk) =
  case ConsensusBlock.blockPrevHash blk of
    ConsensusBlock.GenesisHash -> Nothing
    ConsensusBlock.BlockHash headerHash ->
      Just . HeaderHash
        $ ConsensusBlock.toShortRawHash (Proxy @Consensus.ByronBlock) headerHash

previousBlockHeaderHash b@(ShelleyBlock sbe _) =
  obtainConsensusShelleyBasedEra sbe $ shelleyBlockPrevHash sbe b
 where
  shelleyBlockPrevHash
    :: Consensus.ShelleyBasedEra (ShelleyLedgerEra era)
    => ShelleyBasedEra era
    -> Block era
    -> Maybe (Hash BlockHeader)
  shelleyBlockPrevHash _ (ShelleyBlock _ blk) =
     case ConsensusBlock.blockPrevHash blk of
      ConsensusBlock.GenesisHash -> Nothing
      ConsensusBlock.BlockHash headerHash ->
        Just . HeaderHash
          $ ConsensusBlock.toShortRawHash
              (Proxy @(Consensus.ShelleyBlock (ShelleyLedgerEra era)))
              headerHash

-- TODO: Update to handle Byron era blocks
getBlockIssuer :: ShelleyBasedEra era -> Block era -> Hash StakePoolKey
getBlockIssuer sbe' b = obtainBlockConstraints sbe' $ getBlockIssuer' sbe' b
 where
  getBlockIssuer'
    :: Ledger.Era (ShelleyLedgerEra era)
    => Ledger.Crypto (ShelleyLedgerEra era) ~ Consensus.StandardCrypto
    => Ledger.ToCBORGroup (Ledger.TxSeq (ShelleyLedgerEra era))
    => ShelleyBasedEra era -> Block era -> Hash StakePoolKey
  getBlockIssuer' _ (ShelleyBlock _ blk) =
      let sBlockRaw = Consensus.shelleyBlockRaw blk
          SL.Block bHeader _ = sBlockRaw
          TPraos.BHeader bhBody _ = bHeader
          stakePoolKeyHash = TPraos.issuerIDfromBHBody bhBody
      in StakePoolKeyHash . coerce $ stakePoolKeyHash

obtainBlockConstraints
  :: ShelleyBasedEra era
  -> (  Ledger.Era (ShelleyLedgerEra era)
     => Ledger.Crypto (ShelleyLedgerEra era) ~ Consensus.StandardCrypto
     => Ledger.ToCBORGroup (Ledger.TxSeq (ShelleyLedgerEra era))
     => a)
  -> a
obtainBlockConstraints ShelleyBasedEraShelley f = f
obtainBlockConstraints ShelleyBasedEraAllegra f = f
obtainBlockConstraints ShelleyBasedEraMary  f = f
obtainBlockConstraints ShelleyBasedEraAlonzo f = f

getBlockOpCertKesHotKey
  :: ShelleyBasedEra era -> Block era -> VerificationKey ApiPraos.KesKey
getBlockOpCertKesHotKey sbe (ShelleyBlock _ b) =
  obtainBlockConstraints sbe $ getBlockOperationalCertificate' b
 where
  getBlockOperationalCertificate' b' =
    let sBlockRaw = Consensus.shelleyBlockRaw b'
        SL.Block bHeader _ = sBlockRaw
        TPraos.BHeader bhBody _ = bHeader
        oCert = SL.bheaderOCert bhBody
    in ApiPraos.KesVerificationKey $ SL.ocertVkHot oCert

getBlockOpCertCounter
  :: ShelleyBasedEra era -> Block era -> Word64
getBlockOpCertCounter sbe (ShelleyBlock _ b) =
  obtainBlockConstraints sbe $ getBlockOperationalCertificate' b
 where
  getBlockOperationalCertificate' b' =
    let sBlockRaw = Consensus.shelleyBlockRaw b'
        SL.Block bHeader _ = sBlockRaw
        TPraos.BHeader bhBody _ = bHeader
        oCert = SL.bheaderOCert bhBody
    in SL.ocertN oCert


getBlockProtocolVersion :: ShelleyBasedEra era -> Block era -> SL.ProtVer
getBlockProtocolVersion sbe (ShelleyBlock _ blk) =
  obtainBlockConstraints sbe $ getBlockProtocolVersion' blk
 where
  getBlockProtocolVersion' blk' =
    let sBlockRaw = Consensus.shelleyBlockRaw blk'
        SL.Block bHeader _ = sBlockRaw
        TPraos.BHeader bhBody _ = bHeader
    in SL.bprotver bhBody

getBlockSize :: Block era -> Word64
getBlockSize (ByronBlock blk) =
  case Consensus.byronBlockRaw blk of
    Byron.ABOBBlock bBlk ->
      fromIntegral $ Byron.blockLength bBlk
    Byron.ABOBBoundary bBoundaryBlk ->
      fromIntegral $ Byron.boundaryBlockLength bBoundaryBlk
getBlockSize (ShelleyBlock sbe blk) =
  obtainBlockConstraints sbe $ getBlockSize' blk
 where
  getBlockSize' b' =
    let SL.Block _ txs = Consensus.shelleyBlockRaw b'
    in fromIntegral $ SL.bBodySize txs



getBlockVrfVerificationKey
  :: ShelleyBasedEra era -> Block era -> VerificationKey ApiPraos.VrfKey
getBlockVrfVerificationKey sbe (ShelleyBlock _ b) =
  obtainBlockConstraints sbe $ getBlockVrfVerificationKey' b
  where
    getBlockVrfVerificationKey' blk' =
       let sBlockRaw = Consensus.shelleyBlockRaw blk'
           SL.Block bHeader _ = sBlockRaw
           TPraos.BHeader bhBody _ = bHeader
       in ApiPraos.VrfVerificationKey $ SL.bheaderVrfVk bhBody


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

    showsPrec p (ShelleyBlock ShelleyBasedEraAlonzo block) =
      showParen (p >= 11)
        ( showString "ShelleyBlock ShelleyBasedEraAlonzo "
        . showsPrec 11 block
        )

getBlockTxs :: Block era -> [Tx era]
getBlockTxs (ByronBlock Consensus.ByronBlock { Consensus.byronBlockRaw }) =
    case byronBlockRaw of
      Byron.ABOBBoundary{} -> [] -- no txs in EBBs
      Byron.ABOBBlock Byron.ABlock {
          Byron.blockBody =
            Byron.ABody {
              Byron.bodyTxPayload = Byron.ATxPayload txs
            }
        } -> map ByronTx txs
getBlockTxs (ShelleyBlock era Consensus.ShelleyBlock{Consensus.shelleyBlockRaw}) =
    obtainConsensusShelleyBasedEra era $
      getShelleyBlockTxs era shelleyBlockRaw

getShelleyBlockTxs :: forall era ledgerera.
                      ledgerera ~ ShelleyLedgerEra era
                   => Consensus.ShelleyBasedEra ledgerera
                   => ShelleyBasedEra era
                   -> Ledger.Block (TPraos.BHeader (Ledger.Crypto ledgerera)) ledgerera
                   -> [Tx era]
getShelleyBlockTxs era (Ledger.Block _header txs) =
  [ ShelleyTx era txinblock
  | txinblock <- toList (Ledger.fromTxSeq txs) ]

obtainConsensusShelleyBasedEra
  :: forall era ledgerera a.
     ledgerera ~ ShelleyLedgerEra era
  => ShelleyBasedEra era
  -> (Consensus.ShelleyBasedEra ledgerera => a) -> a
obtainConsensusShelleyBasedEra ShelleyBasedEraShelley f = f
obtainConsensusShelleyBasedEra ShelleyBasedEraAllegra f = f
obtainConsensusShelleyBasedEra ShelleyBasedEraMary    f = f
obtainConsensusShelleyBasedEra ShelleyBasedEraAlonzo  f = f


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

      Consensus.BlockAlonzo b' ->
        BlockInMode (ShelleyBlock ShelleyBasedEraAlonzo b')
                     AlonzoEraInCardanoMode

toConsensusBlock :: ConsensusBlockForMode mode ~ block => BlockInMode mode -> block
toConsensusBlock bInMode =
  case bInMode of
    -- Byron mode
    BlockInMode (ByronBlock b') ByronEraInByronMode -> Consensus.DegenBlock b'

    -- Shelley mode
    BlockInMode (ShelleyBlock ShelleyBasedEraShelley b') ShelleyEraInShelleyMode -> Consensus.DegenBlock b'

    -- Cardano mode
    BlockInMode (ByronBlock b') ByronEraInCardanoMode -> Consensus.BlockByron b'
    BlockInMode (ShelleyBlock ShelleyBasedEraShelley b') ShelleyEraInCardanoMode -> Consensus.BlockShelley b'
    BlockInMode (ShelleyBlock ShelleyBasedEraAllegra b') AllegraEraInCardanoMode -> Consensus.BlockAllegra b'
    BlockInMode (ShelleyBlock ShelleyBasedEraMary b') MaryEraInCardanoMode -> Consensus.BlockMary b'
    BlockInMode (ShelleyBlock ShelleyBasedEraAlonzo b') AlonzoEraInCardanoMode -> Consensus.BlockAlonzo b'

-- ----------------------------------------------------------------------------
-- Block headers
--

data BlockHeader = BlockHeader !SlotNo
                               !(Hash BlockHeader)
                               !BlockNo

-- | For now at least we use a fixed concrete hash type for all modes and era.
-- The different eras do use different types, but it's all the same underlying
-- representation.
newtype instance Hash BlockHeader = HeaderHash SBS.ShortByteString
  deriving (Eq, Ord, Show)
  deriving (ToJSON, FromJSON) via UsingRawBytesHex (Hash BlockHeader)



instance SerialiseAsRawBytes (Hash BlockHeader) where
    serialiseToRawBytes (HeaderHash bs) = SBS.fromShort bs

    deserialiseFromRawBytes (AsHash AsBlockHeader) bs
      | BS.length bs == 32 = Just $! HeaderHash (SBS.toShort bs)
      | otherwise          = Nothing

instance HasTypeProxy BlockHeader where
    data AsType BlockHeader = AsBlockHeader
    proxyToAsType _ = AsBlockHeader

getBlockHeader :: forall era . Block era -> BlockHeader
getBlockHeader (ShelleyBlock shelleyEra block) = case shelleyEra of
  ShelleyBasedEraShelley -> go
  ShelleyBasedEraAllegra -> go
  ShelleyBasedEraMary -> go
  ShelleyBasedEraAlonzo -> go
  where
    go :: Consensus.ShelleyBasedEra (ShelleyLedgerEra era) => BlockHeader
    go = BlockHeader headerFieldSlot (HeaderHash hashSBS) headerFieldBlockNo
      where
        Consensus.HeaderFields {
            Consensus.headerFieldHash
              = Consensus.ShelleyHash (TPraos.HashHeader (Cardano.Crypto.Hash.Class.UnsafeHash hashSBS)),
            Consensus.headerFieldSlot,
            Consensus.headerFieldBlockNo
          } = Consensus.getHeaderFields block
getBlockHeader (ByronBlock block)
  = BlockHeader
      headerFieldSlot
      (HeaderHash $ Cardano.Crypto.Hashing.abstractHashToShort byronHeaderHash)
      headerFieldBlockNo
  where
    Consensus.HeaderFields {
      Consensus.headerFieldHash = Consensus.ByronHash byronHeaderHash,
      Consensus.headerFieldSlot,
      Consensus.headerFieldBlockNo
    } = Consensus.getHeaderFields block


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

chainPointToSlotNo :: ChainPoint -> Maybe SlotNo
chainPointToSlotNo ChainPointAtGenesis = Nothing
chainPointToSlotNo (ChainPoint slotNo _) = Just slotNo

chainPointToHeaderHash :: ChainPoint -> Maybe (Hash BlockHeader)
chainPointToHeaderHash ChainPointAtGenesis = Nothing
chainPointToHeaderHash (ChainPoint _ blockHeader) = Just blockHeader


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

instance ToJSON ChainTip where
  toJSON ChainTipAtGenesis = Aeson.Null
  toJSON (ChainTip slot headerHash (Consensus.BlockNo bNum)) =
    object [ "slot" .= slot
           , "hash" .= serialiseToRawBytesHexText headerHash
           , "block" .= bNum
           ]

chainTipToChainPoint :: ChainTip -> ChainPoint
chainTipToChainPoint ChainTipAtGenesis = ChainPointAtGenesis
chainTipToChainPoint (ChainTip s h _)  = ChainPoint s h

makeChainTip :: WithOrigin BlockNo -> ChainPoint -> ChainTip
makeChainTip woBlockNo chainPoint = case woBlockNo of
  Origin -> ChainTipAtGenesis
  At blockNo -> case chainPoint of
    ChainPointAtGenesis -> ChainTipAtGenesis
    ChainPoint slotNo headerHash -> ChainTip slotNo headerHash blockNo

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
