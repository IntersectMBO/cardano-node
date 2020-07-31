{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tracing.Render
  ( renderHeaderHash
  , renderHeaderHashForVerbosity
  , renderPoint
  , renderPointForVerbosity
  , renderRealPoint
  , renderSlotNo
  , renderTip
  , renderTipForVerbosity
  , renderTxId
  , renderTxIdForVerbosity
  , renderWithOrigin
  ) where

import           Cardano.Prelude
import           Prelude (id)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.BM.Tracing (TracingVerbosity (..))
import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import           Cardano.Tracing.ConvertTxId (ConvertTxId (..))
import           Ouroboros.Consensus.Block (ConvertRawHash (..), RealPoint (..))
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, TxId)
import           Ouroboros.Network.Block (HeaderHash, Tip, getTipPoint)

renderTxIdForVerbosity
  :: ConvertTxId blk
  => TracingVerbosity
  -> TxId (GenTx blk)
  -> Text
renderTxIdForVerbosity verb = trimHashTextForVerbosity verb . renderTxId

renderTxId :: ConvertTxId blk => TxId (GenTx blk) -> Text
renderTxId = Text.decodeLatin1 . B16.encode . txIdToRawBytes

renderWithOrigin :: (a -> Text) -> WithOrigin a -> Text
renderWithOrigin _ Origin = "origin"
renderWithOrigin render (At a) = render a

renderSlotNo :: SlotNo -> Text
renderSlotNo = Text.pack . show . unSlotNo

renderRealPoint
  :: forall blk.
     ConvertRawHash blk
  => RealPoint blk
  -> Text
renderRealPoint (RealPoint slotNo headerHash) =
  renderHeaderHash (Proxy @blk) headerHash
    <> "@"
    <> renderSlotNo slotNo

renderPointForVerbosity
  :: forall blk.
     ConvertRawHash blk
  => TracingVerbosity
  -> Point blk
  -> Text
renderPointForVerbosity verb point =
  case point of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h ->
      renderHeaderHashForVerbosity (Proxy @blk) verb h
        <> "@"
        <> renderSlotNo slot

renderPoint :: ConvertRawHash blk => Point blk -> Text
renderPoint = renderPointForVerbosity MaximalVerbosity

renderTipForVerbosity
  :: ConvertRawHash blk
  => TracingVerbosity
  -> Tip blk
  -> Text
renderTipForVerbosity verb = renderPointForVerbosity verb . getTipPoint

renderTip :: ConvertRawHash blk => Tip blk -> Text
renderTip = renderTipForVerbosity MaximalVerbosity

renderHeaderHashForVerbosity
  :: ConvertRawHash blk
  => proxy blk
  -> TracingVerbosity
  -> HeaderHash blk
  -> Text
renderHeaderHashForVerbosity p verb =
  trimHashTextForVerbosity verb . renderHeaderHash p

-- | Hex encode and render a 'HeaderHash' as text.
renderHeaderHash :: ConvertRawHash blk => proxy blk -> HeaderHash blk -> Text
renderHeaderHash p = Text.decodeLatin1 . B16.encode . toRawHash p

trimHashTextForVerbosity :: TracingVerbosity -> Text -> Text
trimHashTextForVerbosity verb =
  case verb of
    MinimalVerbosity -> Text.take 7
    NormalVerbosity -> Text.take 7
    MaximalVerbosity -> id
