{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Cardano.Node.Tracing.Render
  ( renderChunkNo
  , renderHeaderHash
  , renderHeaderHashForDetails
  , renderChainHash
  , renderTipBlockNo
  , renderTipHash
  , condenseT
  , showT
  , renderPoint
  , renderPointAsPhrase
  , renderPointForDetails
  , renderRealPoint
  , renderRealPointAsPhrase
  , renderSlotNo
  , renderTip
  , renderTipForDetails
  , renderTxId
  , renderTxIdForDetails
  , renderWithOrigin
  , renderScriptHash
  , renderScriptIntegrityHash
  , renderScriptPurpose
  , renderMissingRedeemers
  ) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Data.ByteString.Base16 as B16
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Logging
import           Cardano.Node.Queries (ConvertTxId (..))
import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Types as Aeson
import           Ouroboros.Consensus.Block (BlockNo (..), ConvertRawHash (..), RealPoint (..))
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, TxId)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkNo (..))
import           Ouroboros.Consensus.Util.Condense (Condense, condense)
import           Ouroboros.Network.Block (ChainHash (..), HeaderHash, StandardHash, Tip,
                   getTipPoint)

condenseT :: Condense a => a -> Text
condenseT = Text.pack . condense

renderChunkNo :: ChunkNo -> Text
renderChunkNo = Text.pack . show . unChunkNo

renderTipBlockNo :: ImmDB.Tip blk -> Text
renderTipBlockNo = Text.pack . show . unBlockNo . ImmDB.tipBlockNo

renderTipHash :: StandardHash blk => ImmDB.Tip blk -> Text
renderTipHash tInfo = Text.pack . show $ ImmDB.tipHash tInfo

renderTxIdForDetails
  :: ConvertTxId blk
  => DetailLevel
  -> TxId (GenTx blk)
  -> Text
renderTxIdForDetails dtal = trimHashTextForDetails dtal . renderTxId

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

-- | Render a short phrase describing a 'RealPoint'.
-- e.g. "62292d753b2ee7e903095bc5f10b03cf4209f456ea08f55308e0aaab4350dda4 at
-- slot 39920"
renderRealPointAsPhrase
  :: forall blk.
     ConvertRawHash blk
  => RealPoint blk
  -> Text
renderRealPointAsPhrase (RealPoint slotNo headerHash) =
  renderHeaderHash (Proxy @blk) headerHash
    <> " at slot "
    <> renderSlotNo slotNo

renderPointForDetails
  :: forall blk.
     ConvertRawHash blk
  => DetailLevel
  -> Point blk
  -> Text
renderPointForDetails dtal point =
  case point of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h ->
      renderHeaderHashForDetails (Proxy @blk) dtal h
        <> "@"
        <> renderSlotNo slot

renderPoint :: ConvertRawHash blk => Point blk -> Text
renderPoint = renderPointForDetails DDetailed

-- | Render a short phrase describing a 'Point'.
-- e.g. "62292d753b2ee7e903095bc5f10b03cf4209f456ea08f55308e0aaab4350dda4 at
-- slot 39920" or "genesis (origin)" in the case of a genesis point.
renderPointAsPhrase :: forall blk. ConvertRawHash blk => Point blk -> Text
renderPointAsPhrase point =
  case point of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h ->
      renderHeaderHash (Proxy @blk) h
        <> " at slot "
        <> renderSlotNo slot

renderTipForDetails
  :: ConvertRawHash blk
  => DetailLevel
  -> Tip blk
  -> Text
renderTipForDetails dtal = renderPointForDetails dtal . getTipPoint

renderTip :: ConvertRawHash blk => Tip blk -> Text
renderTip = renderTipForDetails DDetailed

renderHeaderHashForDetails
  :: ConvertRawHash blk
  => proxy blk
  -> DetailLevel
  -> HeaderHash blk
  -> Text
renderHeaderHashForDetails p dtal =
  trimHashTextForDetails dtal . renderHeaderHash p


-- | Hex encode and render a 'HeaderHash' as text.
renderHeaderHash :: ConvertRawHash blk => proxy blk -> HeaderHash blk -> Text
renderHeaderHash p = Text.decodeLatin1 . B16.encode . toRawHash p

renderChainHash :: (HeaderHash blk -> Text) -> ChainHash blk -> Text
renderChainHash _ GenesisHash = "GenesisHash"
renderChainHash p (BlockHash hash) = p hash

trimHashTextForDetails :: DetailLevel -> Text -> Text
trimHashTextForDetails dtal =
  case dtal of
    DMinimal  -> Text.take 7
    _         -> id

renderScriptIntegrityHash :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto) -> Aeson.Value
renderScriptIntegrityHash (Just witPPDataHash) =
  Aeson.String . Crypto.hashToTextAsHex $ SafeHash.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null


renderMissingRedeemers :: ()
  => Ledger.EraCrypto ledgerera ~ StandardCrypto
  => [(Alonzo.ScriptPurpose ledgerera, Ledger.ScriptHash StandardCrypto)]
  -> Aeson.Value
renderMissingRedeemers scripts = Aeson.object $ map renderTuple  scripts
  where
    renderTuple :: ()
      => Ledger.EraCrypto ledgerera ~ StandardCrypto
      => (Alonzo.ScriptPurpose ledgerera, Ledger.ScriptHash StandardCrypto)
      -> Aeson.Pair
    renderTuple (scriptPurpose, sHash) =
      Aeson.fromText (renderScriptHash sHash) .= renderScriptPurpose scriptPurpose

renderScriptHash :: Ledger.ScriptHash StandardCrypto -> Text
renderScriptHash = Api.serialiseToRawBytesHexText . Api.fromShelleyScriptHash

renderScriptPurpose :: ()
  => Ledger.EraCrypto ledgerera ~ StandardCrypto
  => Alonzo.ScriptPurpose ledgerera
  -> Aeson.Value
renderScriptPurpose = \case
  Alonzo.Minting pid ->
    Aeson.object [ "minting" .= Aeson.toJSON pid]
  Alonzo.Spending txin ->
    Aeson.object [ "spending" .= Api.fromShelleyTxIn txin]
  Alonzo.Rewarding rwdAcct ->
    Aeson.object [ "rewarding" .= Aeson.String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)]
  Alonzo.Certifying _cert ->
    Aeson.object
      [ "certifying" .= Aeson.toJSON @String "TODO CIP-1694 unimplemented" -- toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate sbe cert)
      ]
