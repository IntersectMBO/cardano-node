{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Hash.IsHash
  ( IsHash(..)
  , hashHex
  , mediumHashHex
  , shortHashHex
  , showBSHex
  )
where

import           Prelude
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import           Data.Text (Text)
import qualified Text.Builder as T

import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Mempool.API (GenTx, TxId)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.Hash.Short as CC
import qualified Ouroboros.Consensus.Mock.Ledger as Mock

class IsHash a where
  hashByteString :: a -> BS.ByteString

-- | Prints out a bytestring in hexadecimal.
showBSHex :: BS.ByteString -> Text
showBSHex bs = T.run $ BS.foldr paddedShowHex mempty bs
 where
   paddedShowHex x xs =  T.hexadecimalDigit (fromIntegral (x `shiftR` 4) :: Word8)
                      <> T.hexadecimalDigit (fromIntegral (x .&. 0xf) :: Word8)
                      <> xs

-- | Show only first @16@ characters of 'Hash'.
mediumHashHex :: IsHash a => a -> Text
mediumHashHex = showBSHex . BS.take 8 . hashByteString

-- | Show only first @8@ characters of 'Hash'.
shortHashHex :: IsHash a => a -> Text
shortHashHex = showBSHex . BS.take 4 . hashByteString

-- | Show only first @8@ characters of 'Hash'.
hashHex :: IsHash a => a -> Text
hashHex = showBSHex . hashByteString

instance IsHash (Crypto.AbstractHash algo a) where
  hashByteString (Crypto.AbstractHash digest) = ByteArray.convert digest

instance IsHash (CC.Hash CC.ShortHash a) where
  hashByteString = CC.getHash

instance IsHash (TxId (GenTx Byron.ByronBlock)) where
  hashByteString (Byron.ByronTxId utxoTxid) = hashByteString utxoTxid
  hashByteString (Byron.ByronDlgId dlgCertificateId) = hashByteString dlgCertificateId
  hashByteString (Byron.ByronUpdateProposalId updateUpId) = hashByteString updateUpId
  hashByteString (Byron.ByronUpdateVoteId updateVoteId) = hashByteString updateVoteId

instance IsHash Mock.TxId where
  hashByteString = undefined

instance IsHash (TxId (GenTx (Mock.SimpleBlock a b))) where
  hashByteString = hashByteString . Mock.unSimpleGenTxId

instance IsHash Byron.ByronHash where
  hashByteString = hashByteString . Byron.unByronHash
