{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | BIP32-Ed25519 digital signatures.
module Cardano.Api.Crypto.Bip32Ed25519
  ( Bip32Ed25519DSIGN
  , SigDSIGN (..)
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)

    -- * Serialisation
  , xPrvToBytes
  , xPrvFromBytes
  )
where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.ByteArray as BA (ByteArrayAccess, ScrubbedBytes, convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Wallet as CC

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.Seed
import           Cardano.Crypto.Util (SignableRepresentation (..))

import qualified Crypto.ECC.Edwards25519 as Ed25519
import           Crypto.Error (eitherCryptoError)


data Bip32Ed25519DSIGN

instance DSIGNAlgorithm Bip32Ed25519DSIGN where

    --
    -- Key and signature types
    --

    newtype VerKeyDSIGN Bip32Ed25519DSIGN = VerKeyBip32Ed25519DSIGN CC.XPub
        deriving (Show, Eq, Generic)
        deriving newtype NFData
        deriving NoUnexpectedThunks via UseIsNormalForm CC.XPub

    newtype SignKeyDSIGN Bip32Ed25519DSIGN = SignKeyBip32Ed25519DSIGN CC.XPrv
        deriving (Generic, ByteArrayAccess)
        deriving newtype NFData
        deriving NoUnexpectedThunks via UseIsNormalForm CC.XPrv

    newtype SigDSIGN Bip32Ed25519DSIGN = SigBip32Ed25519DSIGN CC.XSignature
        deriving (Show, Eq, Generic, ByteArrayAccess)
        deriving NoUnexpectedThunks via UseIsNormalForm CC.XSignature

    --
    -- Metadata and basic key operations
    --

    algorithmNameDSIGN _ = "bip32_ed25519"

    deriveVerKeyDSIGN (SignKeyBip32Ed25519DSIGN sk) =
      VerKeyBip32Ed25519DSIGN $ CC.toXPub sk

    --
    -- Core algorithm operations
    --

    type Signable Bip32Ed25519DSIGN = SignableRepresentation

    signDSIGN () a (SignKeyBip32Ed25519DSIGN sk) =
      SigBip32Ed25519DSIGN $
        CC.sign (mempty :: ScrubbedBytes) sk (getSignableRepresentation a)

    verifyDSIGN () (VerKeyBip32Ed25519DSIGN vk) a (SigBip32Ed25519DSIGN sig) =
      if CC.verify vk (getSignableRepresentation a) sig
        then Right ()
        else Left "Verification failed"

    --
    -- Key generation
    --

    seedSizeDSIGN _  = 32

    genKeyDSIGN seed =
      SignKeyBip32Ed25519DSIGN $
        CC.generateNew
          (getSeedBytes seed)
          (mempty :: ScrubbedBytes)
          (mempty :: ScrubbedBytes)

    --
    -- raw serialise/deserialise
    --

    -- | BIP32-Ed25519 extended verification key size is 64 octets.
    sizeVerKeyDSIGN  _ = 64

    -- | BIP32-Ed25519 extended signing key size is 96 octets.
    sizeSignKeyDSIGN _ = 96

    -- | BIP32-Ed25519 extended signature size is 64 octets.
    sizeSigDSIGN     _ = 64

    rawSerialiseVerKeyDSIGN (VerKeyBip32Ed25519DSIGN vk) = CC.unXPub vk
    rawSerialiseSignKeyDSIGN (SignKeyBip32Ed25519DSIGN sk) = xPrvToBytes sk
    rawSerialiseSigDSIGN = BA.convert

    rawDeserialiseVerKeyDSIGN =
      either (const Nothing) (Just . VerKeyBip32Ed25519DSIGN) . CC.xpub
    rawDeserialiseSignKeyDSIGN =
      fmap SignKeyBip32Ed25519DSIGN . xPrvFromBytes
    rawDeserialiseSigDSIGN =
      either (const Nothing) (Just . SigBip32Ed25519DSIGN) . CC.xsignature


instance Show (SignKeyDSIGN Bip32Ed25519DSIGN) where
  show (SignKeyBip32Ed25519DSIGN sk) = show $ xPrvToBytes sk

instance ToCBOR (VerKeyDSIGN Bip32Ed25519DSIGN) where
  toCBOR = encodeVerKeyDSIGN
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance FromCBOR (VerKeyDSIGN Bip32Ed25519DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance ToCBOR (SignKeyDSIGN Bip32Ed25519DSIGN) where
  toCBOR = encodeSignKeyDSIGN
  encodedSizeExpr _ = encodedSignKeyDESIGNSizeExpr

instance FromCBOR (SignKeyDSIGN Bip32Ed25519DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance ToCBOR (SigDSIGN Bip32Ed25519DSIGN) where
  toCBOR = encodeSigDSIGN
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance FromCBOR (SigDSIGN Bip32Ed25519DSIGN) where
  fromCBOR = decodeSigDSIGN


-- | Serialise an 'CC.XPrv' to a 'ByteString' (96 bytes).
--
-- In @cardano-crypto@, an 'CC.XPrv' was originally serialised using the
-- following 128-byte binary format:
--
-- +---------------------------------+-----------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Public Key (32 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+-----------------------+
--
-- However, this function serialises an 'CC.XPrv' using a more compact 96-byte
-- binary format:
--
-- +---------------------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+
--
xPrvToBytes :: CC.XPrv -> ByteString
xPrvToBytes xPrv = privateKeyBytes <> chainCodeBytes
  where
    privateKeyBytes :: ByteString
    privateKeyBytes = BS.take 64 (CC.unXPrv xPrv)

    chainCodeBytes :: ByteString
    chainCodeBytes = BS.drop 96 (CC.unXPrv xPrv)

-- | Deserialise an 'CC.XPrv' from a 'ByteString' (96 bytes).
--
-- In @cardano-crypto@, an 'CC.XPrv' was originally deserialised using the
-- following 128-byte binary format:
--
-- +---------------------------------+-----------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Public Key (32 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+-----------------------+
--
-- However, this function deserialises an 'CC.XPrv' using a more compact
-- 96-byte binary format:
--
-- +---------------------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+
--
xPrvFromBytes :: ByteString -> Maybe CC.XPrv
xPrvFromBytes bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        eitherToMaybe $ CC.xprv $ prv <> pub <> cc
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just

    ed25519ScalarMult :: ByteString -> Maybe ByteString
    ed25519ScalarMult bs = do
      scalar <- eitherToMaybe . eitherCryptoError $ Ed25519.scalarDecodeLong bs
      pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar
