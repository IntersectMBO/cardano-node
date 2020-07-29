module Cardano.Api.Shelley.ITN
  ( xprvFromBytes
  ) where

import           Cardano.Prelude

import qualified Crypto.ECC.Edwards25519 as Ed25519
import           Crypto.Error (eitherCryptoError)
import qualified Data.ByteString as BS

import qualified Cardano.Crypto.Wallet as CC

-- Shamelessly copied from cardano-addresses
-- | Construct an 'XPrv' from raw 'ByteString' (96 bytes).
xprvFromBytes :: ByteString -> Maybe CC.XPrv
xprvFromBytes bytes
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
