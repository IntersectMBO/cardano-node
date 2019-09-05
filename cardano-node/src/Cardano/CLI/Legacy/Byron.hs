{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Cardano.CLI.Legacy.Byron (
      LegacyDelegateKey(..)
    , encodeLegacyDelegateKey
    , decodeLegacyDelegateKey
    ) where

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import           Control.Lens (LensLike, _Left)
import           Control.Monad
import qualified Data.Binary as Binary
import           Data.Coerce
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB

import qualified Crypto.SCRAPE as Scrape

import           Cardano.Prelude hiding (option)

import qualified Cardano.Crypto.Wallet as CC
import           Cardano.Crypto.Signing (SigningKey(..))

-- LegacyDelegateKey is a subset of the UserSecret's from the legacy codebase:
-- 1. the VSS keypair must be present
-- 2. the signing key must be present
-- 3. the rest must be absent (Nothing)
--
-- Legacy reference: https://github.com/input-output-hk/cardano-sl/blob/release/3.0.1/lib/src/Pos/Util/UserSecret.hs#L189
data LegacyDelegateKey
  =  LegacyDelegateKey
  { lrkSigningKey :: !SigningKey
  , lrkVSSKeyPair :: !Scrape.KeyPair
  }

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
encodeBinary :: Binary.Binary a => a -> E.Encoding
encodeBinary = E.encodeBytes . LB.toStrict . Binary.encode

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
decodeBinary :: Binary.Binary a => D.Decoder s a
decodeBinary = do
    x <- D.decodeBytesCanonical
    toCborError $ case Binary.decodeOrFail (LB.fromStrict x) of
        Left (_, _, err) -> Left (T.pack err)
        Right (bs, _, res)
            | LB.null bs -> Right res
            | otherwise  -> Left "decodeBinary: unconsumed input"

encodeXPrv :: CC.XPrv -> E.Encoding
encodeXPrv a = E.encodeBytes $ CC.unXPrv a

decodeXPrv :: D.Decoder s CC.XPrv
decodeXPrv =
  toCborError . over _Left T.pack . CC.xprv =<< D.decodeBytesCanonical

  where over :: LensLike Identity s t a b -> (a -> b) -> s -> t
        over = coerce

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
-- | Enforces that the input size is the same as the decoded one, failing in
-- case it's not.
enforceSize :: Text -> Int -> D.Decoder s ()
enforceSize lbl requestedSize = D.decodeListLenCanonical >>= matchSize requestedSize lbl

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> Text -> Int -> D.Decoder s ()
matchSize requestedSize lbl actualSize =
  when (actualSize /= requestedSize) $
    cborError (lbl <> " failed the size check. Expected " <> show requestedSize <> ", found " <> show actualSize)

-- Reverse-engineered from cardano-sl legacy codebase.
encodeLegacyDelegateKey :: LegacyDelegateKey -> E.Encoding
encodeLegacyDelegateKey dk@LegacyDelegateKey{lrkSigningKey=(SigningKey sk)}
  =  E.encodeListLen 4
  <> E.encodeListLen 1 <> encodeBinary (lrkVSSKeyPair dk)
  <> E.encodeListLen 1 <> encodeXPrv sk
  <> E.encodeListLenIndef <> E.encodeBreak
  <> E.encodeListLen 0

-- Reverse-engineered from cardano-sl legacy codebase.
decodeLegacyDelegateKey :: D.Decoder s LegacyDelegateKey
decodeLegacyDelegateKey = do
    enforceSize "UserSecret" 4
    vss  <- do
      enforceSize "vss" 1
      decodeBinary
    pkey <- do
      enforceSize "pkey" 1
      SigningKey <$> decodeXPrv
    _    <- do
      D.decodeListLenIndef
      D.decodeSequenceLenIndef (flip (:)) [] reverse D.decodeNull
    _    <- do
      enforceSize "wallet" 0
    pure $ LegacyDelegateKey pkey vss
