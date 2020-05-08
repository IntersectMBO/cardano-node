{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Test.Cardano.Api.Gen
  ( genAddress
  , genNetwork
  , genSigningKey
  , genSigningKeyByron
  , genSigningKeyShelley
  , genTxIn
  , genTxOut
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  , genVerificationKey
  , genVerificationKeyAddressByron
  , genVerificationKeyAddressShelley
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import qualified Cardano.Crypto as Byron
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import           Cardano.Crypto.Seed as Crypto

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set


import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.TxData (pattern TxBody, Wdrl (..))

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import qualified Test.Cardano.Crypto.Gen as Byron

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genAddress :: Gen Address
genAddress =
  Gen.choice
    [ genVerificationKeyAddressByron
    , genVerificationKeyAddressShelley
    ]

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> Byron.genProtocolMagicId
    ]

genSigningKey :: Gen SigningKey
genSigningKey =
  Gen.choice
    [ genSigningKeyByron
    , genSigningKeyShelley
    ]

genSigningKeyByron :: Gen SigningKey
genSigningKeyByron =
  SigningKeyByron <$> Byron.genSigningKey

genSigningKeyShelley :: Gen SigningKey
genSigningKeyShelley = do
  seed <- genSeed seedSize
  let sk = genKeyDSIGN seed
  return (SigningKeyShelley sk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeDSIGN (Proxy :: Proxy (DSIGN TPraosStandardCrypto)))

genTxIn :: Gen TxIn
genTxIn =
  TxIn <$> genFakeTxId <*> Gen.word (Range.linear 0 10000)

genTxOut :: Gen TxOut
genTxOut =
  TxOut <$> genAddress <*> genLovelace

genTxSigned :: Gen TxSigned
genTxSigned =
  Gen.choice
    [ genTxSignedByron
    , genTxSignedShelley
    ]

genTxSignedByron :: Gen TxSigned
genTxSignedByron =
  signTransaction
    <$> genTxUnsignedByron
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKeyByron

genTxSignedShelley :: Gen TxSigned
genTxSignedShelley =
  signTransaction
    <$> genTxUnsignedShelley
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKeyShelley

genTxUnsigned :: Gen TxUnsigned
genTxUnsigned =
  Gen.choice
    [ genTxUnsignedByron
    , genTxUnsignedShelley
    ]

genTxUnsignedByron :: Gen TxUnsigned
genTxUnsignedByron = do
  tx <- genTx
  let cbor = serialize tx
  pure $ TxUnsignedByron tx (LBS.toStrict cbor) (coerce $ Byron.hashRaw cbor)

genTxUnsignedShelley :: Gen TxUnsigned
genTxUnsignedShelley =
    TxUnsignedShelley <$> genTxBodyShelley
  where
    -- TODO: Improve this incredibly naive generator.
    genTxBodyShelley :: Gen ShelleyTxBody
    genTxBodyShelley = do
      coin <- Gen.integral (Range.linear 0 10000000000)
      slot <- Gen.word64 (Range.linear minBound maxBound)
      pure $ TxBody (Set.fromList []) (StrictSeq.fromList []) (StrictSeq.fromList [])
                (Wdrl $ Map.fromList []) (Coin coin) (SlotNo slot) SNothing SNothing

genVerificationKey :: Gen VerificationKey
genVerificationKey =
  Gen.choice
    [ genVerificationKeyByron
    , genVerificationKeyShelley
    ]

genVerificationKeyAddressByron :: Gen Address
genVerificationKeyAddressByron =
  byronVerificationKeyAddress <$> genVerificationKeyByron <*> genNetwork

genVerificationKeyAddressShelley :: Gen Address
genVerificationKeyAddressShelley =
  shelleyVerificationKeyAddress <$> genVerificationKeyShelley <*> genNetwork

genVerificationKeyByron :: Gen VerificationKey
genVerificationKeyByron =
  getVerificationKey <$> genSigningKeyByron

genVerificationKeyShelley :: Gen VerificationKey
genVerificationKeyShelley =
  getVerificationKey <$> genSigningKeyShelley

-- -------------------------------------------------------------------------------------------------

-- Generates a fake TxId by applying the right hashing function to a random ByteString.
genFakeTxId :: Gen TxId
genFakeTxId =
  TxId . {- Crypto. -} hashRaw identity <$> Gen.bytes (Range.linear 10 50)

-- This name will clash with one in Cardano.Crypto.Hash.Class.
-- This should be removed (or maybe specialized) then the one in Cardano.Crypto.Hash.Class is
-- available.
hashRaw :: (a -> ByteString) -> a -> Crypto.Hash Crypto.Blake2b_256 ()
hashRaw serialise = Crypto.UnsafeHash . Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) . serialise

genLovelace :: Gen Lovelace
genLovelace =
  Lovelace <$> Gen.integral (Range.linear 1 999999999999)

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)
