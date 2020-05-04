{-# LANGUAGE PatternSynonyms #-}
module Test.Cardano.Api.Gen
  ( genSigningKey
  , genSigningKeyByron
  , genSigningKeyShelley
  , genNetwork
  , genVerificationKey
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  , genByronVerificationKeyAddress
  , genShelleyVerificationKeyAddress
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import           Cardano.Crypto (hashRaw)
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude

import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.TxData (pattern TxBody, Wdrl (..))

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import qualified Test.Cardano.Crypto.Gen as Byron

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genByronVerificationKeyAddress :: Gen Address
genByronVerificationKeyAddress =
  byronVerificationKeyAddress <$> genVerificationKeyByron <*> genNetwork

genShelleyVerificationKeyAddress :: Gen Address
genShelleyVerificationKeyAddress =
  shelleyVerificationKeyAddress <$> genVerificationKeyShelley <*> genNetwork

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
  seed <- genSeed
  let sk = fst (withDRG (drgNewTest seed) genKeyDSIGN)
  return $ SigningKeyShelley (SKey sk)

genSeed :: Gen (Word64, Word64, Word64, Word64, Word64)
genSeed =
  (,,,,)
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> Byron.genProtocolMagicId
    ]

genVerificationKey :: Gen VerificationKey
genVerificationKey =
  Gen.choice
    [ genVerificationKeyByron
    , genVerificationKeyShelley
    ]

genVerificationKeyByron :: Gen VerificationKey
genVerificationKeyByron =
  getVerificationKey <$> genSigningKeyByron

genVerificationKeyShelley :: Gen VerificationKey
genVerificationKeyShelley =
  getVerificationKey <$> genSigningKeyShelley

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
  pure $ TxUnsignedByron tx (LBS.toStrict cbor) (coerce $ hashRaw cbor)

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

