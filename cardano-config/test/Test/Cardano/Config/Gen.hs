{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Config.Gen
  ( genAddress
  , genGenesisDelegationPair
  , genGenesisFundPair
  , genKESKeyPair
  , genKeyRole
  , genNetworkTopology
  , genNodeAddress
  , genNodeHostAddress
  , genNodeSetup
  , genShelleyGenesis
  , genSigningKey
  , genTextView
  , genVRFKeyPair
  ) where

import           Cardano.Prelude

import           Cardano.Config.TextView
import           Cardano.Config.Topology
import           Cardano.Config.Types

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.KES.Class
import           Cardano.Api.Shelley.ColdKeys (KeyRole (..), OperatorKeyRole (..))

import qualified Data.IP as IP

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Internal.Gen ()

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.Crypto

import           Test.Shelley.Spec.Ledger.Generator.Genesis

genKESKeyPair :: forall k. KESAlgorithm k => Gen (VerKeyKES k, SignKeyKES k)
genKESKeyPair = do
    seed <- genSeed seedSize
    let sk = genKeyKES seed
        vk = deriveVerKeyKES sk
    pure (vk, sk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeKES (Proxy :: Proxy k))

genKeyRole :: Gen KeyRole
genKeyRole =
  Gen.element
    [ GenesisKey
    , GenesisUTxOKey
    , OperatorKey GenesisDelegateKey
    , OperatorKey StakePoolOperatorKey
    ]

genNetworkTopology :: Gen NetworkTopology
genNetworkTopology =
  Gen.choice
    [ MockNodeTopology <$> Gen.list (Range.linear 0 10) genNodeSetup
    , RealNodeTopology <$> Gen.list (Range.linear 0 10) genRemoteAddress
    ]

genSigningKey :: Gen (SignKeyDSIGN (DSIGN TPraosStandardCrypto))
genSigningKey = do
  seed <- genSeed $ fromIntegral (seedSizeDSIGN (Proxy :: Proxy (DSIGN TPraosStandardCrypto)))
  return $ genKeyDSIGN seed

genTextView :: Gen TextView
genTextView =
  TextView
    <$> fmap TextViewType (Gen.utf8 (Range.linear 1 20) Gen.alpha)
    <*> fmap TextViewTitle (Gen.utf8 (Range.linear 1 80) (Gen.filter (/= '\n') Gen.ascii))
    <*> Gen.bytes (Range.linear 0 500)

genNodeAddress :: Gen NodeAddress
genNodeAddress =
  NodeAddress
    <$> genNodeHostAddress
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)

genNodeHostAddress :: Gen NodeHostAddress
genNodeHostAddress =
  NodeHostAddress
    <$> Gen.choice
          [ fmap (IP.IPv4 . IP.toIPv4w) <$> Gen.maybe Gen.enumBounded
          , fmap (IP.IPv6 . IP.toIPv6w) <$> Gen.maybe genFourWord32
          ]
  where
    genFourWord32 :: Gen (Word32, Word32, Word32, Word32)
    genFourWord32 =
       (,,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.enumBounded <*> Gen.enumBounded

genNodeSetup :: Gen NodeSetup
genNodeSetup =
  NodeSetup
    <$> Gen.word64 (Range.linear 0 10000)
    <*> genNodeAddress
    <*> Gen.list (Range.linear 0 6) genRemoteAddress

genRemoteAddress :: Gen RemoteAddress
genRemoteAddress =
  RemoteAddress
    <$> Gen.element cooking
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)
    <*> Gen.int (Range.linear 0 100)
