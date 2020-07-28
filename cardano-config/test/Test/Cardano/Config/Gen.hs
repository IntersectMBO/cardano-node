{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Config.Gen
  ( genAddress
  , genGenesisDelegationPair
  , genGenesisFundPair
  , genNetworkTopology
  , genNodeAddress
  , genNodeHostAddress
  , genNodeSetup
  , genShelleyGenesis
  , genSigningKey
  , genVRFKeyPair
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types

import           Cardano.Crypto.DSIGN.Class

import qualified Data.IP as IP

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Gen ()
import qualified Hedgehog.Range as Range

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.Crypto

import           Test.Shelley.Spec.Ledger.Generator.Genesis

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
