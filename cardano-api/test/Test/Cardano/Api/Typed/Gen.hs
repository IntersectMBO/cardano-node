module Test.Cardano.Api.Typed.Gen
  ( genAddressByron
  , genAddressShelley
  , genSigningKey
  , genStakeAddress
  , genVerificationKey
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Ouroboros.Network.Magic (NetworkMagic(..))


import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Cardano.Api.Gen (genSeed)
import           Test.Cardano.Api.Orphans ()

genAddressByron :: Gen (Address Byron)
genAddressByron = makeByronAddress <$> genVerificationKey AsByronKey <*> genNetworkId

genAddressShelley :: Gen (Address Shelley)
genAddressShelley =
  Gen.choice
    [ makeShelleyAddress <$> genNetworkId <*> genPaymentCredential <*> genStakeAddressReference
    , makeByronAddress <$> genVerificationKey AsByronKey <*> genNetworkId
    ]

genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded

-- TODO: Generate payment credential via script
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genStakeAddress :: Gen StakeAddress
genStakeAddress = makeStakeAddress <$> genNetworkId <*> genStakeCredential

-- TODO: Generate StakeAddressReference via pointer
genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  Gen.choice
    [ StakeAddressByValue <$> genStakeCredential
    , return NoStakeAddress
    ]

-- TODO: Generate StakeCredential via script
genStakeCredential :: Gen StakeCredential
genStakeCredential = do
  vKey <- genVerificationKey AsStakeKey
  return . StakeCredentialByKey $ verificationKeyHash vKey

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken
