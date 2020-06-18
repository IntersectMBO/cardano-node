module Test.Cardano.Api.Typed.Gen
  ( genAddressByron
  , genAddressShelley
  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genSigningKey
  , genStakeAddress
  , genVerificationKey
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

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

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded

genOperationalCertificate :: Gen OperationalCertificate
genOperationalCertificate = fst <$> genOperationalCertificateWithCounter

genOperationalCertificateIssueCounter :: Gen OperationalCertificateIssueCounter
genOperationalCertificateIssueCounter = snd <$> genOperationalCertificateWithCounter

genOperationalCertificateWithCounter :: Gen (OperationalCertificate, OperationalCertificateIssueCounter)
genOperationalCertificateWithCounter = do
  kesVKey <- genVerificationKey AsKesKey
  stakePoolSign <- genSigningKey AsStakePoolKey
  kesP <- genKESPeriod
  c <- Gen.integral $ Range.linear 0 1000
  let -- stakePoolVer = getVerificationKey stakePoolSign
      -- TODO: Commenting this out as we're temporarily supporting the old op
      -- cert issue counter format.
      -- iCounter = OperationalCertificateIssueCounter c stakePoolVer
      iCounter = OperationalCertificateIssueCounter c

  case issueOperationalCertificate kesVKey stakePoolSign kesP iCounter of
    -- This case should be impossible as we clearly derive the verification
    -- key from the generated signing key.
    Left err -> fail $ displayError err
    Right pair -> return pair


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

{-
--TODO: Currently undefined
genTxBodyByron :: TxBody Byron
genTxBodyByron = getTxBody <$> genTxByron

genTxBodyShelley :: TxBody Shelley
genTxBodyShelley =
  Gen.choice
    [ genTxBodyByron
    , getTxBody <$> genTxShelley
    ]

--TODO: Currently undefined
genTxByron :: Gen (Tx Byron)
genTxByron = makeByronTransaction

genTxShelley :: Gen (Tx Shelley)
genTxShelley =
  Gen.choice
    [ genTxByron
    , makeShelleyTransaction
    ]
-}
genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken
