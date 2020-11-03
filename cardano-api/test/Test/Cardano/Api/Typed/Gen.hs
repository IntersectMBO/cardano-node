module Test.Cardano.Api.Typed.Gen
  ( genAddressByron
  , genAddressShelley
  , genByronKeyWitness
  , genRequiredSig
  , genMultiSigScriptAllegra
  , genMultiSigScriptMary
  , genMultiSigScriptShelley
  , genScriptHash
  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genScriptShelley
  , genShelleyWitness
  , genSigningKey
  , genStakeAddress
  , genTxByron
  , genTxShelley
  , genTxBodyByron
  , genTxBodyShelley
  , genVerificationKey
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId)

{- HLINT ignore "Reduce duplication" -}

genAddressByron :: Gen (Address Byron)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address Shelley)
genAddressShelley =
  Gen.choice
    [ makeShelleyAddress <$> genNetworkId
                         <*> genPaymentCredential
                         <*> genStakeAddressReference

    , makeByronAddress   <$> genNetworkId
                         <*> genVerificationKey AsByronKey
    ]

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genLovelace :: Gen Lovelace
genLovelace = Lovelace <$> Gen.integral (Range.linear 0 5000)

-- Script Primitive Generators --

genRequiredSig :: ScriptFeatureInEra SignatureFeature era -> Gen (MultiSigScript era)
genRequiredSig sfeat = do
  verKey <- genVerificationKey AsPaymentKey
  return $ RequireSignature (verificationKeyHash verKey) sfeat

genRequireTimeBefore :: ScriptFeatureInEra TimeLocksFeature era -> Gen (MultiSigScript era)
genRequireTimeBefore sfeat = flip RequireTimeBefore sfeat <$> genSlotNo

genRequireTimeAfter :: ScriptFeatureInEra TimeLocksFeature era -> Gen (MultiSigScript era)
genRequireTimeAfter sfeat = flip RequireTimeAfter sfeat <$> genSlotNo

genAll :: [MultiSigScript era] -> Gen (MultiSigScript era)
genAll s = pure $ RequireAllOf s

genAny :: [MultiSigScript era] -> Gen (MultiSigScript era)
genAny s = pure $ RequireAnyOf s

genMofN :: [MultiSigScript era] -> Gen (MultiSigScript era)
genMofN s = do
 let numKeys = length s
 required <- Gen.integral (Range.linear 0 numKeys)
 pure $ RequireMOf required s

-- Shelley

genMultiSigScriptShelley :: Gen (MultiSigScript Shelley)
genMultiSigScriptShelley = genMultiSigScriptsShelley >>= Gen.element

genMultiSigScriptsShelley :: Gen [MultiSigScript Shelley]
genMultiSigScriptsShelley =
  Gen.recursive Gen.choice
    -- Non-recursive generators
    [ Gen.list (Range.constant 1 10) $ genRequiredSig SignaturesInShelleyEra
    ]
    -- Recursive generators
    [ Gen.subtermM
        genMultiSigScriptsShelley
        (\a -> sequence [genAll a, genAny a, genMofN a])

    ]

genScriptShelley :: Gen (Script Shelley)
genScriptShelley = makeMultiSigScriptShelley <$> genMultiSigScriptShelley

genScriptHash :: Gen (Hash (Script Shelley))
genScriptHash = scriptHashShelley <$> genScriptShelley

-- Allegra

genMultiSigScriptAllegra :: Gen (MultiSigScript Allegra)
genMultiSigScriptAllegra = genMultiSigScriptsAllegra >>= Gen.element

genMultiSigScriptsAllegra :: Gen [MultiSigScript Allegra]
genMultiSigScriptsAllegra =
  Gen.recursive Gen.choice
    -- Non-recursive generators
    [ Gen.list (Range.constant 1 10) $ genRequireTimeAfter TimeLocksInAllegraEra
    , Gen.list (Range.constant 1 10) $ genRequireTimeBefore TimeLocksInAllegraEra
    , Gen.list (Range.constant 1 10) $ genRequiredSig SignaturesInAllegraEra
    ]
    -- Recursive generators
    [ Gen.subtermM3
        genMultiSigScriptsAllegra
        genMultiSigScriptsAllegra
        genMultiSigScriptsAllegra
        (\a b c -> do shuffled <- Gen.shuffle $ a ++ b ++ c
                      subSeq <- Gen.subsequence shuffled
                      sequence [genAll subSeq, genAny subSeq, genMofN subSeq]
                      )

    ]

-- Mary

genMultiSigScriptMary :: Gen (MultiSigScript Mary)
genMultiSigScriptMary = genMultiSigScriptsMary >>= Gen.element

genMultiSigScriptsMary :: Gen [MultiSigScript Mary]
genMultiSigScriptsMary =
  Gen.recursive Gen.choice
    -- Non-recursive generators
    [ Gen.list (Range.constant 1 10) $ genRequireTimeAfter TimeLocksInMaryEra
    , Gen.list (Range.constant 1 10) $ genRequireTimeBefore TimeLocksInMaryEra
    , Gen.list (Range.constant 1 10) $ genRequiredSig SignaturesInMaryEra
    ]
    -- Recursive generators
    [ Gen.subtermM3
        genMultiSigScriptsMary
        genMultiSigScriptsMary
        genMultiSigScriptsMary
        (\a b c -> do shuffled <- Gen.shuffle $ a ++ b ++ c
                      subSeq <- Gen.subsequence shuffled
                      sequence [genAll subSeq, genAny subSeq, genMofN subSeq]
                      )

    ]

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
    stkPoolOrGenDelExtSign <- Gen.either (genSigningKey AsStakePoolKey) (genSigningKey AsGenesisDelegateExtendedKey)
    kesP <- genKESPeriod
    c <- Gen.integral $ Range.linear 0 1000
    let stakePoolVer = either getVerificationKey (convert . getVerificationKey) stkPoolOrGenDelExtSign
        iCounter = OperationalCertificateIssueCounter c stakePoolVer

    case issueOperationalCertificate kesVKey stkPoolOrGenDelExtSign kesP iCounter of
      -- This case should be impossible as we clearly derive the verification
      -- key from the generated signing key.
      Left err -> fail $ displayError err
      Right pair -> return pair
  where
    convert :: VerificationKey GenesisDelegateExtendedKey
            -> VerificationKey StakePoolKey
    convert = (castVerificationKey :: VerificationKey GenesisDelegateKey
                                   -> VerificationKey StakePoolKey)
            . (castVerificationKey :: VerificationKey GenesisDelegateExtendedKey
                                   -> VerificationKey GenesisDelegateKey)


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

genTxBodyShelley :: Gen (TxBody Shelley)
genTxBodyShelley =
   makeShelleyTransaction
     <$> genTxExtraContent
     <*> genTTL
     <*> genTxFee
     <*> Gen.list (Range.constant 1 10) genTxIn
     <*> Gen.list (Range.constant 1 10) genShelleyTxOut

genByronTxOut :: Gen (TxOut Byron)
genByronTxOut =
  TxOut <$> genAddressByron <*> genLovelace

genShelleyTxOut :: Gen (TxOut Shelley)
genShelleyTxOut =
  TxOut <$> genAddressShelley <*> genLovelace

genShelleyHash :: Gen (Crypto.Hash Crypto.Blake2b_256 ())
genShelleyHash = return $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

-- TODO: Should probably have a naive generator that generates no inputs, no outputs etc
genTxBodyByron :: Gen (TxBody Byron)
genTxBodyByron = do
  txIns <- Gen.list (Range.constant 1 10) genTxIn
  txOuts <- Gen.list (Range.constant 1 10) genByronTxOut
  case makeByronTransaction txIns txOuts of
    Left err -> panic $ show err
    Right txBody -> return txBody

genTxByron :: Gen (Tx Byron)
genTxByron =
  makeSignedTransaction
    <$> Gen.list (Range.constant 1 10) genByronKeyWitness
    <*> genTxBodyByron

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = TxIx <$> Gen.word Range.constantBounded

genTxShelley :: Gen (Tx Shelley)
genTxShelley =
  makeSignedTransaction
    <$> genWitnessList
    <*> genTxBodyShelley
 where
   genWitnessList :: Gen [Witness Shelley]
   genWitnessList = do
     bsWits <- Gen.list (Range.constant 0 10) genShelleyBootstrapWitness
     keyWits <- Gen.list (Range.constant 0 10) genShelleyKeyWitness
     return $ bsWits ++ keyWits

genTxExtraContent :: Gen TxExtraContent
genTxExtraContent = return txExtraContentEmpty

genTTL :: Gen TTL
genTTL = genSlotNo

genTxFee :: Gen TxFee
genTxFee = genLovelace

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genByronKeyWitness :: Gen (Witness Byron)
genByronKeyWitness = do
  pmId <- genProtocolMagicId
  txinWitness <- genVKWitness pmId
  return $ ByronKeyWitness txinWitness

genWitnessNetworkIdOrByronAddress :: Gen WitnessNetworkIdOrByronAddress
genWitnessNetworkIdOrByronAddress =
  Gen.choice
    [ WitnessNetworkId <$> genNetworkId
    , WitnessByronAddress <$> genAddressByron
    ]

genShelleyBootstrapWitness :: Gen (Witness Shelley)
genShelleyBootstrapWitness =
 makeShelleyBootstrapWitness
   <$> genWitnessNetworkIdOrByronAddress
   <*> genTxBodyShelley
   <*> genSigningKey AsByronKey

genShelleyKeyWitness :: Gen (Witness Shelley)
genShelleyKeyWitness =
  makeShelleyKeyWitness
    <$> genTxBodyShelley
    <*> genShelleyWitnessSigningKey

genShelleyWitness :: Gen (Witness Shelley)
genShelleyWitness = Gen.choice [genShelleyKeyWitness, genShelleyBootstrapWitness]

genShelleyWitnessSigningKey :: Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  Gen.choice [ WitnessPaymentKey <$>  genSigningKey AsPaymentKey
             , WitnessPaymentExtendedKey <$>  genSigningKey AsPaymentExtendedKey
             , WitnessStakeKey <$>  genSigningKey AsStakeKey
             , WitnessStakePoolKey <$>  genSigningKey AsStakePoolKey
             , WitnessGenesisDelegateKey <$>  genSigningKey AsGenesisDelegateKey
             , WitnessGenesisUTxOKey <$>  genSigningKey AsGenesisUTxOKey
             ]
{-
-- TODO: makeShelleyScriptWitness = undefined
genShelleyScriptWitness :: Gen (Witness Shelley)
genShelleyScriptWitness = makeShelleyScriptWitness
-}

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)
