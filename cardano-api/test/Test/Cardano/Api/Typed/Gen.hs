{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Typed.Gen
  ( genAddressByron
  , genAddressShelley
  , genByronKeyWitness
  , genRequiredSig
  , genMofNRequiredSig
  , genSimpleScript
  , genSimpleScripts
  , genMultiSigScript
  , genMultiSigScriptAllegra
  , genMultiSigScriptMary
  , genMultiSigScriptShelley
  , genScriptHash
  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genScript
  , genShelleyWitness
  , genSigningKey
  , genStakeAddress
  , genTx
  , genTxBody
  , genVerificationKey
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import qualified Data.Map as Map

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Cardano.Api.MetaData (genTxMetadata)
import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId)

{- HLINT ignore "Reduce duplication" -}

genAddressByron :: Gen (Address ByronAddr)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley = makeShelleyAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genLovelace :: Gen Lovelace
genLovelace = Lovelace <$> Gen.integral (Range.linear 0 5000)

-- Script Primitive Generators --

genRequiredSig :: ScriptFeatureInEra SignatureFeature era -> Gen (MultiSigScript era)
genRequiredSig sfeat = do
  verKey <- genVerificationKey AsPaymentKey
  return $ RequireSignature sfeat (verificationKeyHash verKey)

genRequireTimeBefore :: ScriptFeatureInEra TimeLocksFeature era -> Gen (MultiSigScript era)
genRequireTimeBefore sfeat = RequireTimeBefore sfeat <$> genSlotNo

genRequireTimeAfter :: ScriptFeatureInEra TimeLocksFeature era -> Gen (MultiSigScript era)
genRequireTimeAfter sfeat = RequireTimeAfter sfeat <$> genSlotNo

genAll :: [MultiSigScript era] -> Gen (MultiSigScript era)
genAll s = pure $ RequireAllOf s

genAny :: [MultiSigScript era] -> Gen (MultiSigScript era)
genAny s = pure $ RequireAnyOf s

genMofN :: [MultiSigScript era] -> Gen (MultiSigScript era)
genMofN s = do
 let numKeys = length s
 required <- Gen.integral (Range.linear 0 numKeys)
 pure $ RequireMOf required s

-- Script generators for a provided Shelley-based era

genSimpleScript :: ShelleyBasedEra era -> Gen (SimpleScript era)
genSimpleScript era =
  case era of
    ShelleyBasedEraShelley -> genMultiSigScriptShelley
    ShelleyBasedEraAllegra -> genMultiSigScriptAllegra
    ShelleyBasedEraMary -> genMultiSigScriptMary

genSimpleScripts :: ShelleyBasedEra era -> Gen [SimpleScript era]
genSimpleScripts era =
  case era of
    ShelleyBasedEraShelley -> genMultiSigScriptsShelley
    ShelleyBasedEraAllegra -> genMultiSigScriptsAllegra
    ShelleyBasedEraMary -> genMultiSigScriptsMary

-- Shelley

genMultiSigScriptShelley :: Gen (MultiSigScript ShelleyEra)
genMultiSigScriptShelley = genMultiSigScriptsShelley >>= Gen.element

genMultiSigScriptsShelley :: Gen [MultiSigScript ShelleyEra]
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

-- Allegra

genMultiSigScriptAllegra :: Gen (MultiSigScript AllegraEra)
genMultiSigScriptAllegra = genMultiSigScriptsAllegra >>= Gen.element

genMultiSigScriptsAllegra :: Gen [MultiSigScript AllegraEra]
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

genMultiSigScriptMary :: Gen (MultiSigScript MaryEra)
genMultiSigScriptMary = genMultiSigScriptsMary >>= Gen.element

genMultiSigScriptsMary :: Gen [MultiSigScript MaryEra]
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

genAllRequiredSig :: Gen (MultiSigScript ShelleyEra)
genAllRequiredSig =
  RequireAllOf <$> Gen.list (Range.constant 1 10) (genRequiredSig SignaturesInShelleyEra)

genAnyRequiredSig :: Gen (MultiSigScript ShelleyEra)
genAnyRequiredSig =
  RequireAnyOf <$> Gen.list (Range.constant 1 10) (genRequiredSig SignaturesInShelleyEra)

genMofNRequiredSig :: Gen (MultiSigScript ShelleyEra)
genMofNRequiredSig = do
 required <- Gen.integral (Range.linear 2 15)
 total <- Gen.integral (Range.linear (required + 1) 15)
 RequireMOf required <$> Gen.list (Range.singleton total) (genRequiredSig SignaturesInShelleyEra)

genMultiSigScript :: Gen (MultiSigScript ShelleyEra)
genMultiSigScript =
  Gen.choice [genAllRequiredSig, genAnyRequiredSig, genMofNRequiredSig]

genScript :: HasScriptFeatures era => ShelleyBasedEra era -> Gen (Script era)
genScript era = SimpleScript <$> genSimpleScript era

genScriptHash :: Gen ScriptHash
genScriptHash = scriptHash <$> genScript ShelleyBasedEraShelley

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

genTxBodyShelley :: Gen (TxBody ShelleyEra)
genTxBodyShelley = do
  res <- makeTransactionBody <$> genTxBodyContent ShelleyEra
  case res of
    Left err -> fail (show err) -- TODO: Render function for TxBodyError
    Right txBody -> pure txBody

genByronTxOut :: Gen (TxOut ByronEra)
genByronTxOut =
  TxOut <$> (byronAddressInEra <$> genAddressByron)
        <*> (TxOutAdaOnly AdaOnlyInByronEra <$> genLovelace)

genShelleyTxOut :: Gen (TxOut ShelleyEra)
genShelleyTxOut =
  TxOut <$> (shelleyAddressInEra <$> genAddressShelley)
        <*> (TxOutAdaOnly AdaOnlyInShelleyEra <$> genLovelace)

genShelleyHash :: Gen (Crypto.Hash Crypto.Blake2b_256 ())
genShelleyHash = return $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

-- TODO: Should probably have a naive generator that generates no inputs, no outputs etc
genTxBodyByron :: Gen (TxBody ByronEra)
genTxBodyByron = do
  res <- makeTransactionBody <$> genTxBodyContent ByronEra
  case res of
    Left err -> fail (show err)
    Right txBody -> pure txBody

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = TxIx <$> Gen.word Range.constantBounded

genQuantity :: Gen Quantity
genQuantity = Quantity <$> Gen.integral (Range.linear 0 5000)

-- TODO: UTF8 bytes or random bytes?
genAssetName :: Gen AssetName
genAssetName = AssetName <$> Gen.bytes (Range.singleton 32)

genPolicyId :: Gen PolicyId
genPolicyId = PolicyId <$> genScriptHash

genAssetId :: Gen AssetId
genAssetId =
  Gen.frequency
    [ (1, pure AdaAssetId)
    , (9, AssetId <$> genPolicyId <*> genAssetName)
    ]

genValue :: Gen Value
genValue = valueFromList <$> Gen.list range genKeyValuePair
  where
    range :: Range Int
    range = Range.constant 1 10

    genKeyValuePair :: Gen (AssetId, Quantity)
    genKeyValuePair = (,) <$> genAssetId <*> genQuantity

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era =
  case era of
    ByronEra -> TxOutAdaOnly AdaOnlyInByronEra <$> genLovelace
    ShelleyEra -> TxOutAdaOnly AdaOnlyInShelleyEra <$> genLovelace
    AllegraEra -> TxOutAdaOnly AdaOnlyInAllegraEra <$> genLovelace
    MaryEra -> TxOutValue MultiAssetInMaryEra <$> genValue

genTxOut :: CardanoEra era -> Gen (TxOut era)
genTxOut era =
  case era of
    ByronEra -> genByronTxOut
    ShelleyEra -> genShelleyTxOut
    AllegraEra ->
      TxOut
        <$> (shelleyAddressInEra <$> genAddressShelley)
        <*> (TxOutAdaOnly AdaOnlyInAllegraEra <$> genLovelace)
    MaryEra ->
      TxOut
        <$> (shelleyAddressInEra <$> genAddressShelley)
        <*> genTxOutValue era

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case era of
    ByronEra -> pure TxValidityNoLowerBound
    ShelleyEra -> pure TxValidityNoLowerBound
    AllegraEra -> TxValidityLowerBound ValidityLowerBoundInAllegraEra <$> genTtl
    MaryEra -> TxValidityLowerBound ValidityLowerBoundInMaryEra <$> genTtl

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case era of
    ByronEra -> pure (TxValidityNoUpperBound ValidityNoUpperBoundInByronEra)
    ShelleyEra -> TxValidityUpperBound ValidityUpperBoundInShelleyEra <$> genTtl
    AllegraEra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra <$> genTtl
    MaryEra -> TxValidityUpperBound ValidityUpperBoundInMaryEra <$> genTtl

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case era of
    ByronEra -> pure TxMetadataNone
    ShelleyEra ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra TxMetadataInShelleyEra <$> genTxMetadata
        ]
    AllegraEra ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra TxMetadataInAllegraEra <$> genTxMetadata
        ]
    MaryEra ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra TxMetadataInMaryEra <$> genTxMetadata
        ]

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case era of
    ByronEra -> pure TxAuxScriptsNone
    ShelleyEra -> pure TxAuxScriptsNone
    AllegraEra ->
      TxAuxScripts AuxScriptsInAllegraEra
        <$> (map SimpleScript <$> genSimpleScripts ShelleyBasedEraAllegra)
    MaryEra ->
      TxAuxScripts AuxScriptsInMaryEra
        <$> (map SimpleScript <$> genSimpleScripts ShelleyBasedEraMary)

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals era)
genTxWithdrawals era =
  case era of
    ByronEra -> pure TxWithdrawalsNone
    ShelleyEra ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals WithdrawalsInShelleyEra mempty) -- TODO: Generate withdrawals
        ]
    AllegraEra ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals WithdrawalsInAllegraEra mempty) -- TODO: Generate withdrawals
        ]
    MaryEra ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals WithdrawalsInMaryEra mempty) -- TODO: Generate withdrawals
        ]

genTxCertificates :: CardanoEra era -> Gen (TxCertificates era)
genTxCertificates era =
  case era of
    ByronEra -> pure TxCertificatesNone
    ShelleyEra ->
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates CertificatesInShelleyEra mempty) -- TODO: Generate certificates
        ]
    AllegraEra ->
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates CertificatesInAllegraEra mempty) -- TODO: Generate certificates
        ]
    MaryEra ->
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates CertificatesInMaryEra mempty) -- TODO: Generate certificates
        ]

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal era =
  case era of
    ByronEra -> pure TxUpdateProposalNone
    ShelleyEra ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , pure (TxUpdateProposal UpdateProposalInShelleyEra emptyUpdateProposal) -- TODO: Generate proposals
        ]
    AllegraEra ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , pure (TxUpdateProposal UpdateProposalInAllegraEra emptyUpdateProposal) -- TODO: Generate proposals
        ]
    MaryEra ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , pure (TxUpdateProposal UpdateProposalInMaryEra emptyUpdateProposal) -- TODO: Generate proposals
        ]
  where
    emptyUpdateProposal :: UpdateProposal
    emptyUpdateProposal = UpdateProposal Map.empty (EpochNo 0)

genTxMintValue :: CardanoEra era -> Gen (TxMintValue era)
genTxMintValue era =
  case era of
    ByronEra -> pure TxMintNone
    ShelleyEra -> pure TxMintNone
    AllegraEra -> pure TxMintNone
    MaryEra ->
      Gen.choice
        [ pure TxMintNone
        , TxMintValue MultiAssetInMaryEra <$> genValue
        ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent era)
genTxBodyContent era = do
  trxIns <- Gen.list (Range.constant 1 10) genTxIn
  trxOuts <- Gen.list (Range.constant 1 10) (genTxOut era)
  fee <- genTxFee era
  validityRange <- genTxValidityRange era
  txMd <- genTxMetadataInEra era
  auxScripts <- genTxAuxScripts era
  withdrawals <- genTxWithdrawals era
  certs <- genTxCertificates era
  updateProposal <- genTxUpdateProposal era
  mintValue <- genTxMintValue era

  pure $ TxBodyContent
    { txIns = trxIns
    , txOuts = trxOuts
    , txFee = fee
    , txValidityRange = validityRange
    , txMetadata = txMd
    , txAuxScripts = auxScripts
    , txWithdrawals = withdrawals
    , txCertificates = certs
    , txUpdateProposal = updateProposal
    , txMintValue = mintValue
    }

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case era of
    ByronEra -> pure TxFeeImplicit
    ShelleyEra -> TxFeeExplicit TxFeesExplicitInShelleyEra <$> genLovelace
    AllegraEra -> TxFeeExplicit TxFeesExplicitInAllegraEra <$> genLovelace
    MaryEra -> TxFeeExplicit TxFeesExplicitInMaryEra <$> genLovelace

genTxBody :: CardanoEra era -> Gen (TxBody era)
genTxBody era =
  case era of
    ByronEra -> genTxBodyByron
    ShelleyEra -> genTxBodyShelley
    AllegraEra -> do
      res <- makeTransactionBody <$> genTxBodyContent AllegraEra
      case res of
        Left err -> fail (show err) -- TODO: Render function for TxBodyError
        Right txBody -> pure txBody
    MaryEra -> do
      res <- makeTransactionBody <$> genTxBodyContent MaryEra
      case res of
        Left err -> fail (show err) -- TODO: Render function for TxBodyError
        Right txBody -> pure txBody

genTx :: forall era. CardanoEra era -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnessList
    <*> genTxBody era
  where
    genWitnessList :: Gen [Witness era]
    genWitnessList =
      case era of
        ByronEra -> Gen.list (Range.constant 1 10) genByronKeyWitness
        ShelleyEra -> genShelleyBasedWitnessList
        AllegraEra -> genShelleyBasedWitnessList
        MaryEra -> genShelleyBasedWitnessList

    genShelleyBasedWitnessList :: IsShelleyBasedEra era => Gen [Witness era]
    genShelleyBasedWitnessList = do
      bsWits <- Gen.list (Range.constant 0 10) (genShelleyBootstrapWitness era)
      keyWits <- Gen.list (Range.constant 0 10) (genShelleyKeyWitness era)
      return $ bsWits ++ keyWits

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genByronKeyWitness :: Gen (Witness ByronEra)
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

genShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (Witness era)
genShelleyBootstrapWitness era =
 makeShelleyBootstrapWitness
   <$> genWitnessNetworkIdOrByronAddress
   <*> genTxBody era
   <*> genSigningKey AsByronKey

genShelleyKeyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (Witness era)
genShelleyKeyWitness era =
  makeShelleyKeyWitness
    <$> genTxBody era
    <*> genShelleyWitnessSigningKey

genShelleyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (Witness era)
genShelleyWitness era =
  Gen.choice
   [ genShelleyKeyWitness era
   , genShelleyBootstrapWitness era
   ]

genShelleyWitnessSigningKey :: Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  Gen.choice [ WitnessPaymentKey <$>  genSigningKey AsPaymentKey
             , WitnessPaymentExtendedKey <$>  genSigningKey AsPaymentExtendedKey
             , WitnessStakeKey <$>  genSigningKey AsStakeKey
             , WitnessStakePoolKey <$>  genSigningKey AsStakePoolKey
             , WitnessGenesisDelegateKey <$>  genSigningKey AsGenesisDelegateKey
             , WitnessGenesisUTxOKey <$>  genSigningKey AsGenesisUTxOKey
             ]

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)
