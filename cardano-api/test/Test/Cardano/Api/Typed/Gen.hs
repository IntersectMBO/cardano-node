{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Typed.Gen
  ( genAddressByron
  , genAddressShelley
  , genValueNestedRep
  , genValueNestedBundle
  , genByronKeyWitness

    -- * Scripts
  , genScript
  , genSimpleScript
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  , canonicaliseScriptVersion

  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genShelleyWitness
  , genSigningKey
  , genStakeAddress
  , genTx
  , genTxBody
  , genValue
  , genValueDefault
  , genVerificationKey
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import qualified Data.Map.Strict as Map
import           Data.String

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Cardano.Api.Metadata (genTxMetadata)
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


----------------------------------------------------------------------------
-- SimpleScript generators
--

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang

genScript (PlutusScriptLanguage lang) = case lang of {}

genSimpleScript :: SimpleScriptVersion lang -> Gen (SimpleScript lang)
genSimpleScript lang =
    genTerm
  where
    genTerm = Gen.recursive Gen.choice nonRecursive recursive

    -- Non-recursive generators
    nonRecursive =
         (RequireSignature . verificationKeyHash <$>
             genVerificationKey AsPaymentKey)

      : [ RequireTimeBefore supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

     ++ [ RequireTimeAfter supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

    -- Recursive generators
    recursive =
      [ RequireAllOf <$> Gen.list (Range.linear 0 10) genTerm

      , RequireAnyOf <$> Gen.list (Range.linear 0 10) genTerm

      , do ts <- Gen.list (Range.linear 0 10) genTerm
           m  <- Gen.integral (Range.constant 0 (length ts))
           return (RequireMOf m ts)
      ]


-- ----------------------------------------------------------------------------
-- Script generators for any language, or any language valid in a specific era
--

genScriptInAnyLang :: Gen ScriptInAnyLang
genScriptInAnyLang =
    Gen.choice
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    Gen.choice
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang
    return (hashScript script)


-- | For JSON round-trip testing we need to re-tag scripts with the lowest
-- language version that they fit into.

-- The reason for this is that the external JSON syntax for the simple script
-- language does specify the language version and so the JSON decoder simply
-- uses the lowest version of the language that supports the concrete script.
--
canonicaliseScriptVersion :: ScriptInEra era -> ScriptInEra era
canonicaliseScriptVersion (ScriptInEra SimpleScriptV2InAllegra
                                      (SimpleScript SimpleScriptV2 s))
    | Just s' <- adjustSimpleScriptVersion SimpleScriptV1 s
    = ScriptInEra SimpleScriptV1InAllegra (SimpleScript SimpleScriptV1 s')

canonicaliseScriptVersion (ScriptInEra SimpleScriptV2InMary
                                      (SimpleScript SimpleScriptV2 s))
    | Just s' <- adjustSimpleScriptVersion SimpleScriptV1 s
    = ScriptInEra SimpleScriptV1InMary (SimpleScript SimpleScriptV1 s')

canonicaliseScriptVersion s = s


----------------------------------------------------------------------------
-- Multi-asset generators
--

genAssetName :: Gen AssetName
genAssetName =
  Gen.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element ["", "a", "b", "c"])
    , (1, AssetName <$> Gen.utf8 (Range.singleton  32) Gen.alphaNum)
    , (1, AssetName <$> Gen.utf8 (Range.constant 1 31) Gen.alphaNum)
    ]

genPolicyId :: Gen PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: Gen AssetId
genAssetId = Gen.choice [ AssetId <$> genPolicyId <*> genAssetName
                        , return AdaAssetId
                        ]

genQuantity :: Range Integer -> Gen Quantity
genQuantity range = fromInteger <$> Gen.integral range

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = genQuantity (Range.constantFrom 0 (-2) 2)

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = genQuantity (Range.constant 0 2)

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    Gen.list (Range.constant 0 10)
             ((,) <$> genAId <*> genQuant)

-- | Generate a 'Value' with any asset ID and a positive or negative quantity.
genValueDefault :: Gen Value
genValueDefault = genValue genAssetId genSignedQuantity

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedQuantity
  where
    genAssetIdNoAda :: Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = genValue genAssetId genUnsignedQuantity


-- Note that we expect to sometimes generate duplicate policy id keys since we
-- pick 90% of policy ids from a set of just three.
genValueNestedRep :: Gen ValueNestedRep
genValueNestedRep =
  ValueNestedRep <$> Gen.list (Range.constant 0 5) genValueNestedBundle

genValueNestedBundle :: Gen ValueNestedBundle
genValueNestedBundle =
  Gen.choice
    [ ValueNestedBundleAda <$> genSignedQuantity
    , ValueNestedBundle <$> genPolicyId
                        <*> Gen.map (Range.constant 0 5)
                                    ((,) <$> genAssetName <*> genSignedQuantity)
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

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era =
  case era of
    ByronEra -> TxOutAdaOnly AdaOnlyInByronEra <$> genLovelace
    ShelleyEra -> TxOutAdaOnly AdaOnlyInShelleyEra <$> genLovelace
    AllegraEra -> TxOutAdaOnly AdaOnlyInAllegraEra <$> genLovelace
    MaryEra -> TxOutValue MultiAssetInMaryEra <$> genValueForTxOut

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
    ByronEra   -> pure TxAuxScriptsNone
    ShelleyEra -> pure TxAuxScriptsNone
    AllegraEra -> TxAuxScripts AuxScriptsInAllegraEra
                           <$> Gen.list (Range.linear 0 3)
                                        (genScriptInEra AllegraEra)
    MaryEra    -> TxAuxScripts AuxScriptsInMaryEra
                           <$> Gen.list (Range.linear 0 3)
                                        (genScriptInEra MaryEra)

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
        , TxMintValue MultiAssetInMaryEra <$> genValueForMinting
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
    ByronEra -> pure (TxFeeImplicit TxFeesImplicitInByronEra)
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
