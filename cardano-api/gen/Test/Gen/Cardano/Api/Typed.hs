{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Gen.Cardano.Api.Typed
  ( genAddressByron
  , genAddressInEra
  , genAddressShelley
  , genCertificate
  , genCostModel
  , genMaybePraosNonce
  , genPraosNonce
  , genProtocolParameters
  , genValueNestedRep
  , genValueNestedBundle
  , genByronKeyWitness
  , genCardanoKeyWitness
  , genShelleyKeyWitness

  , genTxId
  , genTxIn
  , genTxOutTxContext
  , genTxOutUTxOContext
  , genUTxO

    -- * Scripts
  , genHashableScriptData
  , genReferenceScript
  , genScript
  , genSimpleScript
  , genPlutusScript
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  , genScriptData
  , genScriptDataSchema
  , genScriptValidity

  , genAssetName
  , genAssetId
  , genEpochNo
  , genExecutionUnitPrices
  , genExecutionUnits
  , genHashScriptData
  , genKESPeriod
  , genNat
  , genNetworkId
  , genNetworkMagic
  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genOperationalCertificateWithCounter
  , genPaymentCredential
  , genPolicyId
  , genQuantity
  , genRationalInt64
  , genSeed
  , genShelleyBootstrapWitness
  , genShelleyHash
  , genShelleyWitness
  , genShelleyWitnessSigningKey
  , genSignedQuantity
  , genSignedNonZeroQuantity
  , genSigningKey
  , genSlotNo
  , genStakeAddress
  , genStakeAddressReference
  , genStakeCredential
  , genTtl
  , genTx
  , genTxAuxScripts
  , genTxBody
  , genTxBodyContent
  , genTxCertificates
  , genTxFee
  , genTxIndex
  , genTxInsCollateral
  , genTxInsReference
  , genTxMetadataInEra
  , genTxMintValue
  , genLovelace
  , genPositiveLovelace
  , genValue
  , genValueDefault
  , genVerificationKey
  , genVerificationKeyHash
  , genUpdateProposal
  , genProtocolParametersUpdate
  , genScriptDataSupportedInAlonzoEra
  , genTxOutDatumHashTxContext
  , genTxOutDatumHashUTxOContext
  , genTxOutValue
  , genTxReturnCollateral
  , genTxScriptValidity
  , genTxTotalCollateral
  , genTxUpdateProposal
  , genTxValidityLowerBound
  , genTxValidityRange
  , genTxValidityUpperBound
  , genTxWithdrawals
  , genUnsignedQuantity
  , genPositiveQuantity
  , genValueForMinting
  , genValueForTxOut
  , genWitnesses
  , genWitnessNetworkIdOrByronAddress

  , genRational

  , genGovernancePoll
  , genGovernancePollAnswer
  , genGovernancePollWitness
  ) where

import           Cardano.Api hiding (txIns)
import qualified Cardano.Api as Api
import           Cardano.Api.Byron (KeyWitness (ByronKeyWitness),
                   WitnessNetworkIdOrByronAddress (..))
import           Cardano.Api.Shelley (GovernancePoll (..), GovernancePollAnswer (..),
                   GovernancePollWitness (..), Hash (..), KESPeriod (KESPeriod),
                   OperationalCertificateIssueCounter (OperationalCertificateIssueCounter),
                   PlutusScript (PlutusScriptSerialised), ProtocolParameters (..),
                   ReferenceScript (..), ReferenceTxInsScriptsInlineDatumsSupportedInEra (..),
                   StakeCredential (StakeCredentialByKey), StakePoolKey,
                   refInsScriptsAndInlineDatsSupportedInEra)


import           Control.Applicative (optional)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import           Data.Functor (($>), (<&>))
import           Data.Int (Int64)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio (Ratio, (%))
import           Data.String
import           Data.Text (Text)
import           Data.Word (Word64)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.Shelley.TxBody as Ledger (EraIndependentTxBody)

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Numeric.Natural (Natural)

import qualified Cardano.Crypto.Hash.Class as CRYPTO
import           Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Keys (VKey (..))
import           Cardano.Ledger.SafeHash (unsafeMakeSafeHash)

import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId)
import           Test.Gen.Cardano.Api.Metadata (genTxMetadata)

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use let" -}

genAddressByron :: Gen (Address ByronAddr)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley = makeShelleyAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genAddressInEra :: CardanoEra era -> Gen (AddressInEra era)
genAddressInEra era =
  case cardanoEraStyle era of
    LegacyByronEra ->
      byronAddressInEra <$> genAddressByron

    ShelleyBasedEra _ ->
      Gen.choice
        [ byronAddressInEra   <$> genAddressByron
        , shelleyAddressInEra <$> genAddressShelley
        ]

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genLovelace :: Gen Lovelace
genLovelace = Lovelace <$> Gen.integral (Range.linear 0 5000)

genPositiveLovelace :: Gen Lovelace
genPositiveLovelace = Lovelace <$> Gen.integral (Range.linear 1 5000)


----------------------------------------------------------------------------
-- SimpleScript generators
--

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript SimpleScriptLanguage =
    SimpleScript <$> genSimpleScript
genScript (PlutusScriptLanguage lang) =
    PlutusScript lang <$> genPlutusScript lang

genSimpleScript :: Gen SimpleScript
genSimpleScript =
    genTerm
  where
    genTerm = Gen.recursive Gen.choice nonRecursive recursive

    -- Non-recursive generators
    nonRecursive =
      [ RequireSignature . verificationKeyHash <$> genVerificationKey AsPaymentKey
      , RequireTimeBefore <$> genSlotNo
      , RequireTimeAfter <$> genSlotNo
      ]

    -- Recursive generators
    recursive =
      [ RequireAllOf <$> Gen.list (Range.linear 0 10) genTerm

      , RequireAnyOf <$> Gen.list (Range.linear 0 10) genTerm

      , do ts <- Gen.list (Range.linear 0 10) genTerm
           m  <- Gen.integral (Range.constant 0 (List.length ts))
           return (RequireMOf m ts)
      ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> Gen.bytes (Range.linear 0 32)

genScriptDataSchema :: Gen ScriptDataJsonSchema
genScriptDataSchema = Gen.element [ScriptDataJsonNoSchema, ScriptDataJsonDetailedSchema]

genHashableScriptData :: Gen HashableScriptData
genHashableScriptData = do
  sd <- genScriptData
  case deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR sd of
    Left e -> error $ "genHashableScriptData: " <> show e
    Right r -> return r


{-# DEPRECATED genScriptData "Use genHashableScriptData" #-}
genScriptData :: Gen ScriptData
genScriptData =
    Gen.recursive
      Gen.choice
        [ ScriptDataNumber <$> genInteger
        , ScriptDataBytes  <$> genByteString
        ]
        -- The Gen.recursive combinator calls these with the size halved
        [ ScriptDataConstructor <$> genConstructorInteger
                                <*> genScriptDataList
        , ScriptDataList <$> genScriptDataList
        , ScriptDataMap  <$> genScriptDataMap
        ]
  where
    genInteger :: Gen Integer
    genInteger = Gen.integral
                  (Range.linear
                    (-fromIntegral (maxBound :: Word64) :: Integer)
                    (2 * fromIntegral (maxBound :: Word64) :: Integer))


    genConstructorInteger :: Gen Integer
    genConstructorInteger = Gen.integral
                  (Range.linear
                    0 -- TODO: Alonzo should be -> (-fromIntegral (maxBound :: Word64) :: Integer)
                      -- Wrapping bug needs to be fixed in Plutus library
                    (fromIntegral (maxBound :: Word64) :: Integer))

    genByteString :: Gen ByteString
    genByteString = BS.pack <$> Gen.list (Range.linear 0 64)
                                         (Gen.word8 Range.constantBounded)

    genScriptDataList :: Gen [ScriptData]
    genScriptDataList =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) genScriptData

    genScriptDataMap  :: Gen [(ScriptData, ScriptData)]
    genScriptDataMap =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) $
          (,) <$> genScriptData <*> genScriptData


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


----------------------------------------------------------------------------
-- Multi-asset generators
--

genAssetName :: Gen AssetName
genAssetName =
  Gen.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element ["", "a", "b", "c"])
    , (1, AssetName <$> Gen.bytes (Range.singleton  32))
    , (1, AssetName <$> Gen.bytes (Range.constant 1 31))
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

-- | Generate a positive or negative, but not zero quantity.
genSignedNonZeroQuantity :: Gen Quantity
genSignedNonZeroQuantity =
  Gen.choice [ genQuantity (Range.constant (-2) (-1))
             , genQuantity (Range.constant 1 2)
             ]

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = genQuantity (Range.constant 0 2)

genPositiveQuantity :: Gen Quantity
genPositiveQuantity = genQuantity (Range.constant 1 2)

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    Gen.list (Range.constant 0 10)
             ((,) <$> genAId <*> genQuant)

-- | Generate a 'Value' with any asset ID and a positive or negative quantity.
genValueDefault :: Gen Value
genValueDefault = genValue genAssetId genSignedNonZeroQuantity

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedNonZeroQuantity
  where
    genAssetIdNoAda :: Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = do
  -- Generate a potentially empty list with multi assets
  val <- genValue genAssetId genPositiveQuantity
  -- Generate at least one positive ADA, without it Value in TxOut makes no sense
  -- and will fail deserialization starting with ConwayEra
  ada <- (,) AdaAssetId <$> genPositiveQuantity
  pure $ valueFromList (ada : valueToList val)


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

genShelleyHash :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = TxIx . fromIntegral <$> Gen.word16 Range.constantBounded

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra     -> TxOutAdaOnly adaOnlyInEra <$> genPositiveLovelace
    Right multiAssetInEra -> TxOutValue multiAssetInEra <$> genValueForTxOut

genTxOutTxContext :: CardanoEra era -> Gen (TxOut CtxTx era)
genTxOutTxContext era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHashTxContext era
        <*> genReferenceScript era

genTxOutUTxOContext :: CardanoEra era -> Gen (TxOut CtxUTxO era)
genTxOutUTxOContext era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHashUTxOContext era
        <*> genReferenceScript era

genReferenceScript :: CardanoEra era -> Gen (ReferenceScript era)
genReferenceScript era =
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> return ReferenceScriptNone
    Just supp -> ReferenceScript supp <$> genScriptInAnyLang

genUTxO :: CardanoEra era -> Gen (UTxO era)
genUTxO era =
  UTxO <$> Gen.map (Range.constant 0 5) ((,) <$> genTxIn <*> (toCtxUTxOTxOut <$> genTxOutTxContext era))

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (validityUpperBoundSupportedInEra era,
       validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      error "genTxValidityUpperBound: unexpected era support combination"

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case txMetadataSupportedInEra era of
    Nothing -> pure TxMetadataNone
    Just supported ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra supported <$> genTxMetadata
        ]

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
      TxAuxScripts supported <$>
        Gen.list (Range.linear 0 3)
                 (genScriptInEra era)

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing -> pure TxWithdrawalsNone
    Just supported ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals supported mempty)
          -- TODO: Generate withdrawals
        ]

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era =
  case certificatesSupportedInEra era of
    Nothing -> pure TxCertificatesNone
    Just supported -> do
      certs <- Gen.list (Range.constant 0 3) genCertificate
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates supported certs $ BuildTxWith mempty)
          -- TODO: Generate certificates
        ]

-- TODO: Add remaining certificates
genCertificate :: Gen Certificate
genCertificate =
  Gen.choice
    [ StakeAddressRegistrationCertificate <$> genStakeCredential
    , StakeAddressDeregistrationCertificate <$> genStakeCredential
    ]

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal era =
  case updateProposalSupportedInEra era of
    Nothing -> pure TxUpdateProposalNone
    Just supported ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , TxUpdateProposal supported <$> genUpdateProposal era
        ]

genTxMintValue :: CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported ->
      Gen.choice
        [ pure TxMintNone
        , TxMintValue supported <$> genValueForMinting <*> return (BuildTxWith mempty)
        ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
  txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) genTxIn
  txInsCollateral <- genTxInsCollateral era
  txInsReference <- genTxInsReference era
  txOuts <-
    Gen.list (Range.constant 1 10) (genTxOutTxContext era)
    <&> fixDatumHashCollisions
      -- Without this fix, generated script data may have the same hashes for
      -- both present (value + hash) and non-present (hash only) values.
  txTotalCollateral <- genTxTotalCollateral era
  txReturnCollateral <- genTxReturnCollateral era
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <- BuildTxWith <$> Gen.maybe (genProtocolParameters era)
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era

  pure $ TxBodyContent
    { Api.txIns
    , Api.txInsCollateral
    , Api.txInsReference
    , Api.txOuts
    , Api.txTotalCollateral
    , Api.txReturnCollateral
    , Api.txFee
    , Api.txValidityRange
    , Api.txMetadata
    , Api.txAuxScripts
    , Api.txExtraKeyWits
    , Api.txProtocolParams
    , Api.txWithdrawals
    , Api.txCertificates
    , Api.txUpdateProposal
    , Api.txMintValue
    , Api.txScriptValidity
    }


-- | Ensure that all script data with the same hash are
-- either all presented as values or all presented as hashes.
--
-- It's possible to have a hash without its datum, and also with its datum,
-- and these two possibilities are semantically equivalent.
fixDatumHashCollisions :: forall era. [TxOut CtxTx era] -> [TxOut CtxTx era]
fixDatumHashCollisions outs = map replaceOutHashWithItsDatum outs
  where
    replaceOutHashWithItsDatum :: TxOut CtxTx era -> TxOut CtxTx era
    replaceOutHashWithItsDatum (TxOut address value datum script) =
      TxOut address value (replaceHashWithItsDatum datum) script

    replaceHashWithItsDatum :: TxOutDatum CtxTx era -> TxOutDatum CtxTx era
    replaceHashWithItsDatum datum =
      case datum of
        TxOutDatumHash _ hash -> fromMaybe datum $ Map.lookup hash hashedData
        _ -> datum

    hashedData :: Map (Hash ScriptData) (TxOutDatum CtxTx era)
    hashedData =
      Map.fromList
        [ (hashScriptDataBytes scriptData, datum)
        | TxOut _ _ datum@(TxOutDatumInTx _ scriptData) _ <- outs
        ]


genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> Gen.choice
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> Gen.list (Range.linear 0 10) genTxIn
                          ]

genTxInsReference :: CardanoEra era -> Gen (TxInsReference BuildTx era)
genTxInsReference era =
    case refInsScriptsAndInlineDatsSupportedInEra era of
      Nothing        -> pure TxInsReferenceNone
      Just supported -> TxInsReference supported <$> Gen.list (Range.linear 0 10) genTxIn

genTxReturnCollateral :: CardanoEra era -> Gen (TxReturnCollateral CtxTx era)
genTxReturnCollateral era =
  case totalAndReturnCollateralSupportedInEra  era of
    Nothing -> return TxReturnCollateralNone
    Just supp ->
      TxReturnCollateral supp <$>  genTxOutTxContext era

genTxTotalCollateral :: CardanoEra era -> Gen (TxTotalCollateral era)
genTxTotalCollateral era =
  case totalAndReturnCollateralSupportedInEra  era of
    Nothing -> return TxTotalCollateralNone
    Just supp ->
      TxTotalCollateral supp <$> genPositiveLovelace

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  supported -> pure (TxFeeImplicit supported)
    Right supported -> TxFeeExplicit supported <$> genLovelace

genTxBody :: IsCardanoEra era => CardanoEra era -> Gen (TxBody era)
genTxBody era = do
  res <- Api.createAndValidateTransactionBody <$> genTxBodyContent era
  case res of
    Left err -> fail (displayError err)
    Right txBody -> pure txBody

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = Gen.element [ScriptInvalid, ScriptValid]

genTx :: forall era. IsCardanoEra era => CardanoEra era -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnesses era
    <*> genTxBody era

genWitnesses :: CardanoEra era -> Gen [KeyWitness era]
genWitnesses era =
  case cardanoEraStyle era of
    LegacyByronEra    -> Gen.list (Range.constant 1 10) genByronKeyWitness
    ShelleyBasedEra _ -> do
      bsWits  <- Gen.list (Range.constant 0 10)
                          (genShelleyBootstrapWitness era)
      keyWits <- Gen.list (Range.constant 0 10)
                          (genShelleyKeyWitness era)
      return $ bsWits ++ keyWits

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genByronKeyWitness :: Gen (KeyWitness ByronEra)
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
  -> Gen (KeyWitness era)
genShelleyBootstrapWitness era =
 makeShelleyBootstrapWitness
   <$> genWitnessNetworkIdOrByronAddress
   <*> genTxBody era
   <*> genSigningKey AsByronKey

genShelleyKeyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyKeyWitness era =
  makeShelleyKeyWitness
    <$> genTxBody era
    <*> genShelleyWitnessSigningKey

genShelleyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
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

genCardanoKeyWitness
  :: CardanoEra era
  -> Gen (KeyWitness era)
genCardanoKeyWitness era = case cardanoEraStyle era of
  LegacyByronEra -> genByronKeyWitness
  ShelleyBasedEra _ -> genShelleyWitness era

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

genRational :: Gen Rational
genRational =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Word64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Word64 -> Rational
    ratioToRational = toRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Int64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Int64 -> Rational
    ratioToRational = toRational

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 10)

genPraosNonce :: Gen PraosNonce
genPraosNonce = makePraosNonce <$> Gen.bytes (Range.linear 0 32)

genMaybePraosNonce :: Gen (Maybe PraosNonce)
genMaybePraosNonce = Gen.maybe genPraosNonce

genProtocolParameters :: CardanoEra era -> Gen ProtocolParameters
genProtocolParameters era = do
  protocolParamProtocolVersion <- (,) <$> genNat <*> genNat
  protocolParamDecentralization <- Gen.maybe genRational
  protocolParamExtraPraosEntropy <- genMaybePraosNonce
  protocolParamMaxBlockHeaderSize <- genNat
  protocolParamMaxBlockBodySize <- genNat
  protocolParamMaxTxSize <- genNat
  protocolParamTxFeeFixed <- genLovelace
  protocolParamTxFeePerByte <- genLovelace
  protocolParamMinUTxOValue <- Gen.maybe genLovelace
  protocolParamStakeAddressDeposit <- genLovelace
  protocolParamStakePoolDeposit <- genLovelace
  protocolParamMinPoolCost <- genLovelace
  protocolParamPoolRetireMaxEpoch <- genEpochNo
  protocolParamStakePoolTargetNum <- genNat
  protocolParamPoolPledgeInfluence <- genRationalInt64
  protocolParamMonetaryExpansion <- genRational
  protocolParamTreasuryCut <- genRational
  protocolParamUTxOCostPerWord <- sequence $ protocolUTxOCostPerWordSupportedInEra era $> genLovelace
  protocolParamCostModels <- pure mempty
  --TODO: Babbage figure out how to deal with
  -- asymmetric cost model JSON instances
  protocolParamPrices <- Gen.maybe genExecutionUnitPrices
  protocolParamMaxTxExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxBlockExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxValueSize <- Gen.maybe genNat
  protocolParamCollateralPercent <- Gen.maybe genNat
  protocolParamMaxCollateralInputs <- Gen.maybe genNat
  protocolParamUTxOCostPerByte <- sequence $ protocolUTxOCostPerByteSupportedInEra era $> genLovelace

  pure ProtocolParameters {..}

genProtocolParametersUpdate :: CardanoEra era -> Gen ProtocolParametersUpdate
genProtocolParametersUpdate era = do
  protocolUpdateProtocolVersion     <- Gen.maybe ((,) <$> genNat <*> genNat)
  protocolUpdateDecentralization    <- Gen.maybe genRational
  protocolUpdateExtraPraosEntropy   <- Gen.maybe genMaybePraosNonce
  protocolUpdateMaxBlockHeaderSize  <- Gen.maybe genNat
  protocolUpdateMaxBlockBodySize    <- Gen.maybe genNat
  protocolUpdateMaxTxSize           <- Gen.maybe genNat
  protocolUpdateTxFeeFixed          <- Gen.maybe genLovelace
  protocolUpdateTxFeePerByte        <- Gen.maybe genLovelace
  protocolUpdateMinUTxOValue        <- preAlonzoParam era genLovelace
  protocolUpdateStakeAddressDeposit <- Gen.maybe genLovelace
  protocolUpdateStakePoolDeposit    <- Gen.maybe genLovelace
  protocolUpdateMinPoolCost         <- Gen.maybe genLovelace
  protocolUpdatePoolRetireMaxEpoch  <- Gen.maybe genEpochNo
  protocolUpdateStakePoolTargetNum  <- Gen.maybe genNat
  protocolUpdatePoolPledgeInfluence <- Gen.maybe genRationalInt64
  protocolUpdateMonetaryExpansion   <- Gen.maybe genRational
  protocolUpdateTreasuryCut         <- Gen.maybe genRational
  protocolUpdateUTxOCostPerWord     <- sequence $ protocolUTxOCostPerWordSupportedInEra era $> genLovelace
  protocolUpdateCostModels          <- genCostModels era
  protocolUpdatePrices              <- alonzoParam era genExecutionUnitPrices
  protocolUpdateMaxTxExUnits        <- alonzoParam era genExecutionUnits
  protocolUpdateMaxBlockExUnits     <- alonzoParam era genExecutionUnits
  protocolUpdateMaxValueSize        <- alonzoParam era genNat
  protocolUpdateCollateralPercent   <- alonzoParam era genNat
  protocolUpdateMaxCollateralInputs <- alonzoParam era genNat
  protocolUpdateUTxOCostPerByte     <- sequence $ protocolUTxOCostPerByteSupportedInEra era $> genLovelace

  pure ProtocolParametersUpdate{..}

genUpdateProposal :: CardanoEra era -> Gen UpdateProposal
genUpdateProposal era =
  UpdateProposal
    <$> Gen.map (Range.constant 1 3)
        ( (,)
          <$> genVerificationKeyHash AsGenesisKey
          <*> genProtocolParametersUpdate era
        )
    <*> genEpochNo

exampleCostModelV1 :: Map Text Integer
exampleCostModelV1 = Map.fromList
  [ ("sha2_256-memory-arguments", 0)
  , ("equalsString-cpu-arguments-constant", 0)
  , ("cekDelayCost-exBudgetMemory", 0)
  , ("lessThanEqualsByteString-cpu-arguments-intercept", 0)
  , ("divideInteger-memory-arguments-minimum", 0)
  , ("appendByteString-cpu-arguments-slope", 0)
  , ("blake2b-cpu-arguments-slope", 0)
  , ("iData-cpu-arguments", 0)
  , ("encodeUtf8-cpu-arguments-slope", 0)
  , ("unBData-cpu-arguments", 0)
  , ("multiplyInteger-cpu-arguments-intercept", 0)
  , ("cekConstCost-exBudgetMemory", 0)
  , ("nullList-cpu-arguments", 0)
  , ("equalsString-cpu-arguments-intercept", 0)
  , ("trace-cpu-arguments", 0)
  , ("mkNilData-memory-arguments", 0)
  , ("lengthOfByteString-cpu-arguments", 0)
  , ("cekBuiltinCost-exBudgetCPU", 0)
  , ("bData-cpu-arguments", 0)
  , ("subtractInteger-cpu-arguments-slope", 0)
  , ("unIData-cpu-arguments", 0)
  , ("consByteString-memory-arguments-intercept", 0)
  , ("divideInteger-memory-arguments-slope", 0)
  , ("divideInteger-cpu-arguments-model-arguments-slope", 0)
  , ("listData-cpu-arguments", 0)
  , ("headList-cpu-arguments", 0)
  , ("chooseData-memory-arguments", 0)
  , ("equalsInteger-cpu-arguments-intercept", 0)
  , ("sha3_256-cpu-arguments-slope", 0)
  , ("sliceByteString-cpu-arguments-slope", 0)
  , ("unMapData-cpu-arguments", 0)
  , ("lessThanInteger-cpu-arguments-intercept", 0)
  , ("mkCons-cpu-arguments", 0)
  , ("appendString-memory-arguments-intercept", 0)
  , ("modInteger-cpu-arguments-model-arguments-slope", 0)
  , ("ifThenElse-cpu-arguments", 0)
  , ("mkNilPairData-cpu-arguments", 0)
  , ("lessThanEqualsInteger-cpu-arguments-intercept", 0)
  , ("addInteger-memory-arguments-slope", 0)
  , ("chooseList-memory-arguments", 0)
  , ("constrData-memory-arguments", 0)
  , ("decodeUtf8-cpu-arguments-intercept", 0)
  , ("equalsData-memory-arguments", 0)
  , ("subtractInteger-memory-arguments-slope", 0)
  , ("appendByteString-memory-arguments-intercept", 0)
  , ("lengthOfByteString-memory-arguments", 0)
  , ("headList-memory-arguments", 0)
  , ("listData-memory-arguments", 0)
  , ("consByteString-cpu-arguments-intercept", 0)
  , ("unIData-memory-arguments", 0)
  , ("remainderInteger-memory-arguments-minimum", 0)
  , ("bData-memory-arguments", 0)
  , ("lessThanByteString-cpu-arguments-slope", 0)
  , ("encodeUtf8-memory-arguments-intercept", 0)
  , ("cekStartupCost-exBudgetCPU", 0)
  , ("multiplyInteger-memory-arguments-intercept", 0)
  , ("unListData-memory-arguments", 0)
  , ("remainderInteger-cpu-arguments-model-arguments-slope", 0)
  , ("cekVarCost-exBudgetCPU", 0)
  , ("remainderInteger-memory-arguments-slope", 0)
  , ("cekForceCost-exBudgetCPU", 0)
  , ("sha2_256-cpu-arguments-slope", 0)
  , ("equalsInteger-memory-arguments", 0)
  , ("indexByteString-memory-arguments", 0)
  , ("addInteger-memory-arguments-intercept", 0)
  , ("chooseUnit-cpu-arguments", 0)
  , ("sndPair-cpu-arguments", 0)
  , ("cekLamCost-exBudgetCPU", 0)
  , ("fstPair-cpu-arguments", 0)
  , ("quotientInteger-memory-arguments-minimum", 0)
  , ("decodeUtf8-cpu-arguments-slope", 0)
  , ("lessThanInteger-memory-arguments", 0)
  , ("lessThanEqualsInteger-cpu-arguments-slope", 0)
  , ("fstPair-memory-arguments", 0)
  , ("modInteger-memory-arguments-intercept", 0)
  , ("unConstrData-cpu-arguments", 0)
  , ("lessThanEqualsInteger-memory-arguments", 0)
  , ("chooseUnit-memory-arguments", 0)
  , ("sndPair-memory-arguments", 0)
  , ("addInteger-cpu-arguments-intercept", 0)
  , ("decodeUtf8-memory-arguments-slope", 0)
  , ("equalsData-cpu-arguments-intercept", 0)
  , ("mapData-cpu-arguments", 0)
  , ("mkPairData-cpu-arguments", 0)
  , ("quotientInteger-cpu-arguments-constant", 0)
  , ("consByteString-memory-arguments-slope", 0)
  , ("cekVarCost-exBudgetMemory", 0)
  , ("indexByteString-cpu-arguments", 0)
  , ("unListData-cpu-arguments", 0)
  , ("equalsInteger-cpu-arguments-slope", 0)
  , ("cekStartupCost-exBudgetMemory", 0)
  , ("subtractInteger-cpu-arguments-intercept", 0)
  , ("divideInteger-cpu-arguments-model-arguments-intercept", 0)
  , ("divideInteger-memory-arguments-intercept", 0)
  , ("cekForceCost-exBudgetMemory", 0)
  , ("blake2b-cpu-arguments-intercept", 0)
  , ("remainderInteger-cpu-arguments-constant", 0)
  , ("tailList-cpu-arguments", 0)
  , ("encodeUtf8-cpu-arguments-intercept", 0)
  , ("equalsString-cpu-arguments-slope", 0)
  , ("lessThanByteString-memory-arguments", 0)
  , ("multiplyInteger-cpu-arguments-slope", 0)
  , ("appendByteString-cpu-arguments-intercept", 0)
  , ("lessThanEqualsByteString-cpu-arguments-slope", 0)
  , ("modInteger-memory-arguments-slope", 0)
  , ("addInteger-cpu-arguments-slope", 0)
  , ("equalsData-cpu-arguments-slope", 0)
  , ("decodeUtf8-memory-arguments-intercept", 0)
  , ("chooseList-cpu-arguments", 0)
  , ("constrData-cpu-arguments", 0)
  , ("equalsByteString-memory-arguments", 0)
  , ("cekApplyCost-exBudgetCPU", 0)
  , ("quotientInteger-memory-arguments-slope", 0)
  , ("verifySignature-cpu-arguments-intercept", 0)
  , ("unMapData-memory-arguments", 0)
  , ("mkCons-memory-arguments", 0)
  , ("sliceByteString-memory-arguments-slope", 0)
  , ("sha3_256-memory-arguments", 0)
  , ("ifThenElse-memory-arguments", 0)
  , ("mkNilPairData-memory-arguments", 0)
  , ("equalsByteString-cpu-arguments-slope", 0)
  , ("appendString-cpu-arguments-intercept", 0)
  , ("quotientInteger-cpu-arguments-model-arguments-slope", 0)
  , ("cekApplyCost-exBudgetMemory", 0)
  , ("equalsString-memory-arguments", 0)
  , ("multiplyInteger-memory-arguments-slope", 0)
  , ("cekBuiltinCost-exBudgetMemory", 0)
  , ("remainderInteger-memory-arguments-intercept", 0)
  , ("sha2_256-cpu-arguments-intercept", 0)
  , ("remainderInteger-cpu-arguments-model-arguments-intercept", 0)
  , ("lessThanEqualsByteString-memory-arguments", 0)
  , ("tailList-memory-arguments", 0)
  , ("mkNilData-cpu-arguments", 0)
  , ("chooseData-cpu-arguments", 0)
  , ("unBData-memory-arguments", 0)
  , ("blake2b-memory-arguments", 0)
  , ("iData-memory-arguments", 0)
  , ("nullList-memory-arguments", 0)
  , ("cekDelayCost-exBudgetCPU", 0)
  , ("subtractInteger-memory-arguments-intercept", 0)
  , ("lessThanByteString-cpu-arguments-intercept", 0)
  , ("consByteString-cpu-arguments-slope", 0)
  , ("appendByteString-memory-arguments-slope", 0)
  , ("trace-memory-arguments", 0)
  , ("divideInteger-cpu-arguments-constant", 0)
  , ("cekConstCost-exBudgetCPU", 0)
  , ("encodeUtf8-memory-arguments-slope", 0)
  , ("quotientInteger-cpu-arguments-model-arguments-intercept", 0)
  , ("mapData-memory-arguments", 0)
  , ("appendString-cpu-arguments-slope", 0)
  , ("modInteger-cpu-arguments-constant", 0)
  , ("verifySignature-cpu-arguments-slope", 0)
  , ("unConstrData-memory-arguments", 0)
  , ("quotientInteger-memory-arguments-intercept", 0)
  , ("equalsByteString-cpu-arguments-constant", 0)
  , ("sliceByteString-memory-arguments-intercept", 0)
  , ("mkPairData-memory-arguments", 0)
  , ("equalsByteString-cpu-arguments-intercept", 0)
  , ("appendString-memory-arguments-slope", 0)
  , ("lessThanInteger-cpu-arguments-slope", 0)
  , ("modInteger-cpu-arguments-model-arguments-intercept", 0)
  , ("modInteger-memory-arguments-minimum", 0)
  , ("sha3_256-cpu-arguments-intercept", 0)
  , ("verifySignature-memory-arguments", 0)
  , ("cekLamCost-exBudgetMemory", 0)
  , ("sliceByteString-cpu-arguments-intercept", 0)
  ]

exampleCostModelV2 :: Map Text Integer
exampleCostModelV2 = Map.fromList
  [ ( "addInteger-cpu-arguments-intercept", 0)
  , ( "addInteger-cpu-arguments-slope", 0)
  , ( "addInteger-memory-arguments-intercept", 0)
  , ( "addInteger-memory-arguments-slope", 0)
  , ( "appendByteString-cpu-arguments-intercept", 0)
  , ( "appendByteString-cpu-arguments-slope", 0)
  , ( "appendByteString-memory-arguments-intercept", 0)
  , ( "appendByteString-memory-arguments-slope", 0)
  , ( "appendString-cpu-arguments-intercept", 0)
  , ( "appendString-cpu-arguments-slope", 0)
  , ( "appendString-memory-arguments-intercept", 0)
  , ( "appendString-memory-arguments-slope", 0)
  , ( "bData-cpu-arguments", 0)
  , ( "bData-memory-arguments", 0)
  , ( "blake2b_256-cpu-arguments-intercept", 0)
  , ( "blake2b_256-cpu-arguments-slope", 0)
  , ( "blake2b_256-memory-arguments", 0)
  , ( "cekApplyCost-exBudgetCPU", 0)
  , ( "cekApplyCost-exBudgetMemory", 0)
  , ( "cekBuiltinCost-exBudgetCPU", 0)
  , ( "cekBuiltinCost-exBudgetMemory", 0)
  , ( "cekConstCost-exBudgetCPU", 0)
  , ( "cekConstCost-exBudgetMemory", 0)
  , ( "cekDelayCost-exBudgetCPU", 0)
  , ( "cekDelayCost-exBudgetMemory", 0)
  , ( "cekForceCost-exBudgetCPU", 0)
  , ( "cekForceCost-exBudgetMemory", 0)
  , ( "cekLamCost-exBudgetCPU", 0)
  , ( "cekLamCost-exBudgetMemory", 0)
  , ( "cekStartupCost-exBudgetCPU", 0)
  , ( "cekStartupCost-exBudgetMemory", 0)
  , ( "cekVarCost-exBudgetCPU", 0)
  , ( "cekVarCost-exBudgetMemory", 0)
  , ( "chooseData-cpu-arguments", 0)
  , ( "chooseData-memory-arguments", 0)
  , ( "chooseList-cpu-arguments", 0)
  , ( "chooseList-memory-arguments", 0)
  , ( "chooseUnit-cpu-arguments", 0)
  , ( "chooseUnit-memory-arguments", 0)
  , ( "consByteString-cpu-arguments-intercept", 0)
  , ( "consByteString-cpu-arguments-slope", 0)
  , ( "consByteString-memory-arguments-intercept", 0)
  , ( "consByteString-memory-arguments-slope", 0)
  , ( "constrData-cpu-arguments", 0)
  , ( "constrData-memory-arguments", 0)
  , ( "decodeUtf8-cpu-arguments-intercept", 0)
  , ( "decodeUtf8-cpu-arguments-slope", 0)
  , ( "decodeUtf8-memory-arguments-intercept", 0)
  , ( "decodeUtf8-memory-arguments-slope", 0)
  , ( "divideInteger-cpu-arguments-constant", 0)
  , ( "divideInteger-cpu-arguments-model-arguments-intercept", 0)
  , ( "divideInteger-cpu-arguments-model-arguments-slope", 0)
  , ( "divideInteger-memory-arguments-intercept", 0)
  , ( "divideInteger-memory-arguments-minimum", 0)
  , ( "divideInteger-memory-arguments-slope", 0)
  , ( "encodeUtf8-cpu-arguments-intercept", 0)
  , ( "encodeUtf8-cpu-arguments-slope", 0)
  , ( "encodeUtf8-memory-arguments-intercept", 0)
  , ( "encodeUtf8-memory-arguments-slope", 0)
  , ( "equalsByteString-cpu-arguments-constant", 0)
  , ( "equalsByteString-cpu-arguments-intercept", 0)
  , ( "equalsByteString-cpu-arguments-slope", 0)
  , ( "equalsByteString-memory-arguments", 0)
  , ( "equalsData-cpu-arguments-intercept", 0)
  , ( "equalsData-cpu-arguments-slope", 0)
  , ( "equalsData-memory-arguments", 0)
  , ( "equalsInteger-cpu-arguments-intercept", 0)
  , ( "equalsInteger-cpu-arguments-slope", 0)
  , ( "equalsInteger-memory-arguments", 0)
  , ( "equalsString-cpu-arguments-constant", 0)
  , ( "equalsString-cpu-arguments-intercept", 0)
  , ( "equalsString-cpu-arguments-slope", 0)
  , ( "equalsString-memory-arguments", 0)
  , ( "fstPair-cpu-arguments", 0)
  , ( "fstPair-memory-arguments", 0)
  , ( "headList-cpu-arguments", 0)
  , ( "headList-memory-arguments", 0)
  , ( "iData-cpu-arguments", 0)
  , ( "iData-memory-arguments", 0)
  , ( "ifThenElse-cpu-arguments", 0)
  , ( "ifThenElse-memory-arguments", 0)
  , ( "indexByteString-cpu-arguments", 0)
  , ( "indexByteString-memory-arguments", 0)
  , ( "lengthOfByteString-cpu-arguments", 0)
  , ( "lengthOfByteString-memory-arguments", 0)
  , ( "lessThanByteString-cpu-arguments-intercept", 0)
  , ( "lessThanByteString-cpu-arguments-slope", 0)
  , ( "lessThanByteString-memory-arguments", 0)
  , ( "lessThanEqualsByteString-cpu-arguments-intercept", 0)
  , ( "lessThanEqualsByteString-cpu-arguments-slope", 0)
  , ( "lessThanEqualsByteString-memory-arguments", 0)
  , ( "lessThanEqualsInteger-cpu-arguments-intercept", 0)
  , ( "lessThanEqualsInteger-cpu-arguments-slope", 0)
  , ( "lessThanEqualsInteger-memory-arguments", 0)
  , ( "lessThanInteger-cpu-arguments-intercept", 0)
  , ( "lessThanInteger-cpu-arguments-slope", 0)
  , ( "lessThanInteger-memory-arguments", 0)
  , ( "listData-cpu-arguments", 0)
  , ( "listData-memory-arguments", 0)
  , ( "mapData-cpu-arguments", 0)
  , ( "mapData-memory-arguments", 0)
  , ( "mkCons-cpu-arguments", 0)
  , ( "mkCons-memory-arguments", 0)
  , ( "mkNilData-cpu-arguments", 0)
  , ( "mkNilData-memory-arguments", 0)
  , ( "mkNilPairData-cpu-arguments", 0)
  , ( "mkNilPairData-memory-arguments", 0)
  , ( "mkPairData-cpu-arguments", 0)
  , ( "mkPairData-memory-arguments", 0)
  , ( "modInteger-cpu-arguments-constant", 0)
  , ( "modInteger-cpu-arguments-model-arguments-intercept", 0)
  , ( "modInteger-cpu-arguments-model-arguments-slope", 0)
  , ( "modInteger-memory-arguments-intercept", 0)
  , ( "modInteger-memory-arguments-minimum", 0)
  , ( "modInteger-memory-arguments-slope", 0)
  , ( "multiplyInteger-cpu-arguments-intercept", 0)
  , ( "multiplyInteger-cpu-arguments-slope", 0)
  , ( "multiplyInteger-memory-arguments-intercept", 0)
  , ( "multiplyInteger-memory-arguments-slope", 0)
  , ( "nullList-cpu-arguments", 0)
  , ( "nullList-memory-arguments", 0)
  , ( "quotientInteger-cpu-arguments-constant", 0)
  , ( "quotientInteger-cpu-arguments-model-arguments-intercept", 0)
  , ( "quotientInteger-cpu-arguments-model-arguments-slope", 0)
  , ( "quotientInteger-memory-arguments-intercept", 0)
  , ( "quotientInteger-memory-arguments-minimum", 0)
  , ( "quotientInteger-memory-arguments-slope", 0)
  , ( "remainderInteger-cpu-arguments-constant", 0)
  , ( "remainderInteger-cpu-arguments-model-arguments-intercept", 0)
  , ( "remainderInteger-cpu-arguments-model-arguments-slope", 0)
  , ( "remainderInteger-memory-arguments-intercept", 0)
  , ( "remainderInteger-memory-arguments-minimum", 0)
  , ( "remainderInteger-memory-arguments-slope", 0)
  , ( "serialiseData-cpu-arguments-intercept", 0)
  , ( "serialiseData-cpu-arguments-slope", 0)
  , ( "serialiseData-memory-arguments-intercept", 0)
  , ( "serialiseData-memory-arguments-slope", 0)
  , ( "sha2_256-cpu-arguments-intercept", 0)
  , ( "sha2_256-cpu-arguments-slope", 0)
  , ( "sha2_256-memory-arguments", 0)
  , ( "sha3_256-cpu-arguments-intercept", 0)
  , ( "sha3_256-cpu-arguments-slope", 0)
  , ( "sha3_256-memory-arguments", 0)
  , ( "sliceByteString-cpu-arguments-intercept", 0)
  , ( "sliceByteString-cpu-arguments-slope", 0)
  , ( "sliceByteString-memory-arguments-intercept", 0)
  , ( "sliceByteString-memory-arguments-slope", 0)
  , ( "sndPair-cpu-arguments", 0)
  , ( "sndPair-memory-arguments", 0)
  , ( "subtractInteger-cpu-arguments-intercept", 0)
  , ( "subtractInteger-cpu-arguments-slope", 0)
  , ( "subtractInteger-memory-arguments-intercept", 0)
  , ( "subtractInteger-memory-arguments-slope", 0)
  , ( "tailList-cpu-arguments", 0)
  , ( "tailList-memory-arguments", 0)
  , ( "trace-cpu-arguments", 0)
  , ( "trace-memory-arguments", 0)
  , ( "unBData-cpu-arguments", 0)
  , ( "unBData-memory-arguments", 0)
  , ( "unConstrData-cpu-arguments", 0)
  , ( "unConstrData-memory-arguments", 0)
  , ( "unIData-cpu-arguments", 0)
  , ( "unIData-memory-arguments", 0)
  , ( "unListData-cpu-arguments", 0)
  , ( "unListData-memory-arguments", 0)
  , ( "unMapData-cpu-arguments", 0)
  , ( "unMapData-memory-arguments", 0)
  , ( "verifyEcdsaSecp256k1Signature-cpu-arguments", 0)
  , ( "verifyEcdsaSecp256k1Signature-memory-arguments", 0)
  , ( "verifyEd25519Signature-cpu-arguments-intercept", 0)
  , ( "verifyEd25519Signature-cpu-arguments-slope", 0)
  , ( "verifyEd25519Signature-memory-arguments", 0)
  , ( "verifySchnorrSecp256k1Signature-cpu-arguments-intercept", 0)
  , ( "verifySchnorrSecp256k1Signature-cpu-arguments-slope", 0)
  , ( "verifySchnorrSecp256k1Signature-memory-arguments", 0)
  ]

genCostModelParams :: Map Text Integer -> Gen [Integer]
genCostModelParams costModelParams =
  Gen.list (Range.singleton (Map.size costModelParams)) (Gen.integral (Range.linear 0 5000))

genCostModel :: Language -> Gen Alonzo.CostModel
-- genCostModel :: Language -> Gen CostModel
genCostModel lang = do
  let costModelParams = case lang of
        PlutusV1 -> exampleCostModelV1
        PlutusV2 -> exampleCostModelV2

  eCostModel <- Alonzo.mkCostModel
    <$> genPlutusLanguage
    <*> genCostModelParams costModelParams

  case eCostModel of
    Left err -> error $ "genCostModel: " <> show err
    Right cModel -> return cModel

genPlutusLanguage :: Gen Language
genPlutusLanguage = Gen.element [PlutusV1, PlutusV2]

genCostModels :: CardanoEra era -> Gen (Map AnyPlutusScriptVersion CostModel)
genCostModels era
  | anyCardanoEra era >= anyCardanoEra AlonzoEra = do
      lang <- genPlutusLanguage
      Gen.map (Range.linear 0 (List.length plutusScriptVersions)) $
        (,)
          <$> Gen.element plutusScriptVersions
          <*> (Api.fromAlonzoCostModel <$> genCostModel lang)
      -- Gen.map
      --   (Range.linear 0 (length plutusScriptVersions))
      --   ((fromAlonzoScriptLanguage lang,) <$> genCostModel lang)
  | otherwise = pure Map.empty
  where
    plutusScriptVersions :: [AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)

-- | Gen for Alonzo-specific parameters
alonzoParam :: CardanoEra era -> Gen a -> Gen (Maybe a)
alonzoParam era gen
  | anyCardanoEra era >= anyCardanoEra AlonzoEra = Just <$> gen
  | otherwise = pure Nothing

-- | 'Gen.maybe' but with condition if era is not Alonzo-based
preAlonzoParam :: CardanoEra era -> Gen a -> Gen (Maybe a)
preAlonzoParam era gen
  | anyCardanoEra era >= anyCardanoEra AlonzoEra = pure Nothing
  | otherwise = Gen.maybe gen

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHashTxContext :: CardanoEra era -> Gen (TxOutDatum CtxTx era)
genTxOutDatumHashTxContext era = case era of
    ByronEra   -> pure TxOutDatumNone
    ShelleyEra -> pure TxOutDatumNone
    AllegraEra -> pure TxOutDatumNone
    MaryEra    -> pure TxOutDatumNone
    AlonzoEra  -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInAlonzoEra <$> genHashScriptData
                    , TxOutDatumInTx ScriptDataInAlonzoEra <$> genHashableScriptData
                    ]
    BabbageEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInBabbageEra <$> genHashScriptData
                    , TxOutDatumInTx ScriptDataInBabbageEra <$> genHashableScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> genHashableScriptData
                    ]
    ConwayEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInConwayEra <$> genHashScriptData
                    , TxOutDatumInTx ScriptDataInConwayEra <$> genHashableScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInConwayEra <$> genHashableScriptData
                    ]

genTxOutDatumHashUTxOContext :: CardanoEra era -> Gen (TxOutDatum CtxUTxO era)
genTxOutDatumHashUTxOContext era = case era of
    ByronEra   -> pure TxOutDatumNone
    ShelleyEra -> pure TxOutDatumNone
    AllegraEra -> pure TxOutDatumNone
    MaryEra    -> pure TxOutDatumNone
    AlonzoEra  -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInAlonzoEra <$> genHashScriptData
                    ]
    BabbageEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInBabbageEra <$> genHashScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> genHashableScriptData
                    ]
    ConwayEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInConwayEra <$> genHashScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInConwayEra <$> genHashableScriptData
                    ]

mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Cardano.Api.Hash ScriptData)
genHashScriptData = ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

genScriptDataSupportedInAlonzoEra :: Gen (ScriptDataSupportedInEra AlonzoEra)
genScriptDataSupportedInAlonzoEra = pure ScriptDataInAlonzoEra

genGovernancePoll :: Gen GovernancePoll
genGovernancePoll =
  GovernancePoll
    <$> Gen.text (Range.linear 1 255) Gen.unicodeAll
    <*> Gen.list (Range.constant 1 10) (Gen.text (Range.linear 1 255) Gen.unicodeAll)
    <*> optional (Gen.word (Range.constant 0 100))

genGovernancePollAnswer :: Gen GovernancePollAnswer
genGovernancePollAnswer =
  GovernancePollAnswer
    <$> genGovernancePollHash
    <*> Gen.word (Range.constant 0 10)
 where
   genGovernancePollHash =
     GovernancePollHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

genGovernancePollWitness :: Gen GovernancePollWitness
genGovernancePollWitness =
  Gen.choice
    [ GovernancePollWitnessVRF
        <$> fmap
              unsafeDeserialiseVerKeyVRF
              (Gen.bytes $ Range.singleton 32)
        <*> fmap
              unsafeDeserialiseCertVRF
              (Gen.bytes $ Range.singleton 80)
    , GovernancePollWitnessColdKey
        <$> fmap
              (VKey . unsafeDeserialiseVerKeyDSIGN)
              (Gen.bytes $ Range.singleton 32)
        <*> fmap
              (DSIGN.SignedDSIGN . unsafeDeserialiseSigDSIGN)
              (Gen.bytes $ Range.singleton 64)
    ]
 where
  unsafeDeserialiseVerKeyVRF =
    fromMaybe (error "unsafeDeserialiseVerKeyVRF") . VRF.rawDeserialiseVerKeyVRF
  unsafeDeserialiseCertVRF =
    fromMaybe (error "unsafeDeserialiseCertVRF") . VRF.rawDeserialiseCertVRF

  unsafeDeserialiseVerKeyDSIGN =
    fromMaybe (error "unsafeDeserialiseVerKeyDSIGN") . DSIGN.rawDeserialiseVerKeyDSIGN
  unsafeDeserialiseSigDSIGN =
    fromMaybe (error "unsafeDeserialiseSigDSIGN") . DSIGN.rawDeserialiseSigDSIGN
