{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Api.TxBody (tests) where

import           Control.Monad (when)
import           Data.Bifunctor (second)
import           Data.Data ((:~:) (..))
import           Data.Foldable (toList)
import           Data.Functor ((<&>))
import qualified Data.List as List
import           Data.Maybe.Strict (StrictMaybe (SNothing))
import           Data.Type.Equality (testEquality)
import           Hedgehog (Property, PropertyT, evalEither, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters, TxBody (ShelleyTxBody))
import           Cardano.Binary (serialize')
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Cardano.Ledger.Alonzo.TxBody (adHash)
import qualified Cardano.Ledger.Core as Ledger

import           Ouroboros.Consensus.Shelley.Eras as Ledger (StandardAlonzo)

import           Test.Gen.Cardano.Api.Typed (genTxBodyContent)


-- * Properties

-- | Check that conversion from 'TxBodyContent' to 'TxBody' and back gives
-- result equivalent to original.
--
-- The original randomly-generated data requires
-- BuildTx/ViewTx type conversion (see View section) and
-- normalization (see Normalization section).
--
-- Roundtrip data requires normalization, too.
prop_roundtrip_TxBody_make_get :: Property
prop_roundtrip_TxBody_make_get = property $ do
  AnyCardanoEra era <- forAll $ Gen.element [minBound..maxBound]
  content <- forAll $ genTxBodyContent era
  tripping
    (normalizeContentOriginal $ viewBodyContent content)
    (\_ -> createAndValidateTransactionBody content)
    (<&> \(TxBody content') -> normalizeContentRoundtrip content')


-- | Check that conversion from 'TxBody' to 'TxBodyContent' and back gives
-- result equivalent to original.
--
-- We have no means to generate a random 'TxBody',
-- so we start from 'TxBodyContent' and assume 'TxBody' made from it
-- is arbitrary enough.
--
-- The original randomly-generated data requires
-- ViewTx/BuildTx type conversion (see Build section).
--
-- No normalization is needed here, because 'TxBody' keeps less information
-- on type and value level than 'TxBodyContent'.
-- For instance, no special /None/ values.
prop_roundtrip_TxBody_make_get_make :: Property
prop_roundtrip_TxBody_make_get_make = property $ do
  AnyCardanoEra era <- forAll $ Gen.element [minBound..maxBound]

  -- generate a TxBodyContent to use as a seed
  content0 <- forAll $ genTxBodyContent era
  let TxBodyContent{txProtocolParams} = content0 -- keep for re-building

  -- make an "original" TxBody
  body1 <- evalEither $ createAndValidateTransactionBody content0

  -- convert TxBody back to TxBodyContent
  let TxBody content1 = body1

  -- prepare content1 for making another TxBody
  let content2 = buildBodyContent txProtocolParams content1

  -- make another TxBody -- roundtrip
  body2 <- evalEither $ createAndValidateTransactionBody content2

  assertEqBodies body1 body2


assertEqBodies :: TxBody era -> TxBody era -> PropertyT IO ()
assertEqBodies x y =
  case (x, y) of
    ( ShelleyTxBody ShelleyBasedEraAlonzo xBody xs xd (Just xAux) xv,
      ShelleyTxBody ShelleyBasedEraAlonzo yBody ys yd (Just yAux) yv
      ) -> do
        -- compare aux data separately from the rest
        assertEqAuxData xAux yAux
        x' === y'
        where
          xAux' = Nothing
          yAux' = Nothing
          xBody' = xBody {adHash = SNothing}
          yBody' = yBody {adHash = SNothing}
          x' = ShelleyTxBody ShelleyBasedEraAlonzo xBody' xs xd xAux' xv
          y' = ShelleyTxBody ShelleyBasedEraAlonzo yBody' ys yd yAux' yv
    _ -> x === y

-- | Compare ignoring scripts order
assertEqAuxData
  :: Ledger.AuxiliaryData Ledger.StandardAlonzo
  -> Ledger.AuxiliaryData Ledger.StandardAlonzo
  -> PropertyT IO ()
assertEqAuxData
  (Alonzo.AlonzoAuxiliaryData xMetadata xScripts)
  (Alonzo.AlonzoAuxiliaryData yMetadata yScripts) = do
    when (List.sortOn serialize' (toList xScripts) /= List.sortOn serialize' (toList yScripts)) $
      xScripts === yScripts
    xMetadata === yMetadata


-- * Normalization
--
-- Strip unnecessary details
--
-- In many cases, after roundtrip,
-- @Just mempty@ may become @Nothing@ or vice versa.
-- Input data also may be generated as either @Just 0@ or @Nothing@.
-- Order of some items may also change, they need to be reordered.

-- | Normalizations applied to original body content
normalizeContentOriginal :: TxBodyContent ViewTx era -> TxBodyContent ViewTx era
normalizeContentOriginal content =
  content
    { txAuxScripts = normalizeAuxScripts $ txAuxScripts content
    , txCertificates = normalizeCertificates $ txCertificates content
    , txIns =
        List.nub -- the same txin provided multiply is OK, ignore copies
        $ List.sortOn fst
        $ txIns content
    , txInsCollateral = normalizeInsCollateral $ txInsCollateral content
    , txMetadata = normalizeMetadata $ txMetadata content
    , txMintValue = normalizeMintValue $ txMintValue content
    , txWithdrawals = normalizeWithdrawals $ txWithdrawals content
    }

-- | Normalizations applied to roundtrip result body content
normalizeContentRoundtrip
  :: TxBodyContent ViewTx era -> TxBodyContent ViewTx era
normalizeContentRoundtrip content =
  content
    { txAuxScripts = normalizeAuxScripts $ txAuxScripts content
    , txIns = List.sortOn fst $ txIns content
    , txInsCollateral = normalizeInsCollateral $ txInsCollateral content
    }

-- | Original data: Unify empty and None.
--
-- Roundtrip data: Sort transaction input ids.
normalizeInsCollateral :: TxInsCollateral era -> TxInsCollateral era
normalizeInsCollateral = \case
  TxInsCollateralNone -> TxInsCollateralNone
  TxInsCollateral support ins
    -- for original:
    | null ins -> TxInsCollateralNone
    -- for roundtrip:
    | otherwise -> TxInsCollateral support $ List.sort ins

-- | Unify empty and None.
normalizeMetadata :: TxMetadataInEra era -> TxMetadataInEra era
normalizeMetadata = \case
  TxMetadataInEra _ (TxMetadata m) | null m -> TxMetadataNone
  other -> other

-- | Unify empty and None.
-- Upgrade script versions (see Upgrading scripts section).
-- Stabilize order of scripts sorting them by language.
normalizeAuxScripts :: TxAuxScripts era -> TxAuxScripts era
normalizeAuxScripts = \case
  TxAuxScripts _ [] -> TxAuxScriptsNone
  TxAuxScripts support scripts ->
    -- sorting uses script versions, hence sort after upgrade
    TxAuxScripts support $
    List.sortBy normalizeScriptInEraOrder scripts
  other -> other

normalizeScriptInEraOrder :: ScriptInEra era -> ScriptInEra era -> Ordering
normalizeScriptInEraOrder
  (ScriptInEra slang1 script1)
  (ScriptInEra slang2 script2) =
    case testEquality lang1 lang2 of
      Nothing -> compare (AnyScriptLanguage lang1) (AnyScriptLanguage lang2)
      Just Refl -> normalizeScriptOrder script1 script2
    where
      lang1 = languageOfScriptLanguageInEra slang1
      lang2 = languageOfScriptLanguageInEra slang2

-- | This function does not provide natural enough ordering to expose it in the
-- API. It is used here only to compare script sets, not to order them.
-- This is why it is named "normalize order", not "sort".
normalizeScriptOrder :: Script lang -> Script lang -> Ordering
normalizeScriptOrder =
  curry $ \case
    (SimpleScript s1, SimpleScript s2) -> normalizeSimpleScriptOrder s1 s2
    (PlutusScript v1 s1, PlutusScript v2 s2) ->
      compare (AnyPlutusScriptVersion v1) (AnyPlutusScriptVersion v2)
      <> compare s1 s2

normalizeSimpleScriptOrder :: SimpleScript -> SimpleScript -> Ordering
normalizeSimpleScriptOrder =
  curry $ \case
    (RequireAllOf s1, RequireAllOf s2) -> normList s1 s2
    (RequireAnyOf s1, RequireAnyOf s2) -> normList s1 s2
    (RequireMOf m1 s1, RequireMOf m2 s2) -> compare m1 m2 <> normList s1 s2
    (RequireSignature s1, RequireSignature s2) -> compare s1 s2
    (RequireTimeBefore s1, RequireTimeBefore s2) -> compare s1 s2
    (RequireTimeAfter s1, RequireTimeAfter s2) -> compare s1 s2
    (s1, s2) -> compare (consNum s1) (consNum s2)
  where

    consNum :: SimpleScript -> Int
    consNum = \case
      RequireAllOf{} -> 0
      RequireAnyOf{} -> 1
      RequireMOf{} -> 2
      RequireSignature{} -> 3
      RequireTimeAfter{} -> 4
      RequireTimeBefore{} -> 5

    normList :: [SimpleScript] -> [SimpleScript] -> Ordering
    normList = \case
      [] -> \case
        [] -> EQ
        _ -> LT
      x:xs -> \case
        []   -> GT
        y:ys -> normalizeSimpleScriptOrder x y <> normList xs ys

-- | Unify empty and None.
normalizeWithdrawals :: TxWithdrawals ViewTx era -> TxWithdrawals ViewTx era
normalizeWithdrawals = \case
  TxWithdrawals _ [] -> TxWithdrawalsNone
  other -> other

-- | Unify empty and None.
normalizeCertificates :: TxCertificates ViewTx era -> TxCertificates ViewTx era
normalizeCertificates = \case
  TxCertificates _ [] _ -> TxCertificatesNone
  other -> other

-- | Unify empty and None.
normalizeMintValue :: TxMintValue ViewTx era -> TxMintValue ViewTx era
normalizeMintValue = \case
  TxMintValue _ v _ | v == mempty -> TxMintNone
  other -> other

-- * View: Change 'TxBodyContent' “phase” to 'ViewTx'
--
-- Basically, it's just dropping witness information.

viewBodyContent :: TxBodyContent BuildTx era -> TxBodyContent ViewTx era
viewBodyContent body =
  TxBodyContent
    { txAuxScripts = txAuxScripts body
    , txCertificates = viewCertificates $ txCertificates body
    , txExtraKeyWits = txExtraKeyWits body
    , txFee = txFee body
    , txIns = map viewTxIn $ txIns body
    , txInsCollateral = txInsCollateral body
    , txMetadata = txMetadata body
    , txMintValue = viewMintValue $ txMintValue body
    , txOuts = txOuts body
    , txProtocolParams = ViewTx
    , txScriptValidity = txScriptValidity body
    , txUpdateProposal = txUpdateProposal body
    , txValidityRange = txValidityRange body
    , txWithdrawals = viewWithdrawals $ txWithdrawals body
    , txInsReference = viewInsReference $ txInsReference body
    , txTotalCollateral = txTotalCollateral body
    , txReturnCollateral = txReturnCollateral body
    }

viewTxIn
  :: (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
  -> (TxIn, BuildTxWith ViewTx (Witness WitCtxTxIn era))
viewTxIn = second $ const ViewTx

viewWithdrawals :: TxWithdrawals BuildTx era -> TxWithdrawals ViewTx era
viewWithdrawals = \case
  TxWithdrawalsNone -> TxWithdrawalsNone
  TxWithdrawals support withdrawals ->
    TxWithdrawals
      support
      [(address, amount, ViewTx) | (address, amount, _) <- withdrawals]

viewInsReference :: TxInsReference BuildTx era -> TxInsReference ViewTx era
viewInsReference = \case
  TxInsReferenceNone -> TxInsReferenceNone
  TxInsReference support txIns -> TxInsReference support txIns

viewCertificates :: TxCertificates BuildTx era -> TxCertificates ViewTx era
viewCertificates = \case
  TxCertificatesNone -> TxCertificatesNone
  TxCertificates support certificates _ ->
    TxCertificates support certificates ViewTx

viewMintValue :: TxMintValue BuildTx era -> TxMintValue ViewTx era
viewMintValue = \case
  TxMintNone -> TxMintNone
  TxMintValue support value _ -> TxMintValue support value ViewTx


-- * Build: Change 'TxBodyContent' “phase” to 'BuildTx'
--
-- Here we make up the most trivial witnesses or plug holes with 'panic'
-- to make sure the fields are not touched.

buildBodyContent
  :: BuildTxWith BuildTx (Maybe ProtocolParameters)
  -> TxBodyContent ViewTx era
  -> TxBodyContent BuildTx era
buildBodyContent protocolParams body =
  TxBodyContent
    { txAuxScripts = txAuxScripts body
    , txCertificates = buildCertificates $ txCertificates body
    , txExtraKeyWits = txExtraKeyWits body
    , txFee = txFee body
    , txIns = map buildTxIn $ txIns body
    , txInsCollateral = txInsCollateral body
    , txMetadata = txMetadata body
    , txMintValue = buildMintValue $ txMintValue body
    , txOuts = txOuts body
    , txProtocolParams = protocolParams
    , txUpdateProposal = txUpdateProposal body
    , txScriptValidity = txScriptValidity body
    , txValidityRange = txValidityRange body
    , txWithdrawals = buildWithdrawals $ txWithdrawals body
    , txInsReference = buildInsReference $ txInsReference body
    , txTotalCollateral = txTotalCollateral body
    , txReturnCollateral = txReturnCollateral body
    }

buildTxIn
  :: (TxIn, BuildTxWith ViewTx (Witness WitCtxTxIn era))
  -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
buildTxIn = second $ const $ BuildTxWith $ KeyWitness KeyWitnessForSpending

buildWithdrawals :: TxWithdrawals ViewTx era -> TxWithdrawals BuildTx era
buildWithdrawals = \case
  TxWithdrawalsNone -> TxWithdrawalsNone
  TxWithdrawals support withdrawals ->
    TxWithdrawals
      support
      [ ( address
        , amount
        , error "buildWithdrawals: build field should not be checked"
        )
      | (address, amount, _) <- withdrawals
      ]

buildInsReference :: TxInsReference ViewTx era -> TxInsReference BuildTx era
buildInsReference = \case
  TxInsReferenceNone -> TxInsReferenceNone
  TxInsReference support txIns -> TxInsReference support txIns

buildCertificates :: TxCertificates ViewTx era -> TxCertificates BuildTx era
buildCertificates = \case
  TxCertificatesNone -> TxCertificatesNone
  TxCertificates support certificates _ ->
    TxCertificates support certificates (BuildTxWith mempty)

buildMintValue :: TxMintValue ViewTx era -> TxMintValue BuildTx era
buildMintValue = \case
  TxMintNone -> TxMintNone
  TxMintValue support value _ -> TxMintValue support value $ BuildTxWith mempty


tests :: TestTree
tests = testGroup "Test.Cardano.Api.TxBody" $
  [ testPropertyNamed "round trip txbody make get"      "round trip txbody make get"      prop_roundtrip_TxBody_make_get
  , testPropertyNamed "round trip txbody make get make" "round trip txbody make get make" prop_roundtrip_TxBody_make_get_make
  ]
