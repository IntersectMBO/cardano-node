{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Gen
  ( genMetadata
  , genAlonzoGenesis
  ) where

import           Cardano.Prelude
import           Shelley.Spec.Ledger.Metadata (Metadata (..), Metadatum (..))
import           Hedgehog (MonadGen, Range)

import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Coin as Ledger
import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMetadata :: MonadGen m => m (Metadata era)
genMetadata = do
  numberOfIndicies <- Gen.integral (Range.linear 1 15)
  let indexes = map (\i -> fromIntegral i :: Word64) [1..numberOfIndicies]
  mDatums <- Gen.list (Range.singleton numberOfIndicies) genMetadatum
  return . Metadata . Map.fromList $ zip indexes mDatums

genMetadatum :: MonadGen m => m Metadatum
genMetadatum = do
  int <- Gen.list (Range.linear 1 5) (I <$> Gen.integral (Range.linear 1 100))
  bytes <- Gen.list (Range.linear 1 5) (B <$> Gen.bytes (Range.linear 1 20))
  str <- Gen.list (Range.linear 1 5) (S <$> Gen.text (Range.linear 1 20) Gen.alphaNum)
  let mDatumList = int ++ bytes ++ str

  singleMetadatum <- Gen.element mDatumList

  Gen.element
    [ List mDatumList
    , Map [(singleMetadatum, singleMetadatum)]
    , Map [(List mDatumList, singleMetadatum)]
    , Map [(singleMetadatum, List mDatumList)]
    ]

genCoin :: MonadGen m => Range Integer -> m Ledger.Coin
genCoin r = do
  unCoin' <- Gen.integral r
  return $ Ledger.Coin unCoin'

genLanguage :: MonadGen m => m Alonzo.Language
genLanguage = return Alonzo.PlutusV1

genPrices :: MonadGen m => m Alonzo.Prices
genPrices = do
  prMem' <- genCoin (Range.linear 0 10)
  prSteps' <- genCoin (Range.linear 0 10)

  return Alonzo.Prices
    { Alonzo.prMem = prMem'
    , Alonzo.prSteps = prSteps'
    }

genExUnits :: MonadGen m => m Alonzo.ExUnits
genExUnits = do
  exUnitsMem' <- Gen.word64 (Range.linear 0 10)
  exUnitsSteps' <- Gen.word64 (Range.linear 0 10)
  return Alonzo.ExUnits
    { Alonzo.exUnitsMem = exUnitsMem'
    , Alonzo.exUnitsSteps = exUnitsSteps'
    }

genCostModel :: MonadGen m => Range Int -> m Text -> m Integer ->  m Alonzo.CostModel
genCostModel r gt gi = do
  map' <- Gen.map r ((,) <$> gt <*> gi)
  return $ Alonzo.CostModel map'

genAlonzoGenesis :: MonadGen m => m Alonzo.AlonzoGenesis
genAlonzoGenesis = do
  adaPerUTxOWord' <- genCoin (Range.linear 0 5)
  costmdls' <- Gen.map (Range.linear 0 5) $ (,)
    <$> genLanguage
    <*> genCostModel (Range.linear 0 5)
          (Gen.text (Range.linear 0 10) Gen.alphaNum)
          (Gen.integral (Range.linear 0 100))
  prices' <- genPrices
  maxTxExUnits' <- genExUnits
  maxBlockExUnits' <- genExUnits
  maxValSize' <- Gen.integral (Range.linear 0 10)
  collateralPercentage' <- Gen.integral (Range.linear 0 10)
  maxCollateralInputs' <- Gen.integral (Range.linear 0 10)

  return Alonzo.AlonzoGenesis
    { Alonzo.adaPerUTxOWord = adaPerUTxOWord'
    , Alonzo.costmdls = costmdls'
    , Alonzo.prices = prices'
    , Alonzo.maxTxExUnits = maxTxExUnits'
    , Alonzo.maxBlockExUnits = maxBlockExUnits'
    , Alonzo.maxValSize = maxValSize'
    , Alonzo.collateralPercentage = collateralPercentage'
    , Alonzo.maxCollateralInputs = maxCollateralInputs'
    }
