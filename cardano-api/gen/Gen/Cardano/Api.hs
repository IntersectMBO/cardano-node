{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.Cardano.Api
  ( genMetadata
  , genAlonzoGenesis
  ) where

import           Cardano.Prelude
import           Control.Monad (MonadFail(fail))
import qualified Data.Map.Strict as Map

--TODO: why do we have this odd split? We can get rid of the old name "typed"
import           Gen.Cardano.Api.Typed (genRational)

import           Cardano.Ledger.Shelley.Metadata (Metadata (..), Metadatum (..))
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMetadata :: Gen (Metadata era)
genMetadata = do
  numberOfIndices <- Gen.integral (Range.linear 1 15)
  let indices = map (\i -> fromIntegral i :: Word64) [1..numberOfIndices]
  mData <- Gen.list (Range.singleton numberOfIndices) genMetadatum
  return . Metadata . Map.fromList $ zip indices mData

genMetadatum :: Gen Metadatum
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

genCoin :: Range Integer -> Gen Ledger.Coin
genCoin r = do
  unCoin' <- Gen.integral r
  return $ Ledger.Coin unCoin'

genPrice :: Gen Ledger.NonNegativeInterval
genPrice = do
  unPrice <- genRational
  case Ledger.boundRational unPrice of
    Nothing -> fail "genPrice: genRational should give us a bounded rational"
    Just p -> pure p

genLanguage :: Gen Alonzo.Language
genLanguage = return Alonzo.PlutusV1

genPrices :: Gen Alonzo.Prices
genPrices = do
  prMem'   <- genPrice
  prSteps' <- genPrice

  return Alonzo.Prices
    { Alonzo.prMem = prMem'
    , Alonzo.prSteps = prSteps'
    }

genExUnits :: Gen Alonzo.ExUnits
genExUnits = do
  exUnitsMem' <- Gen.integral (Range.linear 0 10)
  exUnitsSteps' <- Gen.integral (Range.linear 0 10)
  return Alonzo.ExUnits
    { Alonzo.exUnitsMem = exUnitsMem'
    , Alonzo.exUnitsSteps = exUnitsSteps'
    }

genCostModel :: Range Int -> Gen Text -> Gen Integer -> Gen Alonzo.CostModel
genCostModel r gt gi = do
  map' <- Gen.map r ((,) <$> gt <*> gi)
  return $ Alonzo.CostModel map'

genAlonzoGenesis :: Gen Alonzo.AlonzoGenesis
genAlonzoGenesis = do
  coinsPerUTxOWord <- genCoin (Range.linear 0 5)
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
    { Alonzo.coinsPerUTxOWord = coinsPerUTxOWord
    , Alonzo.costmdls = costmdls'
    , Alonzo.prices = prices'
    , Alonzo.maxTxExUnits = maxTxExUnits'
    , Alonzo.maxBlockExUnits = maxBlockExUnits'
    , Alonzo.maxValSize = maxValSize'
    , Alonzo.collateralPercentage = collateralPercentage'
    , Alonzo.maxCollateralInputs = maxCollateralInputs'
    }
