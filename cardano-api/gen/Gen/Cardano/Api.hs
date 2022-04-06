{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Gen.Cardano.Api
  ( genMetadata
  , genAlonzoGenesis
  ) where

import           Cardano.Prelude
import           Control.Monad (MonadFail(fail))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--TODO: why do we have this odd split? We can get rid of the old name "typed"
import           Gen.Cardano.Api.Typed (genRational)

import           Cardano.Ledger.Shelley.Metadata (Metadata (..), Metadatum (..))
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger

import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2

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

genCostModelFromKeys :: Alonzo.Language -> Set Text -> Gen Alonzo.CostModel
genCostModelFromKeys lang costModelParamNames = do
  newCMPs <- traverse
               (const (Gen.integral (Range.linear 0 100)))
               (mkNullCostModel costModelParamNames)
  case Alonzo.mkCostModel lang newCMPs of
    Left e -> fail ("Corrupt cost model: " <> e)
    Right cm -> pure cm
  where
    mkNullCostModel = Map.fromList . fmap (\k -> (k, 0 :: Integer)) . Set.toList

genCostModel :: Alonzo.Language -> Gen (Alonzo.Language, Alonzo.CostModel)
genCostModel Alonzo.PlutusV1 =
  (Alonzo.PlutusV1,) <$> genCostModelFromKeys Alonzo.PlutusV1 PV1.costModelParamNames
genCostModel Alonzo.PlutusV2 =
  (Alonzo.PlutusV2,) <$> genCostModelFromKeys Alonzo.PlutusV2 PV2.costModelParamNames

genCostModels :: Gen Alonzo.CostModels
genCostModels = Alonzo.CostModels . Map.fromList
  <$> (Gen.subsequence Alonzo.nonNativeLanguages >>= mapM genCostModel)

genAlonzoGenesis :: Gen Alonzo.AlonzoGenesis
genAlonzoGenesis = do
  coinsPerUTxOWord <- genCoin (Range.linear 0 5)
  costmdls' <- genCostModels
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
