{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readChainConfigMethod
  , readDataMethod
  , readParamsMethod
  , readTxMethod
  , readUtxosMethod
  , searchUtxosMethod
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Binary.Version as L
import qualified Cardano.Ledger.Conway.Core as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Plutus as L
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Rpc.Server.Internal.Error
import           Cardano.Rpc.Server.Internal.Monad

import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as M
import           Data.ProtoLens (defMessage)
import           Data.Ratio (Ratio, denominator, numerator, (%))
import           Lens.Micro
import           Network.GRPC.Spec

import           RIO

readParamsMethod :: MonadRpc e m => Proto UtxoRpc.ReadParamsRequest -> m (Proto UtxoRpc.ReadParamsResponse)
readParamsMethod _req = do
  -- TODO: implement field masks - they are ignored for now
  -- they need to be normalised beforehand, see: https://github.com/protocolbuffers/protobuf/blob/main/java/util/src/main/java/com/google/protobuf/util/FieldMaskTree.java#L76
  -- let fieldMask :: [Text] = req ^. #fieldMask ^. #paths
  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum Conway era required") pure
  let sbe = convert eon

  let target = VolatileTip
  let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolParameters
  (pparams, chainPoint) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    pparams <- throwEither =<< throwEither =<< queryExpr qInMode
    chainPoint <- throwEither =<< queryChainPoint
    pure (pparams, chainPoint)

  let (slotNo, blockHash) =
        case chainPoint of
          ChainPointAtGenesis -> (0, mempty)
          ChainPoint (SlotNo slot) (HeaderHash hash) -> (slot, SBS.fromShort hash)
      pparamsCostModels :: Map L.Language [Int64] =
        babbageEraOnwardsConstraints (convert eon) $
          L.getCostModelParams <$> pparams ^. L.ppCostModelsL . to L.costModelsValid
      poolVotingThresholds :: L.PoolVotingThresholds =
        conwayEraOnwardsConstraints eon $
          pparams ^. L.ppPoolVotingThresholdsL
      drepVotingThresholds :: L.DRepVotingThresholds =
        conwayEraOnwardsConstraints eon $
          pparams ^. L.ppDRepVotingThresholdsL

  let chainPointMsg =
        defMessage
          & #slot .~ slotNo
          & #hash .~ blockHash
      pparamsMsg =
        conwayEraOnwardsConstraints eon $
          defMessage
            & #coinsPerUtxoByte .~ pparams ^. L.ppCoinsPerUTxOByteL . to L.unCoinPerByte . to fromIntegral
            & #maxTxSize .~ pparams ^. L.ppMaxTxSizeL . to fromIntegral
            & #minFeeCoefficient .~ pparams ^. L.ppMinFeeBL . to fromIntegral
            & #minFeeConstant .~ pparams ^. L.ppMinFeeAL . to fromIntegral
            & #maxBlockBodySize .~ pparams ^. L.ppMaxBBSizeL . to fromIntegral
            & #maxBlockHeaderSize .~ pparams ^. L.ppMaxBHSizeL . to fromIntegral
            & #stakeKeyDeposit .~ pparams ^. L.ppKeyDepositL . to fromIntegral
            & #poolDeposit .~ pparams ^. L.ppPoolDepositL . to fromIntegral
            & #poolRetirementEpochBound .~ pparams ^. L.ppEMaxL . to L.unEpochInterval . to fromIntegral
            & #desiredNumberOfPools .~ pparams ^. L.ppNOptL . to fromIntegral
            & #poolInfluence .~ pparams ^. L.ppA0L . to L.unboundRational . to inject
            & #desiredNumberOfPools .~ pparams ^. L.ppNOptL . to fromIntegral
            & #monetaryExpansion .~ pparams ^. L.ppRhoL . to L.unboundRational . to inject
            & #minPoolCost .~ pparams ^. L.ppMinPoolCostL . to fromIntegral
            & #protocolVersion . #major .~ pparams ^. L.ppProtocolVersionL . to L.pvMajor . to L.getVersion
            & #protocolVersion . #minor .~ pparams ^. L.ppProtocolVersionL . to L.pvMinor . to fromIntegral
            & #maxValueSize .~ pparams ^. L.ppMaxValSizeL . to fromIntegral
            & #collateralPercentage .~ pparams ^. L.ppCollateralPercentageL . to fromIntegral
            & #maxCollateralInputs .~ pparams ^. L.ppMaxCollateralInputsL . to fromIntegral
            & #costModels . #plutusV1 . #values .~ (join . maybeToList) (M.lookup L.PlutusV1 pparamsCostModels)
            & #costModels . #plutusV2 . #values .~ (join . maybeToList) (M.lookup L.PlutusV2 pparamsCostModels)
            & #costModels . #plutusV3 . #values .~ (join . maybeToList) (M.lookup L.PlutusV3 pparamsCostModels)
            & #prices . #steps .~ pparams ^. L.ppPricesL . to L.prSteps . to L.unboundRational . to inject
            & #prices . #memory .~ pparams ^. L.ppPricesL . to L.prMem . to L.unboundRational . to inject
            & #maxExecutionUnitsPerTransaction .~ pparams ^. L.ppMaxTxExUnitsL . to inject
            & #maxExecutionUnitsPerBlock .~ pparams ^. L.ppMaxBlockExUnitsL . to inject
            & #minFeeScriptRefCostPerByte .~ pparams ^. L.ppMinFeeRefScriptCostPerByteL . to L.unboundRational . to inject
            & #poolVotingThresholds . #thresholds
              .~ ( inject . L.unboundRational
                     -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
                     <$> [ poolVotingThresholds ^. L.pvtMotionNoConfidenceL
                         , poolVotingThresholds ^. L.pvtCommitteeNormalL
                         , poolVotingThresholds ^. L.pvtCommitteeNoConfidenceL
                         , poolVotingThresholds ^. L.pvtHardForkInitiationL
                         , poolVotingThresholds ^. L.pvtPPSecurityGroupL
                         ]
                 )
            & #drepVotingThresholds . #thresholds
              .~ ( inject . L.unboundRational
                     -- order taken from https://github.com/cardano-foundation/CIPs/blob/acb4b2348c968003dfc370cd3769615bfca1f159/CIP-1694/README.md#requirements
                     <$> [ drepVotingThresholds ^. L.dvtMotionNoConfidenceL
                         , drepVotingThresholds ^. L.dvtCommitteeNormalL
                         , drepVotingThresholds ^. L.dvtCommitteeNoConfidenceL
                         , drepVotingThresholds ^. L.dvtUpdateToConstitutionL
                         , drepVotingThresholds ^. L.dvtHardForkInitiationL
                         , drepVotingThresholds ^. L.dvtPPNetworkGroupL
                         , drepVotingThresholds ^. L.dvtPPEconomicGroupL
                         , drepVotingThresholds ^. L.dvtPPTechnicalGroupL
                         , drepVotingThresholds ^. L.dvtPPGovGroupL
                         , drepVotingThresholds ^. L.dvtTreasuryWithdrawalL
                         ]
                 )
            & #minCommitteeSize .~ pparams ^. L.ppCommitteeMinSizeL . to fromIntegral
            & #committeeTermLimit .~ pparams ^. L.ppCommitteeMaxTermLengthL . to L.unEpochInterval . to fromIntegral
            & #governanceActionValidityPeriod .~ pparams ^. L.ppGovActionLifetimeL . to L.unEpochInterval . to fromIntegral
            & #governanceActionDeposit .~ pparams ^. L.ppGovActionDepositL . to fromIntegral
            & #drepDeposit .~ pparams ^. L.ppDRepDepositL . to fromIntegral
            & #drepInactivityPeriod .~ pparams ^. L.ppDRepActivityL . to L.unEpochInterval . to fromIntegral
  pure $
    defMessage
      & #ledgerTip .~ chainPointMsg
      & #values . #cardano .~ pparamsMsg

readChainConfigMethod
  :: MonadRpc e m => Proto UtxoRpc.ReadChainConfigRequest -> m (Proto UtxoRpc.ReadChainConfigResponse)
readChainConfigMethod = undefined

readTxMethod :: MonadRpc e m => Proto UtxoRpc.ReadTxRequest -> m (Proto UtxoRpc.ReadTxResponse)
readTxMethod = undefined

readUtxosMethod :: MonadRpc e m => Proto UtxoRpc.ReadUtxosRequest -> m (Proto UtxoRpc.ReadUtxosResponse)
readUtxosMethod = undefined

readDataMethod :: MonadRpc e m => Proto UtxoRpc.ReadDataRequest -> m (Proto UtxoRpc.ReadDataResponse)
readDataMethod = undefined

searchUtxosMethod :: MonadRpc e m => Proto UtxoRpc.SearchUtxosRequest -> m (Proto UtxoRpc.SearchUtxosResponse)
searchUtxosMethod = undefined

-- orphans to upstream
instance Error QueryConvenienceError where
  prettyError = pretty . renderQueryConvenienceError

instance Error AcquiringFailure where
  prettyError = fromString . show

instance Error UnsupportedNtcVersionError where
  prettyError = fromString . show

instance Error EraMismatch where
  prettyError = fromString . show

instance Inject (Proto UtxoRpc.RationalNumber) (Ratio Integer) where
  inject r = r ^. #numerator . to fromIntegral % r ^. #denominator . to fromIntegral

instance Inject (Ratio Integer) (Proto UtxoRpc.RationalNumber) where
  inject r =
    defMessage
      & #numerator .~ fromIntegral (numerator r)
      & #denominator .~ fromIntegral (denominator r)

instance Inject (Proto UtxoRpc.ExUnits) L.ExUnits where
  inject r =
    L.ExUnits
      { L.exUnitsMem = r ^. #memory . to fromIntegral
      , L.exUnitsSteps = r ^. #steps . to fromIntegral
      }

instance Inject L.ExUnits (Proto UtxoRpc.ExUnits) where
  inject L.ExUnits{L.exUnitsMem = mem, L.exUnitsSteps = steps} =
    defMessage
      & #memory .~ fromIntegral mem
      & #steps .~ fromIntegral steps
