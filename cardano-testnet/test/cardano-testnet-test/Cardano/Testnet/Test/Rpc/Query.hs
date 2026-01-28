{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Rpc.Query
  ( hprop_rpc_query_pparams
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Type.Output (QueryTipLocalStateOutput (..))
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Binary.Version as L
import qualified Cardano.Ledger.Conway.Core as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Plutus as L
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Rpc.Server.Internal.UtxoRpc.Query ()
import           Cardano.Rpc.Server.Internal.UtxoRpc.Type (anyUtxoDataUtxoRpcToUtxo,
                   utxoRpcBigIntToInteger)
import           Cardano.Testnet

import           Prelude

import           Control.Exception
import qualified Data.ByteString.Short as SBS
import           Data.Default.Class
import qualified Data.Map.Strict as M
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_rpc_query_pparams :: Property
hprop_rpc_query_pparams = integrationRetryWorkspace 2 "rpc-query-pparams" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe, cardanoEnableRpc = True}

  TestnetRuntime
    { testnetMagic
    , configurationFile
    , testnetNodes = node0@TestnetNode{nodeSprocket} : _
    } <-
    createAndRunTestnet options def conf

  execConfig <- mkExecConfig tempAbsPath' nodeSprocket testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  pparams <- unLedgerProtocolParameters <$> getProtocolParams epochStateView ceo
  -- H.noteShowPretty_ pparams
  utxos <- findAllUtxos epochStateView sbe
  H.noteShowPretty_ utxos
  rpcSocket <- H.note . unFile $ nodeRpcSocketPath node0

  ----------
  -- Get tip
  ----------
  QueryTipLocalStateOutput{localStateChainTip} <-
    H.noteShowM $ execCliStdoutToJson execConfig [eraName, "query", "tip"]
  (slot, blockHash, blockNo) <- case localStateChainTip of
    ChainTipAtGenesis -> H.failure
    ChainTip (SlotNo slot) (HeaderHash hash) (BlockNo blockNo) -> pure (slot, SBS.fromShort hash, blockNo)

  --------------
  -- RPC queries
  --------------
  let rpcServer = Rpc.ServerUnix rpcSocket
  (pparamsResponse, utxosResponse) <- H.noteShowM . H.evalIO . Rpc.withConnection def rpcServer $ \conn -> do
    pparams' <- do
      let req = Rpc.defMessage
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readParams")) req

    utxos' <- do
      let req = Rpc.defMessage
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf UtxoRpc.QueryService "readUtxos")) req
    pure (pparams', utxos')

  ---------------------------
  -- Test readParams response
  ---------------------------
  pparamsResponse ^. #ledgerTip . #slot === slot
  pparamsResponse ^. #ledgerTip . #hash === blockHash
  pparamsResponse ^. #ledgerTip . #height === blockNo
  pparamsResponse ^. #ledgerTip . #timestamp === 0 -- not possible to implement at this moment

  -- https://docs.cardano.org/about-cardano/explore-more/parameter-guide
  let chainParams = pparamsResponse ^. #values . #cardano
  babbageEraOnwardsConstraints (convert ceo) $ do
    pparams ^. L.ppCoinsPerUTxOByteL . to L.unCoinPerByte . to L.unCoin
      ===^ chainParams ^. #coinsPerUtxoByte . to utxoRpcBigIntToInteger
    pparams ^. L.ppMaxTxSizeL === chainParams ^. #maxTxSize . to fromIntegral
    pparams ^. L.ppMinFeeBL ===^ chainParams ^. #minFeeCoefficient . to (fmap L.Coin . utxoRpcBigIntToInteger)
    pparams ^. L.ppMinFeeAL ===^ chainParams ^. #minFeeConstant . to (fmap L.Coin . utxoRpcBigIntToInteger)
    pparams ^. L.ppMaxBBSizeL === chainParams ^. #maxBlockBodySize . to fromIntegral
    pparams ^. L.ppMaxBHSizeL === chainParams ^. #maxBlockHeaderSize . to fromIntegral
    pparams ^. L.ppKeyDepositL ===^ chainParams ^. #stakeKeyDeposit . to (fmap L.Coin . utxoRpcBigIntToInteger)
    pparams ^. L.ppPoolDepositL ===^ chainParams ^. #poolDeposit . to (fmap L.Coin . utxoRpcBigIntToInteger)
    pparams ^. L.ppEMaxL . to L.unEpochInterval === chainParams ^. #poolRetirementEpochBound . to fromIntegral
    pparams ^. L.ppNOptL === chainParams ^. #desiredNumberOfPools . to fromIntegral
    pparams ^. L.ppA0L . to L.unboundRational === chainParams ^. #poolInfluence . to inject
    pparams ^. L.ppNOptL === chainParams ^. #desiredNumberOfPools . to fromIntegral
    pparams ^. L.ppRhoL . to L.unboundRational === chainParams ^. #monetaryExpansion . to inject
    pparams ^. L.ppMinPoolCostL ===^ chainParams ^. #minPoolCost . to (fmap L.Coin . utxoRpcBigIntToInteger)
    ( pparams ^. L.ppProtocolVersionL . to L.pvMajor . to L.getVersion
      , pparams ^. L.ppProtocolVersionL . to L.pvMinor
      )
      === ( chainParams ^. #protocolVersion . #major
          , chainParams ^. #protocolVersion . #minor . to fromIntegral
          )
    pparams ^. L.ppMaxValSizeL === chainParams ^. #maxValueSize . to fromIntegral
    pparams ^. L.ppCollateralPercentageL === chainParams ^. #collateralPercentage . to fromIntegral
    pparams ^. L.ppMaxCollateralInputsL === chainParams ^. #maxCollateralInputs . to fromIntegral
    let pparamsCostModels = L.getCostModelParams <$> pparams ^. L.ppCostModelsL . to L.costModelsValid
        wrapInMaybe v = if v == mempty then Nothing else Just v
    M.lookup L.PlutusV1 pparamsCostModels === chainParams ^. #costModels . #plutusV1 . #values . to wrapInMaybe
    M.lookup L.PlutusV2 pparamsCostModels === chainParams ^. #costModels . #plutusV2 . #values . to wrapInMaybe
    M.lookup L.PlutusV3 pparamsCostModels === chainParams ^. #costModels . #plutusV3 . #values . to wrapInMaybe
    M.lookup L.PlutusV4 pparamsCostModels === chainParams ^. #costModels . #plutusV4 . #values . to wrapInMaybe
    pparams ^. L.ppPricesL . to L.prSteps . to L.unboundRational === chainParams ^. #prices . #steps . to inject
    pparams ^. L.ppPricesL . to L.prMem . to L.unboundRational === chainParams ^. #prices . #memory . to inject
    pparams ^. L.ppMaxTxExUnitsL === chainParams ^. #maxExecutionUnitsPerTransaction . to inject
    pparams ^. L.ppMaxBlockExUnitsL === chainParams ^. #maxExecutionUnitsPerBlock . to inject
    pparams ^. L.ppMinFeeRefScriptCostPerByteL . to L.unboundRational
      === chainParams ^. #minFeeScriptRefCostPerByte . to inject
    let poolVotingThresholds :: L.PoolVotingThresholds =
          conwayEraOnwardsConstraints ceo $
            pparams ^. L.ppPoolVotingThresholdsL
    ( L.unboundRational
        <$> [ poolVotingThresholds ^. L.pvtMotionNoConfidenceL
            , poolVotingThresholds ^. L.pvtCommitteeNormalL
            , poolVotingThresholds ^. L.pvtCommitteeNoConfidenceL
            , poolVotingThresholds ^. L.pvtHardForkInitiationL
            , poolVotingThresholds ^. L.pvtPPSecurityGroupL
            ]
      )
      === chainParams ^. #poolVotingThresholds . #thresholds . to (map inject)
    let drepVotingThresholds :: L.DRepVotingThresholds =
          conwayEraOnwardsConstraints ceo $
            pparams ^. L.ppDRepVotingThresholdsL
    ( L.unboundRational
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
      === chainParams ^. #drepVotingThresholds . #thresholds . to (map inject)
    pparams ^. L.ppCommitteeMinSizeL === chainParams ^. #minCommitteeSize . to fromIntegral
    pparams ^. L.ppCommitteeMaxTermLengthL . to L.unEpochInterval
      === chainParams ^. #committeeTermLimit . to fromIntegral
    pparams ^. L.ppGovActionLifetimeL . to L.unEpochInterval
      === chainParams ^. #governanceActionValidityPeriod . to fromIntegral
    pparams ^. L.ppGovActionDepositL ===^ chainParams ^. #governanceActionDeposit . to (fmap L.Coin . utxoRpcBigIntToInteger)
    pparams ^. L.ppDRepDepositL ===^ chainParams ^. #drepDeposit . to (fmap L.Coin . utxoRpcBigIntToInteger)
    pparams ^. L.ppDRepActivityL . to L.unEpochInterval === chainParams ^. #drepInactivityPeriod . to fromIntegral

  --------------------------
  -- Test readUtxos response
  --------------------------

  utxoFromUtxoRpc <- H.leftFail $ utxosResponse ^. #items . to (anyUtxoDataUtxoRpcToUtxo $ convert ceo)
  utxos === utxoFromUtxoRpc

  H.threadDelay 90000000

  H.failure


(===^) :: (Eq a, Show a, H.MonadTest m) => a -> Either SomeException a -> m ()
expected ===^ actual = do
  v <- H.leftFail actual
  expected === v

infix 4 ===^
