{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Shelley.Output
  ( PlutusScriptCostError
  , QueryKesPeriodInfoOutput (..)
  , QueryTipLocalState(..)
  , QueryTipLocalStateOutput(..)
  , ScriptCostOutput (..)
  , renderScriptCosts
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Prelude

import           Data.Aeson
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word

import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.Ledger.Shelley.Scripts ()
import           Cardano.Slotting.Time (SystemStart (..))

data QueryKesPeriodInfoOutput =
  QueryKesPeriodInfoOutput
    { qKesInfoCurrentKESPeriod :: Word64
    -- ^ Current KES period.
    , qKesInfoStartKesInterval :: Word64
    -- ^ Beginning of the Kes validity interval.
    , qKesInfoStartEndInterval :: Word64
    -- ^ End of the Kes validity interval.
    , qKesInfoRemainingSlotsInPeriod :: Word64
    -- ^ Remaining slots in current KESPeriod.
    , qKesInfoKesKeyExpiry :: UTCTime
    -- ^ Date of KES key expiry.
    , qKesInfoNodeStateOperationalCertNo :: Word64
    -- ^ The lastest operational certificate number in the node's state
    -- i.e how many times a new KES key has been generated.
    , qKesInfoOnDiskOperationalCertNo :: Word64
    -- ^ The on disk operational certificate number.
    , qKesInfoMaxKesKeyEvolutions :: Word64
    -- ^ The maximum number of KES key evolutions permitted per KESPeriod.
    , qKesInfoSlotsPerKesPeriod :: Word64
    }

instance ToJSON QueryKesPeriodInfoOutput where
  toJSON QueryKesPeriodInfoOutput { qKesInfoCurrentKESPeriod
                                  , qKesInfoStartKesInterval
                                  , qKesInfoStartEndInterval
                                  , qKesInfoRemainingSlotsInPeriod
                                  , qKesInfoKesKeyExpiry
                                  , qKesInfoNodeStateOperationalCertNo
                                  , qKesInfoOnDiskOperationalCertNo
                                  , qKesInfoMaxKesKeyEvolutions
                                  , qKesInfoSlotsPerKesPeriod} =
    object [ "qKesCurrentKesPeriod" .= qKesInfoCurrentKESPeriod
           , "qKesStartKesInterval" .= qKesInfoStartKesInterval
           , "qKesEndKesInterval" .= qKesInfoStartEndInterval
           , "qKesRemainingSlotsInKesPeriod" .= qKesInfoRemainingSlotsInPeriod
           , "qKesOnDiskOperationalCertificateNumber" .= qKesInfoOnDiskOperationalCertNo
           , "qKesNodeStateOperationalCertificateNumber" .= qKesInfoNodeStateOperationalCertNo
           , "qKesMaxKESEvolutions" .= qKesInfoMaxKesKeyEvolutions
           , "qKesSlotsPerKesPeriod" .= qKesInfoSlotsPerKesPeriod
           , "qKesKesKeyExpiry" .= qKesInfoKesKeyExpiry
           ]

data QueryTipLocalState mode = QueryTipLocalState
  { era :: AnyCardanoEra
  , eraHistory :: EraHistory CardanoMode
  , mSystemStart :: Maybe SystemStart
  , mChainTip :: Maybe ChainTip
  }

data QueryTipLocalStateOutput = QueryTipLocalStateOutput
  { localStateChainTip :: ChainTip
  , mEra :: Maybe AnyCardanoEra
  , mEpoch :: Maybe EpochNo
  , mSyncProgress :: Maybe Text
  } deriving Show

-- | A key-value pair difference list for encoding a JSON object.
(..=) :: (KeyValue kv, ToJSON v) => Text -> v -> [kv] -> [kv]
(..=) n v = (n .= v:)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(..=?) :: (KeyValue kv, ToJSON v) => Text -> Maybe v -> [kv] -> [kv]
(..=?) n mv = case mv of
  Just v -> (n .= v:)
  Nothing -> id

instance ToJSON QueryTipLocalStateOutput where
  toJSON a = case localStateChainTip a of
    ChainTipAtGenesis ->
      object $
        ( ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
    ChainTip slotNo blockHeader blockNo ->
      object $
        ( ("slot" ..= slotNo)
        . ("hash" ..= serialiseToRawBytesHexText blockHeader)
        . ("block" ..= blockNo)
        . ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
  toEncoding a = case localStateChainTip a of
    ChainTipAtGenesis ->
      pairs $ mconcat $
        ( ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
    ChainTip slotNo blockHeader blockNo ->
      pairs $ mconcat $
        ( ("slot" ..= slotNo)
        . ("hash" ..= serialiseToRawBytesHexText blockHeader)
        . ("block" ..= blockNo)
        . ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []

instance FromJSON QueryTipLocalStateOutput where
  parseJSON = withObject "QueryTipLocalStateOutput" $ \o -> do
    mEra' <- o .:? "era"
    mEpoch' <- o .:? "epoch"
    mSyncProgress' <- o .:? "syncProgress"

    mSlot <- o .:? "slot"
    mHash <- o .:? "hash"
    mBlock <- o .:? "block"
    case (mSlot, mHash, mBlock) of
      (Nothing, Nothing, Nothing) ->
        pure $ QueryTipLocalStateOutput
                 ChainTipAtGenesis
                 mEra'
                 mEpoch'
                 mSyncProgress'
      (Just slot, Just hash, Just block) ->
        pure $ QueryTipLocalStateOutput
                 (ChainTip slot hash block)
                 mEra'
                 mEpoch'
                 mSyncProgress'
      (_,_,_) -> fail "QueryTipLocalStateOutput was incorrectly JSON encoded.\
                      \ Expected slot, header hash and block number (ChainTip)\
                      \ or none (ChainTipAtGenesis)"

data ScriptCostOutput =
  ScriptCostOutput
    { scScriptHash :: ScriptHash
    , scExecutionUnits :: ExecutionUnits
    , scAda :: Lovelace
    }

instance ToJSON ScriptCostOutput where
  toJSON (ScriptCostOutput sHash execUnits llCost) =
    object [ "scriptHash" .= sHash
           , "executionUnits" .= execUnits
           , "lovelaceCost" .= llCost
           ]

data PlutusScriptCostError
  = PlutusScriptCostErrPlutusScriptNotFound ScriptWitnessIndex
  | PlutusScriptCostErrExecError ScriptWitnessIndex ScriptHash ScriptExecutionError
  | PlutusScriptCostErrRationalExceedsBound ExecutionUnitPrices  ExecutionUnits
  deriving Show


instance Error PlutusScriptCostError where
  displayError (PlutusScriptCostErrPlutusScriptNotFound sWitIndex) =
    "No Plutus script was found at: " <> show sWitIndex
  displayError (PlutusScriptCostErrExecError sWitIndex sHash sExecErro) =
    "Plutus script at: " <> show sWitIndex <> " with hash: " <> show sHash <>
    " errored with: " <> displayError sExecErro
  displayError (PlutusScriptCostErrRationalExceedsBound eUnitPrices eUnits) =
    "Either the execution unit prices: " <> show eUnitPrices <> " or the execution units: " <>
    show eUnits <> " or both are either too precise or not within bounds"

renderScriptCosts
  :: ExecutionUnitPrices
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
  -- ^ Initial mapping of script witness index to actual script.
  -- We need this in order to know which script corresponds to the
  -- calculated execution units.
  -> Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits)
  -- ^ Post execution cost calculation mapping of script witness
  -- index to execution units.
  -> Either PlutusScriptCostError [ScriptCostOutput]
renderScriptCosts eUnitPrices scriptMapping executionCostMapping =
  sequenceA $ Map.foldlWithKey
    (\accum sWitInd eExecUnits -> do
      case List.lookup sWitInd scriptMapping of
        Just (AnyScriptWitness SimpleScriptWitness{}) ->  accum
        Just (AnyScriptWitness (PlutusScriptWitness _ pVer pScript _ _ _)) -> do
          let scriptHash = hashScript $ PlutusScript pVer pScript
          case eExecUnits of
            Right execUnits ->
              case calculateExecutionUnitsLovelace eUnitPrices execUnits of
                Just llCost ->
                  Right (ScriptCostOutput scriptHash execUnits llCost)
                    : accum
                Nothing ->
                  Left (PlutusScriptCostErrRationalExceedsBound eUnitPrices execUnits)
                    : accum
            Left err -> Left (PlutusScriptCostErrExecError sWitInd scriptHash err) : accum
        Nothing -> Left (PlutusScriptCostErrPlutusScriptNotFound sWitInd) : accum
    ) [] executionCostMapping

