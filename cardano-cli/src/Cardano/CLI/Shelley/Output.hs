{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.CLI.Shelley.Output
  ( PlutusScriptCostError
  , QueryKesPeriodInfoOutput (..)
  , QueryTipLocalState(..)
  , QueryTipLocalStateOutput(..)
  , ScriptCostOutput (..)
  , createOpCertIntervalInfo
  , renderScriptCosts
  ) where

import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.Slotting.Time (SystemStart (..))
import           Data.Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word

import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Types
import           Cardano.Ledger.Shelley.Scripts ()

data QueryKesPeriodInfoOutput =
  QueryKesPeriodInfoOutput
    { qKesOpCertIntervalInformation :: OpCertIntervalInformation
      -- | Date of KES key expiry.
    , qKesInfoKesKeyExpiry :: Maybe UTCTime
      -- | The latest operational certificate number in the node's state
      -- i.e how many times a new KES key has been generated.
    , qKesInfoNodeStateOperationalCertNo :: Maybe OpCertNodeStateCounter
      -- | The on disk operational certificate number.
    , qKesInfoOnDiskOperationalCertNo :: OpCertOnDiskCounter
      -- | The maximum number of KES key evolutions permitted per KES period.
    , qKesInfoMaxKesKeyEvolutions :: Word64
    , qKesInfoSlotsPerKesPeriod :: Word64
    } deriving (Eq, Show)

instance ToJSON QueryKesPeriodInfoOutput where
  toJSON (QueryKesPeriodInfoOutput opCertIntervalInfo
                                   kesKeyExpiryTime
                                   nodeStateOpCertNo
                                   (OpCertOnDiskCounter onDiskOpCertNo)
                                   maxKesKeyOps
                                   slotsPerKesPeriod) = do
    let (sKes, eKes, cKes, slotsTillExp) =
          case opCertIntervalInfo of
            OpCertWithinInterval startKes endKes currKes sUntilExp ->
                     ( unOpCertStartingKesPeriod startKes
                     , unOpCertEndingKesPeriod endKes
                     , unCurrentKesPeriod currKes
                     , Just sUntilExp
                     )
            OpCertStartingKesPeriodIsInTheFuture startKes endKes currKes ->
                     ( unOpCertStartingKesPeriod startKes
                     , unOpCertEndingKesPeriod endKes
                     , unCurrentKesPeriod currKes
                     , Nothing
                     )
            OpCertExpired startKes endKes currKes ->
                     ( unOpCertStartingKesPeriod startKes
                     , unOpCertEndingKesPeriod endKes
                     , unCurrentKesPeriod currKes
                     , Nothing
                     )
            OpCertSomeOtherError startKes endKes currKes ->
                     ( unOpCertStartingKesPeriod startKes
                     , unOpCertEndingKesPeriod endKes
                     , unCurrentKesPeriod currKes
                     , Nothing
                     )

    object [ "qKesCurrentKesPeriod" .= cKes
           , "qKesStartKesInterval" .= sKes
           , "qKesEndKesInterval" .= eKes
           , "qKesRemainingSlotsInKesPeriod" .= slotsTillExp
           , "qKesOnDiskOperationalCertificateNumber" .= onDiskOpCertNo
           , "qKesNodeStateOperationalCertificateNumber" .=  nodeStateOpCertNo
           , "qKesMaxKESEvolutions" .= maxKesKeyOps
           , "qKesSlotsPerKesPeriod" .= slotsPerKesPeriod
           , "qKesKesKeyExpiry" .= kesKeyExpiryTime
           ]

instance FromJSON QueryKesPeriodInfoOutput where
  parseJSON = withObject "QueryKesPeriodInfoOutput" $ \o -> do
    currentKesPeriod <- o .: "qKesCurrentKesPeriod"
    startKesInterval <- o .: "qKesStartKesInterval"
    endKesInterval <- o .: "qKesEndKesInterval"
    remainingSlotsInKesPeriod <- o .: "qKesRemainingSlotsInKesPeriod"
    onDiskOperationalCertificateNumber <- o .: "qKesOnDiskOperationalCertificateNumber"
    nodeStateOperationalCertificateNumber <- o .: "qKesNodeStateOperationalCertificateNumber"
    maxKESEvolutions <- o .: "qKesMaxKESEvolutions"
    slotsPerKesPeriod <- o .: "qKesSlotsPerKesPeriod"
    kesKeyExpiry <- o .: "qKesKesKeyExpiry"
    let opCertIntervalInfo = createOpCertIntervalInfo
                               currentKesPeriod
                               startKesInterval
                               endKesInterval
                               remainingSlotsInKesPeriod
    return $ QueryKesPeriodInfoOutput
         { qKesOpCertIntervalInformation = opCertIntervalInfo
         , qKesInfoKesKeyExpiry = kesKeyExpiry
         , qKesInfoNodeStateOperationalCertNo = nodeStateOperationalCertificateNumber
         , qKesInfoOnDiskOperationalCertNo = onDiskOperationalCertificateNumber
         , qKesInfoMaxKesKeyEvolutions = maxKESEvolutions
         , qKesInfoSlotsPerKesPeriod = slotsPerKesPeriod
         }


createOpCertIntervalInfo
  :: CurrentKesPeriod
  -> OpCertStartingKesPeriod
  -> OpCertEndingKesPeriod
  -> Maybe SlotsTillKesKeyExpiry
  -> OpCertIntervalInformation
createOpCertIntervalInfo c@(CurrentKesPeriod cKesPeriod)
                         s@(OpCertStartingKesPeriod oCertStart)
                         e@(OpCertEndingKesPeriod oCertEnd)
                         (Just tillExp)
  | oCertStart <= cKesPeriod && cKesPeriod < oCertEnd =
      OpCertWithinInterval s e c tillExp
  | oCertStart > cKesPeriod = OpCertStartingKesPeriodIsInTheFuture s e c
  | cKesPeriod >= oCertEnd = OpCertExpired s e c
  | otherwise = OpCertSomeOtherError s e c
createOpCertIntervalInfo c@(CurrentKesPeriod cKesPeriod)
                         s@(OpCertStartingKesPeriod oCertStart)
                         e@(OpCertEndingKesPeriod oCertEnd)
                         Nothing
  | oCertStart > cKesPeriod = OpCertStartingKesPeriodIsInTheFuture s e c
  | cKesPeriod >= oCertEnd = OpCertExpired s e c
  | otherwise = OpCertSomeOtherError s e c


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
(..=) :: (KeyValue kv, ToJSON v) => Aeson.Key -> v -> [kv] -> [kv]
(..=) n v = (n .= v:)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(..=?) :: (KeyValue kv, ToJSON v) => Aeson.Key -> Maybe v -> [kv] -> [kv]
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
    { scScriptHash :: Maybe ScriptHash
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
  | PlutusScriptCostErrExecError ScriptWitnessIndex (Maybe ScriptHash) ScriptExecutionError
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
        Just (AnyScriptWitness SimpleScriptWitness{}) -> accum

        Just (AnyScriptWitness (PlutusScriptWitness _ pVer (PScript pScript) _ _ _)) -> do
          let scriptHash = hashScript $ PlutusScript pVer pScript
          case eExecUnits of
            Right execUnits ->
              case calculateExecutionUnitsLovelace eUnitPrices execUnits of
                Just llCost ->
                  Right (ScriptCostOutput (Just scriptHash) execUnits llCost)
                    : accum
                Nothing ->
                  Left (PlutusScriptCostErrRationalExceedsBound eUnitPrices execUnits)
                    : accum
            Left err -> Left (PlutusScriptCostErrExecError sWitInd (Just scriptHash) err) : accum
        -- TODO: Create a new sum type to encapsulate the fact that we can also
        -- have a txin and render the txin in the case of reference scripts.
        Just (AnyScriptWitness (PlutusScriptWitness _ _ (PReferenceScript _refTxIn) _ _ _)) ->
          case eExecUnits of
            Right execUnits ->
              case calculateExecutionUnitsLovelace eUnitPrices execUnits of
                Just llCost ->
                  Right (ScriptCostOutput Nothing execUnits llCost)
                    : accum
                Nothing ->
                  Left (PlutusScriptCostErrRationalExceedsBound eUnitPrices execUnits)
                    : accum
            Left err -> Left (PlutusScriptCostErrExecError sWitInd Nothing err) : accum


        Nothing -> Left (PlutusScriptCostErrPlutusScriptNotFound sWitInd) : accum

    ) [] executionCostMapping
