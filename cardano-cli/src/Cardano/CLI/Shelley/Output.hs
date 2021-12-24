{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.CLI.Shelley.Output
  ( QueryTipLocalState(..)
  , QueryTipLocalStateOutput(..)
  ) where

import           Cardano.Api
import           Prelude

import           Data.Aeson (FromJSON (..), KeyValue, ToJSON (..), object, pairs, withObject, (.:?),
                   (.=))
import           Data.Text (Text)

import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.Ledger.Shelley.Scripts ()
import           Cardano.Slotting.Time (SystemStart (..))

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


