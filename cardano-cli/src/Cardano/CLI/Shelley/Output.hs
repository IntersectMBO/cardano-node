{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.CLI.Shelley.Output
  ( QueryTipOutput(..)
  , QueryTipLocalState(..)
  , QueryTipLocalStateOutput(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.Prelude (Either, Text)
import           Cardano.Slotting.EpochInfo (EpochInfo)
import           Cardano.Slotting.Time (SystemStart (..))
import           Control.Monad
import           Data.Aeson (KeyValue, ToJSON (..), (.=))
import           Data.Function (id, ($), (.))
import           Data.Maybe
import           Data.Monoid (mconcat)
import           Cardano.Ledger.Shelley.Scripts ()

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE

data QueryTipOutput localState = QueryTipOutput
  { chainTip :: ChainTip
  , mLocalState :: Maybe localState
  }

data QueryTipLocalState = QueryTipLocalState
  { era :: AnyCardanoEra
  , eraHistory :: EraHistory CardanoMode
  , mSystemStart :: Maybe SystemStart
  , epochInfo :: EpochInfo (Either TransactionValidityIntervalError)
  }

data QueryTipLocalStateOutput = QueryTipLocalStateOutput
  { mEra :: Maybe AnyCardanoEra
  , mEpoch :: Maybe EpochNo
  , mSyncProgress :: Maybe Text
  }

-- | A key-value pair difference list for encoding a JSON object.
(..=) :: (KeyValue kv, ToJSON v) => Text -> v -> [kv] -> [kv]
(..=) n v = (n .= v:)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(..=?) :: (KeyValue kv, ToJSON v) => Text -> Maybe v -> [kv] -> [kv]
(..=?) n mv = case mv of
  Just v -> (n .= v:)
  Nothing -> id

instance ToJSON (QueryTipOutput QueryTipLocalStateOutput) where
  toJSON a = case chainTip a of
    ChainTipAtGenesis -> J.Null
    ChainTip slot headerHash (BlockNo bNum) ->
      J.object $
        ( ("slot" ..= slot)
        . ("hash" ..= serialiseToRawBytesHexText headerHash)
        . ("block" ..= bNum)
        . ("era" ..=? (mLocalState a >>= mEra))
        . ("epoch" ..=? (mLocalState a >>= mEpoch))
        . ("syncProgress" ..=? (mLocalState a >>= mSyncProgress))
        ) []
  toEncoding a = case chainTip a of
    ChainTipAtGenesis -> JE.null_
    ChainTip slot headerHash (BlockNo bNum) ->
      J.pairs $ mconcat $
        ( ("slot" ..= slot)
        . ("hash" ..= serialiseToRawBytesHexText headerHash)
        . ("block" ..= bNum)
        . ("era" ..=? (mLocalState a >>= mEra))
        . ("epoch" ..=? (mLocalState a >>= mEpoch))
        . ("syncProgress" ..=? (mLocalState a >>= mSyncProgress))
        ) []
