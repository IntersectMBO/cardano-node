{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.CLI.Shelley.Output
  ( QueryTipOutput(..)
  , QueryTipLocalState(..)
  , QueryTipLocalStateOutput(..)
  , ChainTipInfo(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.Prelude (Text)
import           Cardano.Slotting.Time (SystemStart (..))
import           Control.Monad
import           Data.Aeson (KeyValue, ToJSON (..), (.=))
import           Data.Function (id, ($), (.))
import           Data.Maybe
import           Data.Monoid (mconcat)
import           Shelley.Spec.Ledger.Scripts ()
import           Text.Show (Show)

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE

data ChainTipInfo = ChainTipInfo
  { mBlockNo :: BlockNo
  , mSlotNo :: SlotNo
  , mHeaderHash :: Text
  } deriving Show

data QueryTipOutput localState = QueryTipOutput
  { mChainTip :: Maybe ChainTipInfo
  , mLocalState :: Maybe localState
  }

data QueryTipLocalState mode = QueryTipLocalState
  { era :: AnyCardanoEra
  , eraHistory :: EraHistory CardanoMode
  , mSystemStart :: Maybe SystemStart
  , mChainTipInfo :: Maybe ChainTipInfo
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
  toJSON a = case mChainTip a of
    Nothing -> J.Null
    Just (ChainTipInfo slot bNum hh) ->
      J.object $
        ( ("slot" ..= slot)
        . ("hash" ..= hh)
        . ("block" ..= bNum)
        . ("era" ..=? (mLocalState a >>= mEra))
        . ("epoch" ..=? (mLocalState a >>= mEpoch))
        . ("syncProgress" ..=? (mLocalState a >>= mSyncProgress))
        ) []
  toEncoding a = case mChainTip a of
    Nothing -> JE.null_
    Just (ChainTipInfo slot bNum hh) ->
      J.pairs $ mconcat $
        ( ("slot" ..= slot)
        . ("hash" ..= hh)
        . ("block" ..= bNum)
        . ("era" ..=? (mLocalState a >>= mEra))
        . ("epoch" ..=? (mLocalState a >>= mEpoch))
        . ("syncProgress" ..=? (mLocalState a >>= mSyncProgress))
        ) []
