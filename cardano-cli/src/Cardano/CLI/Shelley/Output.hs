module Cardano.CLI.Shelley.Output
  ( QueryTipOutput(..)
  ) where

import           Cardano.Api (AnyCardanoEra, ChainTip (..), EpochNo, serialiseToRawBytesHexText)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.Prelude (Eq, Show, Text)
import           Cardano.Slotting.Block (BlockNo (..))
import           Data.Aeson (ToJSON (..), (.=))
import           Data.Function (($))
import           Data.Maybe ( Maybe(..) )
import           Data.Monoid (mconcat)
import           Shelley.Spec.Ledger.Scripts ()

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.List as L

data QueryTipOutput = QueryTipOutput
  { chainTip :: ChainTip
  , era :: AnyCardanoEra
  , epoch :: Maybe EpochNo
  , syncProgress :: Maybe Text
  , errors :: [J.Value]
  } deriving (Eq, Show)

instance ToJSON QueryTipOutput where
  toJSON a = case chainTip a of
    ChainTipAtGenesis -> J.Null
    ChainTip slot headerHash (BlockNo bNum) ->
      J.object
        [ "slot" .= slot
        , "hash" .= serialiseToRawBytesHexText headerHash
        , "block" .= bNum
        , "era" .= era a
        , "epoch" .= epoch a
        , "syncProgress" .= syncProgress a
        , "errors" .= if L.null (errors a) then Nothing else Just (errors a)
        ]
  toEncoding a = case chainTip a of
    ChainTipAtGenesis -> JE.null_
    ChainTip slot headerHash (BlockNo bNum) ->
      J.pairs $ mconcat
        [ "slot" .= slot
        , "hash" .= serialiseToRawBytesHexText headerHash
        , "block" .= bNum
        , "era" .= era a
        , "epoch" .= epoch a
        , "syncProgress" .= syncProgress a
        , "errors" .= if L.null (errors a) then Nothing else Just (errors a)
        ]
