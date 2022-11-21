{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.Summary (module Cardano.Analysis.Summary) where

import Cardano.Prelude          hiding (head)

import Data.Map.Strict                  qualified as Map

import Cardano.Analysis.API
import Cardano.Unlog.LogObject


computeSummary ::
     UTCTime
  -> [[LogObject]]
  -> ([FilterName], [ChainFilter])
  -> DataDomain SlotNo
  -> DataDomain BlockNo
  -> [BlockEvents]
  -> Summary
computeSummary sumWhen
               objLists
               sumFilters
               sumDomainSlots
               sumDomainBlocks
               chainRejecta
  =
  Summary
  { sumLogStreams          = countOfList  objLists
  , sumLogObjects          = countOfLists objLists
  , sumBlocksRejected      = countOfList chainRejecta
  , ..
  }
 where
   sumChainRejectionStats =
     chainRejecta
     <&> fmap fst . filter (not . snd) . beAcceptance
      &  concat
      &  foldr' (\k m -> Map.insertWith (+) k 1 m) Map.empty
      &  Map.toList
