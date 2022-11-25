{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.Summary (module Cardano.Analysis.Summary) where

import Prelude                                      (head, last)
import Cardano.Prelude

import Data.Map.Strict                  qualified as Map

import Cardano.Analysis.API
import Cardano.Unlog.LogObject
import Cardano.Util


computeSummary ::
     UTCTime
  -> Metadata
  -> Genesis
  -> GenesisSpec
  -> GeneratorProfile
  -> [(Count Cardano.Prelude.Text, [LogObject])]
  -> ([FilterName], [ChainFilter])
  -> ClusterPerf
  -> BlockPropOne
  -> Chain
  -> Summary I
computeSummary sumAnalysisTime
               sumMeta
               sumGenesis
               sumGenesisSpec
               sumGenerator
               loCountsObjLists
               sumFilters
               MachPerf{..}
               BlockProp{..}
               Chain{..}
  =
  Summary
  { sumLogStreams      = countListAll  objLists
  , sumLogObjectsTotal = countListsAll objLists
  , sumBlocksRejected  = countListAll cRejecta
  , sumDomainTime      =
      DataDomain (Interval minStartRaw maxStopRaw)  (Just $ Interval minStartFlt maxStopFlt)
                 (maxStopRaw  `utcTimeDeltaSec` minStartRaw)
                 (maxStopFlt  `utcTimeDeltaSec` minStartFlt)
  , sumStartSpread     =
      DataDomain (Interval minStartRaw maxStartRaw) (Just $ Interval minStartFlt maxStartFlt)
                 (maxStartRaw `utcTimeDeltaSec` minStartRaw)
                 (maxStartFlt `utcTimeDeltaSec` minStartFlt)
  , sumStopSpread      =
      DataDomain (Interval minStopRaw  maxStopRaw)  (Just $ Interval minStopFlt  maxStopFlt)
                 (maxStopRaw  `utcTimeDeltaSec` minStopRaw)
                 (maxStopFlt  `utcTimeDeltaSec` minStopFlt)
  , sumDomainSlots     = Prelude.head mpDomainSlots
  , sumDomainBlocks    = Prelude.head bpDomainBlocks
  --
  , cdfLogObjects        = cdf stdCentiles           (length <$> objLists)
  , cdfLogObjectsEmitted = cdf stdCentiles (loCountsObjLists <&> unCount . fst)
  , ..
  }
 where
   objLists = loCountsObjLists <&> snd

   (,) minStartRaw maxStartRaw = (minimum &&& maximum) losFirsts
   (,) minStopRaw  maxStopRaw  = (minimum &&& maximum) losLasts
   losFirsts  = objLists <&> loAt . Prelude.head
   losLasts   = objLists <&> loAt . Prelude.last

   (,) minStartFlt maxStartFlt = (timeOf *** timeOf) startMinMaxS
   (,) minStopFlt  maxStopFlt  = (timeOf *** timeOf) stopMinMaxS
   startMinMaxS = (minimum &&& maximum) slotFirsts
   stopMinMaxS  = (minimum &&& maximum) slotLasts
   slotFirsts  = slotDomains <&> low
   slotLasts   = slotDomains <&> high
   slotDomains = catMaybes (ddFiltered <$> mpDomainSlots)
   timeOf = unSlotStart . slotStart sumGenesis

   sumChainRejectionStats =
     cRejecta
     <&> fmap fst . filter (not . snd) . beAcceptance
      &  concat
      &  foldr' (\k m -> Map.insertWith (+) k 1 m) Map.empty
      &  Map.toList
