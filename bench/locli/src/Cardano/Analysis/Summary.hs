{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

{- HLINT ignore "Use mapMaybe" -}

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
  -> RunLogs [LogObject]
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
               rl@RunLogs{..}
               sumFilters
               MachPerf{..}
               BlockProp{..}
               Chain{..}
  =
  Summary
  { sumHosts           = countMap rlHostLogs
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
  , cdfLogObjects        = cdf stdCentiles (objLists <&> length)
  , cdfLogObjectsEmitted = cdf stdCentiles logObjectsEmitted
  , cdfLogLinesEmitted   = cdf stdCentiles textLinesEmitted
  , cdfRuntime           = cdf stdCentiles runtimes
  , ..
  }
 where
   cdfLogLineRate       = cdf stdCentiles lineRates

   (,) logObjectsEmitted textLinesEmitted =
     rlHostLogs
     & Map.toList
     & fmap ((hlRawLogObjects &&& hlRawLines) . snd)
     & unzip
   objLists = rlLogs rl <&> snd

   (,) minStartRaw maxStartRaw = (minimum &&& maximum) losFirsts
   (,) minStopRaw  maxStopRaw  = (minimum &&& maximum) losLasts
   losFirsts  = objLists <&> loAt . Prelude.head
   losLasts   = objLists <&> loAt . Prelude.last
   runtimes :: [NominalDiffTime]
   runtimes   = zipWith diffUTCTime losLasts losFirsts
   lineRates  = zipWith (/) (textLinesEmitted <&> fromIntegral)
                            (runtimes <&> fromIntegral @Int . truncate)

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
