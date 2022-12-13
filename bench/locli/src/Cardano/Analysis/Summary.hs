{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

{- HLINT ignore "Use mapMaybe" -}

module Cardano.Analysis.Summary (module Cardano.Analysis.Summary) where

import Prelude                                      (head, last)
import Cardano.Prelude

import Data.Map.Strict                  qualified as Map

import Cardano.Analysis.API
import Cardano.Unlog.LogObject               hiding (Text)
import Cardano.Util


data SummaryError
  = SEEmptyDataset
  | SEIncoherentRunProfiles [Text]
  | SEIncoherentRunEras     [Text]
  | SEIncoherentRunVersions [Manifest]
  | SEIncoherentRunFilters  [([FilterName], [ChainFilter])]
  | SECDFError              CDFError
  deriving Show

summariseMultiSummary ::
     UTCTime
  -> [Centile]
  -> [SummaryOne]
  -> Either SummaryError MultiSummary
summariseMultiSummary _ _ [] = error "Asked to summarise empty list of Summary"
summariseMultiSummary sumAnalysisTime centiles xs@(headline:_) = do
  sumHosts               <- pure $ cdf centiles $ xs <&> unI . sumHosts
  sumLogObjectsTotal     <- pure $ cdf centiles $ xs <&> unI . sumLogObjectsTotal
  sumChainRejectionStats <- pure $ xs <&> unI . sumChainRejectionStats
                                   & traverse identity
                                   & fmap (traverse identity)
                                   & fmap (fmap $ cdf centiles)
  sumBlocksRejected      <- pure $ cdf centiles $ xs <&> unI . sumBlocksRejected
  sumDomainTime          <- pure $ xs <&> sumDomainTime
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumStartSpread         <- pure $ xs <&> sumStartSpread
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumStopSpread          <- pure $ xs <&> sumStopSpread
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumDomainSlots         <- pure $ xs <&> sumDomainSlots
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumDomainBlocks        <- pure $ xs <&> sumDomainBlocks
                                   & traverseDataDomain (cdf centiles . fmap unI)

  sumMeta                <- summariseMetadata $ xs <&> sumMeta
  sumFilters             <- allEqOrElse (xs <&> sumFilters) SEIncoherentRunFilters

  cdfLogLinesEmitted     <- sumCDF2 $ xs <&> cdfLogLinesEmitted
  cdfLogObjectsEmitted   <- sumCDF2 $ xs <&> cdfLogObjectsEmitted
  cdfLogObjects          <- sumCDF2 $ xs <&> cdfLogObjects
  cdfRuntime             <- sumCDF2 $ xs <&> cdfRuntime
  cdfLogLineRate         <- sumCDF2 $ xs <&> cdfLogLineRate
  pure $ Summary
    { ..
    }
 where
   summariseMetadata :: [Metadata] -> Either SummaryError Metadata
   summariseMetadata [] = Left SEEmptyDataset
   summariseMetadata xs@(headline:_) = do
     profile  <- allEqOrElse (xs <&> profile)  SEIncoherentRunProfiles
     era      <- allEqOrElse (xs <&> era)      SEIncoherentRunEras
     manifest <- allEqOrElse (xs <&> manifest) SEIncoherentRunVersions
     pure Metadata { tag = "", batch = batch headline, .. }

   allEqOrElse :: [a] -> ([a] -> SummaryError) -> Either SummaryError a
   allEqOrElse [] _ = Left SEEmptyDataset
   allEqOrElse xss@(headline:xs) err =
     all (== headline) xs
     & bool (Left $ err xss) (Right headline)

   sumCDF2 :: [CDF I a] -> Either SummaryError (CDF (CDF I) a)
   sumCDF2 xs = cdf2OfCDFs (stdCombine1 centiles) xs & bimap SECDFError identity

   -- comb :: forall a. Divisible a => Combine I a
   -- comb = stdCombine1 centiles

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
  { sumHosts           = I $ countMap rlHostLogs
  , sumLogObjectsTotal = I $ countListsAll objLists
  , sumBlocksRejected  = I $ countListAll cRejecta
  , sumDomainTime      =
      DataDomain (Interval minStartRaw maxStopRaw)  (Just $ Interval minStartFlt maxStopFlt)
                 (unI maxStopRaw  `utcTimeDeltaSec` unI minStartRaw)
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
