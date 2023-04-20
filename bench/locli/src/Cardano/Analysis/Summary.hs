{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

{- HLINT ignore "Use mapMaybe" -}

module Cardano.Analysis.Summary (module Cardano.Analysis.Summary) where

import Prelude                                      (head, last)
import Cardano.Prelude

import Data.Either.Extra                            (mapLeft)
import Data.Map.Strict                  qualified as Map
import Data.Text                        qualified as Text

import Cardano.Analysis.API
import Cardano.Unlog.LogObject               hiding (Text)
import Cardano.Util


data SummaryError
  = SEEmptyDataset
  | SEIncoherentRunGeneses       [Genesis]
  | SEIncoherentRunGenesisSpecs  [GenesisSpec]
  | SEIncoherentRunWorkloads     [GeneratorProfile]
  | SEIncoherentRunProfiles      [Text]
  | SEIncoherentRunEras          [Text]
  | SEIncoherentRunVersions      [Manifest]
  | SEIncoherentRunIdents        [Text]
  | SEIncoherentRunFilters       [([FilterName], [ChainFilter])]
  | SECDFError                   CDFError
  deriving Show

profilingCentiles :: [Centile]
profilingCentiles = nEquicentiles 5

summariseMultiSummary ::
     UTCTime
  -> [Centile]
  -> [SummaryOne]
  -> Either SummaryError MultiSummary
summariseMultiSummary _ _ [] = error "Asked to summarise empty list of Summary"
summariseMultiSummary sumAnalysisTime centiles xs@(headline:xss) = do
  sumHosts               <- pure $ cdf centiles $ xs <&> unI . sumHosts
  sumLogObjectsTotal     <- pure $ cdf centiles $ xs <&> unI . sumLogObjectsTotal
  sumChainRejectionStats <- pure $ xs <&> sumChainRejectionStats
                                 & concat
  sumBlocksRejected      <- pure $ cdf centiles $ xs <&> unI . sumBlocksRejected
  sumDomainTime          <- pure $ xs <&> sumDomainTime
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumStartSpread         <- pure $ xs <&> sumStartSpread
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumStopSpread          <- pure $ xs <&> sumStopSpread
                                   & traverseDataDomain (cdf centiles . fmap unI)
  sumDomainSlots         <- traverseDataDomain'
                              (mapLeft SECDFError
                               . collapseCDFs (stdCombine1 centiles))
                              (xs <&> sumDomainSlots)
  sumDomainBlocks        <- pure $ xs <&> sumDomainBlocks
                                   & traverseDataDomain (cdf centiles . fmap unI)

  sumMeta                <- summariseMetadata $ xs <&> sumMeta
  sumFilters             <- allEqOrElse (xs <&> sumFilters) SEIncoherentRunFilters

  cdfLogLinesEmitted     <- sumCDF2 $ xs <&> cdfLogLinesEmitted
  cdfLogObjectsEmitted   <- sumCDF2 $ xs <&> cdfLogObjectsEmitted
  cdfLogObjects          <- sumCDF2 $ xs <&> cdfLogObjects
  cdfRuntime             <- sumCDF2 $ xs <&> cdfRuntime
  cdfLogLineRate         <- sumCDF2 $ xs <&> cdfLogLineRate
  sumGenesis             <- find (not .genesesSameButTimeP (sumGenesis headline))
                                 (sumGenesis <$> xss)
                            & maybe (Right $ sumGenesis headline)
                              (Left .SEIncoherentRunGeneses .(sumGenesis headline:).(:[]))
  sumGenesisSpec         <- find (/= sumGenesisSpec headline)
                                 (sumGenesisSpec <$> xss)
                            & maybe (Right $ sumGenesisSpec headline)
                              (Left .SEIncoherentRunGenesisSpecs .(sumGenesisSpec headline:).(:[]))
  sumWorkload            <- find (/= sumWorkload headline)
                                 (sumWorkload <$> xss)
                            & maybe (Right $ sumWorkload headline)
                              (Left .SEIncoherentRunWorkloads .(sumWorkload headline:).(:[]))
  sumProfilingData       <- xs <&> sumProfilingData
                            & catMaybes
                            & \case
                                [] -> pure Nothing
                                pds -> Just <$> (collapseProfilingDataCDF
                                                 profilingCentiles pds
                                                 & mapLeft SECDFError)

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
     ident'   <- allEqOrElse (xs <&> ident)    SEIncoherentRunIdents
     -- XXX: magic transformation that happens to match
     --      the logic in 'analyse.sh multi-call' on line with "local run="
     pure Metadata { tag   = maximum (xs <&> tag) & Text.take 16 & (<> "_variance")
                   , batch = batch headline
                   , ident = ident'
                   , .. }

   allEqOrElse :: Eq a => [a] -> ([a] -> SummaryError) -> Either SummaryError a
   allEqOrElse [] _ = Left SEEmptyDataset
   allEqOrElse xss@(headline:xs) err =
     all (== headline) xs
     & bool (Left $ err xss) (Right headline)

   sumCDF2 :: Divisible a => [CDF I a] -> Either SummaryError (CDF (CDF I) a)
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
               sumWorkload
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
  , sumDomainSlots     = mpDomainCDFSlots
  , sumDomainBlocks    = bpDomainCDFBlocks
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

   losFirsts, losLasts :: [UTCTime]
   losFirsts  = objLists <&> loAt . Prelude.head
   losLasts   = objLists <&> loAt . Prelude.last
   runtimes :: [NominalDiffTime]
   runtimes   = zipWith diffUTCTime losLasts losFirsts
   lineRates  = zipWith (/) (textLinesEmitted <&> fromIntegral)
                            (runtimes <&> fromIntegral @Int . truncate)

   (,,) sumDomainTime sumStartSpread sumStopSpread =
     slotDomains sumGenesis (losFirsts, losLasts) mpDomainSlots

   sumChainRejectionStats :: [(ChainFilter, Int)]
   sumChainRejectionStats =
     cRejecta
     <&> fmap fst . filter (not . snd) . beAcceptance
      &  concat
      &  foldr' (\k m -> Map.insertWith (+) k 1 m) Map.empty
      &  Map.toList

   profileEntries = Map.elems rlHostLogs <&> hlProfile
   sumProfilingData = if all null profileEntries then Nothing
                      else Just $ profilingDataCDF profilingCentiles $
                           profileEntries <&> mkProfilingData

deriving newtype instance (Num (I Int))

slotDomains :: Genesis
            -> ([UTCTime], [UTCTime])
            -> [DataDomain I SlotNo]
            -> ( DataDomain I RUTCTime
               , DataDomain I RUTCTime
               , DataDomain I RUTCTime)
slotDomains gsis (firstLOs, lastLOs) (catMaybes . fmap ddFiltered -> xs) =
  ( DataDomain (I <$> Interval minStartRaw maxStopRaw)
        (Just $ I <$> Interval minStartFlt maxStopFlt)
               (I $ maxStopRaw  `utcTimeDeltaSec` minStartRaw)
               (I $ maxStopFlt  `utcTimeDeltaSec` minStartFlt)
    <&> toRUTCTime
  , DataDomain (I <$> Interval minStartRaw maxStartRaw)
        (Just $ I <$> Interval minStartFlt maxStartFlt)
               (I $ maxStartRaw `utcTimeDeltaSec` minStartRaw)
               (I $ maxStartFlt `utcTimeDeltaSec` minStartFlt)
    <&> toRUTCTime
  , DataDomain (I <$> Interval minStopRaw  maxStopRaw)
        (Just $ I <$> Interval minStopFlt  maxStopFlt)
               (I $ maxStopRaw  `utcTimeDeltaSec` minStopRaw)
               (I $ maxStopFlt  `utcTimeDeltaSec` minStopFlt)
    <&> toRUTCTime
  )
 where
   minStartRaw, maxStartRaw, minStopRaw, maxStopRaw :: UTCTime
   (,) minStartRaw maxStartRaw = (minimum &&& maximum) firstLOs
   (,) minStopRaw  maxStopRaw  = (minimum &&& maximum) lastLOs

   (,) minStartFlt maxStartFlt = (timeOf *** timeOf) startMinMaxS
   (,) minStopFlt  maxStopFlt  = (timeOf *** timeOf) stopMinMaxS
   startMinMaxS = (minimum &&& maximum) slotFirsts
   stopMinMaxS  = (minimum &&& maximum) slotLasts
   slotFirsts  = xs <&> unI . low
   slotLasts   = xs <&> unI . high

   timeOf = unSlotStart . slotStart gsis
