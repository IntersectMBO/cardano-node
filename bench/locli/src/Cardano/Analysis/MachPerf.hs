{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.MachPerf (module Cardano.Analysis.MachPerf) where

import Prelude (error, head, last)
import Cardano.Prelude hiding (head)
import Cardano.Prelude qualified as CP

import Data.List                        ((!!))
import Data.Map.Strict qualified as Map
import Data.Text                        (pack, unpack)
import Data.Text.Short                  (toText)
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock qualified as Time

import Data.Distribution

import Cardano.Analysis.API
import Cardano.Analysis.Chain
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject hiding (Text)
import Cardano.Unlog.Resources


-- | A side-effect of analysis
data RunScalars
  = RunScalars
  { rsElapsed       :: Maybe NominalDiffTime
  , rsSubmitted     :: Maybe Word64
  , rsThreadwiseTps :: Maybe (Vector Double)
  }
  deriving stock Generic
  deriving anyclass NFData

collectSlotStats :: Run -> [(JsonLogfile, [LogObject])]
                 -> IO (Either Text [(JsonLogfile, (RunScalars, [SlotStats]))])
collectSlotStats run = fmap sequence <$> mapConcurrentlyPure (timelineFromLogObjects run)

runSlotFilters ::
     Run
  -> ([ChainFilter], [FilterName])
  -> [(JsonLogfile, [SlotStats])]
  -> IO (DataDomain SlotNo, [(JsonLogfile, [SlotStats])])
runSlotFilters Run{genesis} (flts, _fltNames) slots = do
  filtered <- mapConcurrentlyPure (fmap $ filterSlotStats flts) slots
  let samplePre  =    slots !! 0 & snd
      samplePost = filtered !! 0 & snd
      domain = mkDataDomain
        ((CP.head samplePre  <&> slSlot) & fromMaybe 0)
        ((lastMay samplePre  <&> slSlot) & fromMaybe 0)
        ((CP.head samplePost <&> slSlot) & fromMaybe 0)
        ((lastMay samplePost <&> slSlot) & fromMaybe 0)
        (fromIntegral . unSlotNo)
  progress "filtered-slotstats-slot-domain" $ J domain
  pure $ (,) domain filtered

 where
   filterSlotStats :: [ChainFilter] -> [SlotStats] -> [SlotStats]
   filterSlotStats filters =
     filter (\x -> all (testSlotStats genesis x) slotFilters)
    where
      slotFilters :: [SlotCond]
      slotFilters = catSlotFilters filters

data SlotStatsSummary
  = SlotStatsSummary
  { sssMissRatios       :: [Double]
  , sssSpanLensCpu      :: [Int]
  , sssSpanLensCpuEpoch :: [Int]
  , sssSpanLensCpuRwd   :: [Int]
  }

slotStatsSummary :: Run -> [SlotStats] -> SlotStatsSummary
slotStatsSummary Run{genesis=Genesis{epochLength}} slots =
  SlotStatsSummary{..}
 where
   sssMissRatios       = missRatio . (maxChecks -) <$> checkCounts
   sssSpanLensCpu      = spanLen <$> spansCpu
   sssSpanLensCpuRwd   = Vec.length <$> filter (spanContainsEpochSlot rewardCalcBeginSlot) spansCpu
   sssSpanLensCpuEpoch = Vec.length <$> spansCpuEpoch

   checkCounts = slCountChecks <$> slots
   maxChecks   = maximum checkCounts

   rewardCalcBeginSlot = 3 + floor @Double (fromIntegral epochLength * 0.4)

   missRatio :: Word64 -> Double
   missRatio = (/ fromIntegral maxChecks) . fromIntegral

   spansCpu :: [Vector SlotStats]
   spansCpu       = spans
                      ((/= Just False) . fmap (>=85) . rCentiCpu . slResources)
                      (toList slots)

   spansCpuEpoch :: [Vector SlotStats]
   spansCpuEpoch  = filter (spanContainsEpochSlot 3) spansCpu <&>
     \v-> let   tailEpoch =  slEpoch (Vec.last v)
          in if tailEpoch == slEpoch (Vec.head v) then v
             else Vec.dropWhile ((tailEpoch == ) . slEpoch) v

   spanLen :: Vector SlotStats -> Int
   spanLen = fromIntegral . unSlotNo . uncurry (-) . (slSlot *** slSlot) . (Vec.last &&& Vec.head)

   spanContainsEpochSlot :: Word64 -> Vector SlotStats -> Bool
   spanContainsEpochSlot s =
     uncurry (&&)
     . ((s >) . unEpochSlot . slEpochSlot . Vec.head &&&
        (s <) . unEpochSlot . slEpochSlot . Vec.last)

slotStatsMachPerf :: Run -> (JsonLogfile, [SlotStats]) -> Either Text (JsonLogfile, MachPerf)
slotStatsMachPerf _ (JsonLogfile f, []) =
  Left $ "slotStatsMachPerf:  zero filtered slots from " <> pack f
slotStatsMachPerf run (f, slots) =
  Right . (f,) $ MachPerf
  { sVersion                  = getVersion
  , sDomainSlots              = i $ mkDataDomainInj (slSlot $ head slots) (slSlot $ last slots)
                                                    (fromIntegral . unSlotNo)
  --
  , sMissDistrib              = dist sssMissRatios
  , sLeadsDistrib             = dist (slCountLeads <$> slots)
  , sUtxoDistrib              = dist (slUtxoSize <$> slots)
  , sDensityDistrib           = dist (slDensity <$> slots)
  , sSpanCheckDistrib         = dist (slSpanCheck `mapSMaybe` slots)
  , sSpanLeadDistrib          = dist (slSpanLead `mapSMaybe` slots)
  , sSpanForgeDistrib         = dist (filter (/= 0) $ slSpanForge `mapSMaybe` slots)
  , sBlocklessDistrib         = dist (slBlockless <$> slots)
  , sSpanLensCpuDistrib       = dist sssSpanLensCpu
  , sSpanLensCpuEpochDistrib  = dist sssSpanLensCpuEpoch
  , sSpanLensCpuRwdDistrib    = dist sssSpanLensCpuRwd
  , sResourceDistribs         = i <$> computeResDistrib stdPercSpecs resDistProjs slots
  }
 where
   i = Identity
   dist :: (Real a, ToRealFrac a Double) => [a] -> Identity (Distribution Double a)
   dist = i . computeDistribution stdPercSpecs

   SlotStatsSummary{..} = slotStatsSummary run slots

   resDistProjs     =
     Resources
     { rCentiCpu    = rCentiCpu   . slResources
     , rCentiGC     = rCentiGC    . slResources
     , rCentiMut    = rCentiMut   . slResources
     , rGcsMajor    = rGcsMajor   . slResources
     , rGcsMinor    = rGcsMinor   . slResources
     , rRSS         = rRSS        . slResources
     , rHeap        = rHeap       . slResources
     , rLive        = rLive       . slResources
     , rAlloc       = rAlloc      . slResources
     , rCentiBlkIO  = rCentiBlkIO . slResources
     , rThreads     = rThreads    . slResources
     }

-- The "fold" state that accumulates as we process 'LogObject's into a stream
-- of 'SlotStats'.
data TimelineAccum
  = TimelineAccum
  { aResAccums     :: ResAccums
  , aResTimestamp  :: UTCTime
  , aMempoolTxs    :: Word64
  , aBlockNo       :: Word64
  , aLastBlockSlot :: SlotNo
  , aSlotStats     :: [SlotStats]
  , aRunScalars    :: RunScalars
  , aTxsCollectedAt:: Map.Map TId UTCTime
  , aHost          :: Host
  }

forTAHead :: TimelineAccum -> (SlotStats -> SlotStats) -> TimelineAccum
forTAHead xs@TimelineAccum{aSlotStats=s:ss} f = xs {aSlotStats=f s:ss}

forTANth :: TimelineAccum -> Int -> (SlotStats -> SlotStats) -> TimelineAccum
forTANth xs@TimelineAccum{aSlotStats=ss, aHost} n f =
  xs { aSlotStats = mapNth f n ss }
 where
   mapNth :: (a -> a) -> Int -> [a] -> [a]
   mapNth f n xs =
     case splitAt n xs of
       (pre, x:post) -> pre <> (f x : post)
       _ -> error $ mconcat
            [ "mapNth: couldn't go ", show n, "-deep into the timeline, "
            , "host=", unpack . toText $ unHost aHost
            ]

timelineFromLogObjects :: Run -> (JsonLogfile, [LogObject])
                       -> Either Text (JsonLogfile, (RunScalars, [SlotStats]))
timelineFromLogObjects _ (JsonLogfile f, []) =
  Left $ "timelineFromLogObjects:  zero logobjects from " <> pack f
timelineFromLogObjects run@Run{genesis} (f, xs) =
  Right . (f,)
  $ foldl' (timelineStep run)
           zeroTimelineAccum
           xs
  & (aRunScalars &&& reverse . aSlotStats)
 where
   firstLogObjectTime :: UTCTime
   firstLogObjectTime = loAt (head xs)
   firstLogObjectHost :: Host
   firstLogObjectHost = loHost (head xs)

   zeroTimelineAccum :: TimelineAccum
   zeroTimelineAccum =
     TimelineAccum
     { aResAccums     = mkResAccums
     , aResTimestamp  = firstLogObjectTime
     , aMempoolTxs    = 0
     , aBlockNo       = 0
     , aLastBlockSlot = 0
     , aSlotStats     = [zeroSlotStats]
     , aRunScalars    = zeroRunScalars
     , aTxsCollectedAt= mempty
     , aHost          = firstLogObjectHost
     }
   zeroRunScalars :: RunScalars
   zeroRunScalars = RunScalars Nothing Nothing Nothing
   zeroSlotStats :: SlotStats
   zeroSlotStats =
     SlotStats
     { slSlot = impliedSlot genesis firstLogObjectTime
     , slEpoch = 0
     , slEpochSlot = 0
     , slEpochSafeInt = 0
     , slStart = SlotStart firstLogObjectTime
     , slCountChecks = 0
     , slCountLeads = 0
     , slCountForges = 0
     , slSpanCheck = SNothing
     , slSpanLead = SNothing
     , slSpanForge = SNothing
     , slSpanTxsMem = SNothing
     , slMempoolTxs = 0
     , slTxsCollected = 0
     , slTxsAccepted = 0
     , slTxsRejected = 0
     , slUtxoSize = 0
     , slDensity = 0
     , slResources = pure Nothing
     , slChainDBSnap = 0
     , slRejectedTx = 0
     , slBlockNo = 0
     , slBlockless = 0
     }

timelineStep :: Run -> TimelineAccum -> LogObject -> TimelineAccum
timelineStep Run{genesis} a@TimelineAccum{aSlotStats=cur:_, ..} =
  let continue :: SlotNo -> UTCTime -> TimelineAccum
      continue slot loAt =
        if slot < slSlot cur then a
        else a
             & (if slot - slSlot cur <= 1
                then identity                   -- for the next slot, no gap to patch
                else patchSlotGap genesis slot) -- for a future slot, patch the gap just until
             & if slot == slSlot cur
               then identity                    -- for the current slot, nothing to add
               else addTimelineSlot genesis slot loAt
      mapExistingSlot :: SlotNo -> (SlotStats -> SlotStats) -> TimelineAccum -> TimelineAccum
      mapExistingSlot slot fSlot a'@TimelineAccum{aSlotStats=last:_} =
        (if slot < slSlot last -- for a slot gone by
         then forTANth  a' (fromIntegral . unSlotNo $ slSlot last - slot) fSlot
         else forTAHead a' fSlot)
      forExistingSlot :: SlotNo -> TimelineAccum -> (SlotStats -> SlotStats) -> TimelineAccum
      forExistingSlot slot a' fSlot = mapExistingSlot slot fSlot a'
  in \case
  -- First, events that can extend the timeline:
  --
  LogObject{loAt, loBody=LOResources rs} ->
    continue slot loAt
    & mapExistingSlot slot
     (\sl -> sl { slResources = Just <$> extractResAccums accs })
    & \a' -> a' { aResAccums    = accs
                , aResTimestamp = loAt
                }
   where
     slot = impliedSlot genesis loAt
     accs = updateResAccums loAt rs aResAccums
  LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} ->
    continue slot loAt
    & mapExistingSlot slot
    (\sl@SlotStats{..} ->
        sl { slCountChecks = slCountChecks + 1
           , slSpanCheck = SJust $ loAt `sinceSlot` slStart
           , slUtxoSize = utxo
           , slDensity = density
           })
  -- Next, events that technically should extend the timeline,
  -- but we don't really care for their misattribution to incur the overhead:
  --
  LogObject{loBody=LOMempoolTxs txCount} ->
    (forTAHead a
      \s-> s { slMempoolTxs = txCount })
    { aMempoolTxs     = txCount }
  LogObject{loBody=LOMempoolRejectedTx} ->
    forTAHead a
      \s-> s { slRejectedTx = slRejectedTx cur + 1 }
  LogObject{loBody=LOLedgerTookSnapshot} ->
    forTAHead a
      \s-> s { slChainDBSnap = slChainDBSnap cur + 1 }
  LogObject{loBody=LOGeneratorSummary _noFails sent elapsed threadwiseTps} ->
    a { aRunScalars = aRunScalars
        { rsThreadwiseTps = Just threadwiseTps
        , rsElapsed       = Just elapsed
        , rsSubmitted     = Just sent
        }
      }
  LogObject{loBody=LOTxsCollected coll, loTid, loAt} ->
    (forTAHead a
      \s-> s { slTxsCollected = slTxsCollected cur + max 0 (fromIntegral coll) })
    { aTxsCollectedAt =
      aTxsCollectedAt &
      (\case
          Just{} -> Just loAt
          --   error $ mconcat
          --   ["Duplicate LOTxsCollected for tid ", show tid, " at ", show loAt]
          Nothing -> Just loAt)
      `Map.alter` loTid
    }
  LogObject{loBody=LOTxsProcessed acc rej, loTid, loAt} ->
    (forTAHead a
      \s@SlotStats{..}-> s
      { slSpanTxsMem =
          case loTid `Map.lookup` aTxsCollectedAt of
            Nothing ->
              -- error $ mconcat
              -- ["LOTxsProcessed missing LOTxsCollected for tid", show tid, " at ", show loAt]
              SJust $
              1.0
              +
              fromSMaybe 0 slSpanTxsMem
            Just base ->
              SJust $
              (loAt `Time.diffUTCTime` base)
              +
              fromSMaybe 0 slSpanTxsMem
      , slTxsAccepted = slTxsAccepted + acc
      , slTxsRejected = slTxsRejected + max 0 (fromIntegral rej)
      })
    { aTxsCollectedAt = loTid `Map.delete` aTxsCollectedAt
    }
  -- Next, events that rely on their slotstats to pre-exist:
  --
  LogObject{loAt, loHost, loBody=LOTraceLeadershipDecided slot yesNo} ->
    if slot > slSlot cur
    then error $ mconcat
         [ "LeadDecided for a future slot=", show slot
         , " cur=", show (slSlot cur)
         , " host=", unpack . toText $ unHost loHost
         ]
    else forExistingSlot slot a $
           onLeadershipCertainty loAt yesNo
   where
     onLeadershipCertainty :: UTCTime -> Bool -> SlotStats -> SlotStats
     onLeadershipCertainty now lead sl@SlotStats{..} =
       sl { slCountLeads = slCountLeads + if lead then 1 else 0
          , slSpanLead   = checkToCertainty
          }
      where
        checkAbsTime = slSpanCheck <&> (`Time.addUTCTime` unSlotStart slStart)
        checkToCertainty = (now `Time.diffUTCTime`) <$> checkAbsTime
  LogObject{loBody=LOBlockContext blockNo} ->
    (forTAHead a
      \s-> s { slBlockNo = blockNo
             , slBlockless = if newBlock then 0 else slBlockless cur
             })
    { aBlockNo        = blockNo
    , aLastBlockSlot  = if newBlock
                        then slSlot cur
                        else aLastBlockSlot
    }
   where
     newBlock = aBlockNo /= blockNo
  LogObject{loAt, loHost, loBody=LOBlockForged _ _ _ slot} ->
    if slot > slSlot cur
    then error $ mconcat
         [ "BlockForged for a future slot=", show slot
         , " cur=", show (slSlot cur)
         , " host=", unpack . toText $ unHost loHost
         ]
    else forExistingSlot slot a $
           onBlockForge loAt
   where
     onBlockForge :: UTCTime -> SlotStats -> SlotStats
     onBlockForge now sl@SlotStats{..} =
       sl { slCountForges = slCountForges + 1
          , slSpanForge   = certaintyToForge
          }
      where
        certaintyAbsTime = Time.addUTCTime
                           <$> slSpanLead
                           <*> (slSpanCheck <&> (`Time.addUTCTime` unSlotStart slStart))
        certaintyToForge = (now `Time.diffUTCTime`) <$> certaintyAbsTime
  _ -> a
timelineStep _ a = const a

patchSlotGap :: Genesis -> SlotNo -> TimelineAccum -> TimelineAccum
patchSlotGap genesis curSlot a@TimelineAccum{aSlotStats=last:_, ..} =
  a & go (unSlotNo $ curSlot - gapStartSlot) gapStartSlot
 where
   gapStartSlot = slSlot last + 1

   go :: Word64 -> SlotNo -> TimelineAccum -> TimelineAccum
   go 0      _         acc = acc
   go gapLen patchSlot acc =
     go (gapLen - 1) (patchSlot + 1) (acc & addGapSlot patchSlot)

   addGapSlot :: SlotNo -> TimelineAccum -> TimelineAccum
   addGapSlot slot acc =
    let (epoch, epochSlot) = genesis `unsafeParseSlot` slot in
      acc { aSlotStats = SlotStats
          { slSlot        = slot
          , slEpoch       = epoch
          , slEpochSlot   = epochSlot
          , slEpochSafeInt= slotEpochSafeInt genesis epochSlot
          , slStart       = slStart
            -- Updated as we see repeats:
          , slCountChecks = 0
          , slCountLeads  = 0
          , slCountForges = 0
          , slSpanCheck   = SNothing
          , slSpanLead    = SNothing
          , slSpanForge   = SNothing
          , slSpanTxsMem  = SNothing
          , slTxsCollected= 0
          , slTxsAccepted = 0
          , slTxsRejected = 0
          , slMempoolTxs  = aMempoolTxs
          , slUtxoSize    = slUtxoSize last
          , slDensity     = slDensity last
          , slChainDBSnap = 0
          , slRejectedTx  = 0
          , slBlockNo     = aBlockNo
          , slBlockless   = unSlotNo $ slot - aLastBlockSlot
          , slResources   = maybeDiscard
                            <$> discardObsoleteValues
                            <*> extractResAccums aResAccums}
          : aSlotStats acc
        }
    where maybeDiscard :: (Word64 -> Maybe Word64) -> Word64 -> Maybe Word64
          maybeDiscard f = f

          slStart = slotStart genesis slot

addTimelineSlot :: Genesis -> SlotNo -> UTCTime -> TimelineAccum -> TimelineAccum
addTimelineSlot genesis slot time a@TimelineAccum{..} =
  let (epoch, epochSlot) = genesis `unsafeParseSlot` slot in
    a { aSlotStats = SlotStats
        { slSlot        = slot
        , slEpoch       = epoch
        , slEpochSlot   = epochSlot
        , slEpochSafeInt= slotEpochSafeInt genesis epochSlot
        , slStart       = slStart
          -- Updated as we see repeats:
        , slCountChecks = 0
        , slCountLeads  = 0
        , slCountForges = 0
        , slSpanCheck   = SJust $ time `sinceSlot` slStart
        , slSpanLead    = SNothing
        , slSpanForge   = SNothing
        , slSpanTxsMem  = SNothing
        , slTxsCollected= 0
        , slTxsAccepted = 0
        , slTxsRejected = 0
        , slMempoolTxs  = aMempoolTxs
        , slUtxoSize    = 0
        , slDensity     = 0
        , slChainDBSnap = 0
        , slRejectedTx  = 0
        , slBlockNo     = aBlockNo
        , slBlockless   = unSlotNo $ slot - aLastBlockSlot
        , slResources   = maybeDiscard
                          <$> discardObsoleteValues
                          <*> extractResAccums aResAccums}
        : aSlotStats
      }
    where maybeDiscard :: (Word64 -> Maybe Word64) -> Word64 -> Maybe Word64
          maybeDiscard f = f

          slStart = slotStart genesis slot

data DerivedSlot
  = DerivedSlot
  { dsSlot      :: SlotNo
  , dsBlockless :: Word64
  }

derivedSlotsHeader :: String
derivedSlotsHeader =
  "Slot,Blockless span"

renderDerivedSlot :: DerivedSlot -> String
renderDerivedSlot DerivedSlot{..} =
  mconcat
  [ show (unSlotNo dsSlot), ",", show dsBlockless
  ]

computeDerivedVectors :: [SlotStats] -> ([DerivedSlot], [DerivedSlot])
computeDerivedVectors ss =
  (\(_,_,d0,d1) -> (d0, d1)) $
  foldr step (0, 0, [], []) ss
 where
   step ::
        SlotStats
     -> (Word64, Word64, [DerivedSlot], [DerivedSlot])
     -> (Word64, Word64, [DerivedSlot], [DerivedSlot])
   step SlotStats{..} (lastBlockless, spanBLSC, accD0, accD1) =
     if lastBlockless < slBlockless
     then ( slBlockless
          , slBlockless
          , DerivedSlot
            { dsSlot = slSlot
            , dsBlockless = slBlockless
            }:accD0
          , DerivedSlot
            { dsSlot = slSlot
            , dsBlockless = slBlockless
            }:accD1
          )
     else ( slBlockless
          , spanBLSC
          , DerivedSlot
            { dsSlot = slSlot
            , dsBlockless = spanBLSC
            }:accD0
          , accD1
          )
