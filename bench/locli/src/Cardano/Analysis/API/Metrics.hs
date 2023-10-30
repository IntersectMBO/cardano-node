{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use null" -}
module Cardano.Analysis.API.Metrics (module Cardano.Analysis.API.Metrics) where

-- Prelude:
import Prelude                                       ((!!))
import Cardano.Prelude

-- Global, non-prelude:
import Data.Aeson
import Data.Aeson                       qualified as AE
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        qualified as T
import Data.Text.Short                  qualified as ST
import Data.Time.Clock
import Data.Map.Strict                  qualified as M

-- Repo-local:
import Cardano.Logging.Resources.Types

-- Package-local:
import Data.CDF

import Cardano.JSON
import Cardano.Util
import Cardano.Unlog.LogObject          qualified as LO
import Cardano.Analysis.API.Context
import Cardano.Analysis.API.Field
import Cardano.Analysis.API.Ground
import Cardano.Analysis.API.Types


sumFieldsReport :: [FieldName]
sumFieldsReport =
  [ "date.systemStart", "time.systemStart", "sumAnalysisTime"
  , "ident"
  , "batch"
  , "ghc_version"
  ] ++ (FieldName <$> manifestPackages) ++
  [ "era"
  , "delegators", "utxo"
  , "add_tx_size", "inputs_per_tx", "outputs_per_tx" , "tps", "tx_count"
  , "plutusScript"
  , "sumHosts", "sumLogObjectsTotal"
  , "sumFilters"
  , "cdfLogLinesEmitted", "cdfLogObjectsEmitted", "cdfLogObjects"
  , "cdfRuntime", "cdfLogLineRate"
  , "ddRawCount.sumDomainTime", "ddFilteredCount.sumDomainTime", "dataDomainFilterRatio.sumDomainTime"
  , "ddRaw.sumStartSpread", "ddRaw.sumStopSpread"
  , "ddFiltered.sumStartSpread", "ddFiltered.sumStopSpread"
  , "sumDomainSlots", "sumDomainBlocks", "sumBlocksRejected"]

instance (KnownCDF f) => TimelineFields (Summary f)  where
  data TimelineComments (Summary f)
    deriving Show

  timelineFields =
      fScalar "sumAnalysisTime"        W10 Dat (IText $ showText.roundUTCTimeDay.sumAnalysisTime)
      "Analysis date"
      "Date of analysis"

   <> fScalar "date.systemStart"       W10 Dat (IDate $ systemStart.sumGenesis)
      "Cluster system start date"
      "Date of cluster genesis systemStart"

   <> fScalar "time.systemStart"       W8  Tim (ITime $ systemStart.sumGenesis)
      "Cluster system start time"
      "Time-of-day portion of cluster genesis systemStart"

   <> fScalar   "ident"                Wno Id  (IText $ ident.sumMeta)
      "Identifier"
      ""

   <> fScalar   "batch"                Wno Id  (IText $ batch.sumMeta)
      "Run batch"
      ""

   <> fScalar   "ghc_version"          Wno Id  (IText $ node_ghc_version.sumMeta)
      "GHC version"
      "GHC version used to build cardano-node"

   <> manifestFields

   <> fScalar "era"                    Wno Era (IText $                     era.sumMeta)
      "Era"
      "Benchmark era"

   <> fScalar "delegators"             W12 Cnt (IWord64 $     delegators.sumGenesisSpec)
      "Delegation map size"
      ""

   <> fScalar "utxo"                   W12 Cnt (IWord64 $           utxo.sumGenesisSpec)
      "Starting UTxO set size"
      "Extra UTxO set size at the beginning of the benchmark"

   <> fScalar "add_tx_size"            W6  B   (IWord64 $      add_tx_size.sumWorkload
                                                               )
      "Extra tx payload"
      ""

   <> fScalar "inputs_per_tx"          W3  Cnt (IWord64 $    inputs_per_tx.sumWorkload)
      "Tx inputs"
      ""

   <> fScalar "outputs_per_tx"         W3  Cnt (IWord64 $   outputs_per_tx.sumWorkload)
      "Tx Outputs"
      ""

   <> fScalar "tps"                    W7  Hz  (IFloat $               tps.sumWorkload)
      "TPS"
      "Offered load, transactions per second"

   <> fScalar "tx_count"               W12 Cnt (IWord64 $         tx_count.sumWorkload)
      "Transaction count"
      "Number of transactions prepared for submission, but not necessarily submitted"

   <> fScalar "plutusScript"           Wno Id  (IText  $ fromMaybe "---".plutusLoopScript.sumWorkload)
      "Plutus script"
      "Name of th Plutus script used for smart contract workload generation, if any"

   <> fScalar "sumHosts"               W4  Cnt
        (IInt $ unCount.arityProj cdfMedian.sumHosts)
      "Machines"
      "Number of machines under analysis"

   <> fScalar "sumFilters"             W2  Cnt (IInt   $   length.snd.sumFilters)
      "Number of filters applied"
      ""

   <> fScalar "cdfLogLinesEmitted"     W12 Cnt (IFloat $ cdfAverageVal.cdfLogLinesEmitted)
      "Log text lines emitted per host"
      ""

   <> fScalar "cdfLogObjectsEmitted"   W12 Cnt (IFloat $ cdfAverageVal.cdfLogObjectsEmitted)
      "Log objects emitted per host"
      ""

   <> fScalar "cdfLogObjects"          W12 Cnt (IFloat $ cdfAverageVal.cdfLogObjects)
      "Log objects analysed per host"
      ""

   <> fScalar "cdfRuntime"             W7  Sec (IFloat $ cdfAverageVal.cdfRuntime)
      "Host run time, s"
      ""

   <> fScalar "cdfLogLineRate"         W6  Hz (IFloat $ cdfAverageVal.cdfLogLineRate)
      "Host log line rate, Hz"
      ""

   <> fScalar "sumLogObjectsTotal"     W12 Cnt (IInt   $ unCount.arityProj cdfMedian.sumLogObjectsTotal)
      "Total log objects analysed"
      ""

   <> fScalar "ddRawCount.sumDomainTime"      W12 Sec (IInt $ arityProj cdfMedian.ddRawCount.sumDomainTime)
      "Run time, s"
      ""

   <> fScalar "ddFilteredCount.sumDomainTime" W12 Sec (IInt $ arityProj cdfMedian.ddFilteredCount.sumDomainTime)
      "Analysed run duration, s"
      ""

   <> fScalar "dataDomainFilterRatio.sumDomainTime" W4 Rto (IFloat $ dataDomainFilterRatio (arityProj cdfMedian).sumDomainTime)
      "Run time efficiency"
      ""

   <> fScalar "ddRaw.sumStartSpread"      W9 Sec (IDeltaT$ intvDurationSec.fmap (fromRUTCTime . arityProj cdfMedian).ddRaw.sumStartSpread)
      "Node start spread, s"
      ""

   <> fScalar "ddRaw.sumStopSpread"       W9 Sec (IDeltaT$ intvDurationSec.fmap (fromRUTCTime . arityProj cdfMedian).ddRaw.sumStopSpread)
      "Node stop spread, s"
      ""

   <> fScalar "ddFiltered.sumStartSpread" W9 Sec (IDeltaT$ maybe 0 (intvDurationSec.fmap (fromRUTCTime . arityProj cdfMedian)).ddFiltered.sumStartSpread)
      "Perf analysis start spread, s"
      ""

   <> fScalar "ddFiltered.sumStopSpread"  W9 Sec (IDeltaT$ maybe 0 (intvDurationSec.fmap (fromRUTCTime . arityProj cdfMedian)).ddFiltered.sumStopSpread)
      "Perf analysis stop spread, s"
      ""

   <> fScalar "sumDomainSlots"         W12 Slo (IInt  $ floor.arityProj cdfMedian.cdfAverage.ddFilteredCount.sumDomainSlots)
      "Slots analysed"
      ""

   <> fScalar "sumDomainBlocks"        W10 Blk (IInt  $ arityProj cdfMedian.ddFilteredCount.sumDomainBlocks)
      "Blocks analysed"
      ""

   <> fScalar "sumBlocksRejected"      W10 Cnt (IInt  $ unCount.arityProj cdfMedian.sumBlocksRejected)
      "Blocks rejected"
      ""
   where
     pkgCommit pkg = fScalar pkg W7  Ver
                     (IText $ unCommit.ciCommit.getComponent pkg.manifest.sumMeta)
                     (pkg <> " git") ""
     pkgVersion pkg = fScalar pkg W12  Ver
                     (IText $ unVersion.ciVersion.getComponent pkg.manifest.sumMeta)
                     (pkg <> " version") ""
     manifestFields = mconcat . mconcat $
                       [ manifestPackages <&> pkgVersion
                       , manifestPackages <&> pkgCommit
                       ]


-- fieldJSONOverlay f = (:[]) . tryOverlayFieldDescription f

propSubsetFn :: PropSubset -> (Field DSelect p a -> Bool)
propSubsetFn = \case
  PropFull          -> const True
  PropControl       -> dFields bpFieldsControl
  PropForger        -> dFields bpFieldsForger
  PropPeers         -> dFields bpFieldsPeers
  PropEndToEnd      -> dFields bpFieldsEndToEnd
  PropEndToEndBrief -> dFields bpFieldsEndToEndBrief

bpFieldsControl, bpFieldsForger, bpFieldsPeers, bpFieldsEndToEnd, bpFieldsEndToEndBrief :: [FieldName]
bpFieldsControl =
  [ "cdfBlocksPerHost", "cdfBlocksFilteredRatio", "cdfBlocksChainedRatio", "cdfBlockBattle", "cdfBlockSize" ]
bpFieldsForger =
  [ "cdfForgerStart", "cdfForgerBlkCtx", "cdfForgerLgrState", "cdfForgerLgrView", "cdfForgerLead", "cdfForgerTicked", "cdfForgerMemSnap", "cdfForgerForge", "cdfForgerAnnounce", "cdfForgerSend", "cdfForgerAdoption", "cdfForgerAnnounceCum" ]
bpFieldsPeers =
  [ "cdfPeerNoticeFirst", "cdfPeerFetchFirst", "cdfPeerRequest", "cdfPeerFetch", "cdfPeerAnnounce", "cdfPeerSend", "cdfPeerAdoption" ]
bpFieldsEndToEnd =
  adoptionCentiles      <&> FieldName . renderAdoptionCentile
bpFieldsEndToEndBrief =
  adoptionCentilesBrief <&> FieldName . renderAdoptionCentile

instance CDFFields BlockProp p where
  cdfFields =
      fGrp ",------------------ Forger event Δt: --------------------."
            W4 Sec P3 Log Free
    [ fGrp' "cdfForgerStart"         "Loop" (DDeltaT cdfForgerStart)
      "Started forge loop iteration"
      "Forge loop iteration delay (TraceStartLeadershipCheck), relative to slot start"
    , fGrp' "cdfForgerBlkCtx"        "BkCt" (DDeltaT cdfForgerBlkCtx)
      "Acquired block context"
      "Block context acquired (TraceBlockContext), relative to forge loop beginning"

    , fGrp' "cdfForgerLgrState"      "LgSt" (DDeltaT cdfForgerLgrState)
      "Acquired ledger state"
      "Ledger state acquired (TraceLedgerState), relative to block context acquisition"

    , fGrp' "cdfForgerLgrView"       "LgVi" (DDeltaT cdfForgerLgrView)
      "Acquired ledger view"
      "Ledger view acquired (TraceLedgerView), relative to ledger state acquisition"

    , fGrp' "cdfForgerLead"          "Lead" (DDeltaT cdfForgerLead)
      "Leadership check duration"
      "Leadership check duration (TraceNodeIsNotLeader, TraceNodeIsLeader), relative  to ledger view acquisition"

    , fGrp' "cdfForgerTicked"        "Tick" (DDeltaT cdfForgerTicked)
      "Ledger ticking"
      "Time spent ticking the ledger state (TraceForgeTickedLedgerState), relative to leadership check completion"

    , fGrp' "cdfForgerMemSnap"       "Snap" (DDeltaT cdfForgerMemSnap)
      "Mempool snapshotting"
      "Time spent taking a mempool snapshot (TraceForgingMempoolSnapshot), relative to ledger ticking conclusion"

    , fGrp' "cdfForgerForge"         "Forg" (DDeltaT cdfForgerForge)
      "Leadership to forged"
      "Time spent forging the block: TraceForgedBlock relative to positive leadership decision"

    , fGrp' "cdfForgerAnnounce"      "Anno" (DDeltaT cdfForgerAnnounce)
      "Forged to announced"
      "Time between block forging completion and header announcement (ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock)"

    , fGrp' "cdfForgerSend"          "Send" (DDeltaT cdfForgerSend)
      "Forged to sending"
      "Time between block forging completion and begin-of-sending (TraceBlockFetchServerSendBlock)"

    , fGrp' "cdfForgerAdoption"      "Adop" (DDeltaT cdfForgerAdoption)
      "Forged to self-adopted"
      "Time between block forging completion and adoption (TraceAdoptedBlock)"

    , fGrp' "cdfForgerAnnounceCum"   "AnnC" (DDeltaT cdfForgerAnnounceCum)
      "Slot start to announced"
      "Time since slot start until header announcement (ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock)"
    ]
   <> fGrp ",------ Peer event Δt: ------."
            W4 Sec P3 Log Free
    [ fGrp' "cdfPeerNoticeFirst"     "NtiF" (DDeltaT cdfPeerNoticeFirst)
      "First peer notice"
      "Time it took for the fastest peer to notice the block (ChainSyncClientEvent.TraceDownloadedHeader), since block's slot start"

    , fGrp' "cdfPeerFetchFirst"      "FetF" (DDeltaT cdfPeerFetchFirst)
      "First peer fetch"
      "Time it took for the fastest peer to fetch the block (BlockFetchClient.CompletedBlockFetch), since block's slot start"

    , fGrp' "cdfPeerRequest"         "Requ" (DDeltaT cdfPeerRequest)
      "Notice to fetch request"
      "Time it took the peer to request the block body (BlockFetchClient.SendFetchRequest), after it have seen its header"

    , fGrp' "cdfPeerFetch"           "Fetc" (DDeltaT cdfPeerFetch)
      "Fetch duration"
      "Time it took the peer to complete fetching the block (BlockFetchClient.CompletedBlockFetch), after having requested it"

    , fGrp' "cdfPeerAnnounce"        "Anno" (DDeltaT cdfPeerAnnounce)
      "Fetched to announced"
      "Time it took a peer to announce the block (ChainSyncServerEvent.TraceChainSyncServerUpdate), since it was fetched"

    , fGrp' "cdfPeerSend"            "Send" (DDeltaT cdfPeerSend)
      "Fetched to sending"
      "Time until the peer started sending the block (BlockFetchServer.SendBlock), since it was fetched"

    , fGrp' "cdfPeerAdoption"        "Adop" (DDeltaT cdfPeerAdoption)
      "Fetched to adopted"
      "Time until the peer adopts the block (TraceAddBlockEvent.AddedToCurrentChain), since it was fetched"
    ]
   <> [ Field (renderAdoptionCentile ct) (r!!i)
                                         (T.take 4 $ T.pack $ printf "%.04f" centi)
                                        W4 Sec P3 Log Free
        (DDeltaT $
          checkCentile i centi
          . fromMaybe (error $ printf "No centile %d/%f in bpPropagation."
                        i centi)
          . flip atMay i
          . M.toList
          . bpPropagation)
        (T.pack $ printf "%.2f adoption" centi)
        (T.pack $ printf "Time since slot start to block's adoption by %d%% of the cluster." (ceiling $ centi * 100 :: Int))
            -- (T.pack $ printf "Block adopted by %.2f fraction of the entire cluster." centi)
      | (i, ct@(Centile centi)) <- zip [0::Int ..] adoptionCentiles
      ]

   <> fGrpF "Host block stats" W5
    [ fGrpF' "cdfBlocksPerHost"       "total" Blk P0 Lin Free (DInt cdfBlocksPerHost)
      "Blocks per host"
      "For each host, number of blocks made during the entire observation period"

    , fGrpF' "cdfBlocksFilteredRatio" "fltrd" Rto P3 Lin R01 (DFloat cdfBlocksFilteredRatio)
      "Filtered to chained block ratio"
      "For each host, ratio of blocks that passed filtering / all on chain"

    , fGrpF' "cdfBlocksChainedRatio" "chaind" Rto P3 Lin R01 (DFloat cdfBlocksChainedRatio)
      "Chained to forged block ratio"
      "For each host, ratio of blocks that made into chain / all forged"]

   <> fBoth "cdfBlockBattle"  "Battl" " #" W4 Blk P0 Lin Free (DInt cdfBlockBattle)
      "Height & slot battles"
      "For a given block, number of all abandoned blocks at its block height.  Sum of height and slot battles"

   <> fBoth "cdfBlockSize"  "Size" "bytes" W9 B   P0 Lin Free (DInt cdfBlockSize)
      "Block size"
      "Block size, in bytes"
   where r = nChunksEachOf (length adoptionCentiles) 5
             ",-- Slot-rel. Δt to adoption centile: -."
         checkCentile i centi (centi', d) =
           if T.unpack centi' == printf "cdf%.2f" centi then d
           else error $ printf "Centile mismatch: [%d]: exp=%f act=%s"
                i centi (T.unpack centi')

  fieldJSONOverlay f kvs =
    [ overlay kvs
    , alterSubObject overlay "bpPropagation" kvs
    ]
   where overlay = tryOverlayFieldDescription f

instance TimelineFields BlockEvents where
  timelineFields =
      fBoth' "beBlockNo"   "block" "no."   W5 Blk P0 Lin Free (IWord64 (unBlockNo.beBlockNo))
   <> fBoth' "beSlotNo"    "abs."  "slot#" W5 Slo P0 Lin Free (IWord64 (unSlotNo .beSlotNo))
   <> fBoth' "beBlock"     "block" "hash"  W6 Hsh P0 Lin Free (IText   (shortHash.beBlock))
   <> fBoth' "beBlockPrev" "prev"  "hash"  W6 Hsh P0 Lin Free (IText   (shortHash.beBlockPrev))
   <> fBoth' "bfForger"    "forger" "host" W7 Hos P0 Lin Free (IText   (ST.toText.unHost.bfForger . beForge))
   <> fBoth' "bfBlockSize" "size"  "bytes" W6 B   P0 Lin Free (IInt    (bfBlockSize.beForge))
   <> fBoth' "bfBlockGap"  "block" "gap"   W5 Len P0 Lin Free (IDeltaT (bfBlockGap .beForge))
   <> fBoth' "bpeIsFork"   "for"  "-ks"    W3 Blk P0 Lin Free (IInt    (count bpeIsFork.beErrors))
   <> fGrp "--------- Forger event Δt: ---------"
            W4 Sec P3 Log Free
    [ fGrp' "bfStarted"     "Start"  (IDeltaT (bfStarted  .beForge)) "" ""
    , fGrp' "bfBlkCtx"      "BlkCtx" (IDeltaTM (bfBlkCtx  .beForge)) "" ""
    , fGrp' "bfLgrState"    "LgrSta" (IDeltaTM (bfLgrState.beForge)) "" ""
    , fGrp' "bfLgrView"     "LgrVie" (IDeltaTM (bfLgrView .beForge)) "" ""
    , fGrp' "bfLeading"     "Lead"   (IDeltaT (bfLeading  .beForge)) "" ""
    , fGrp' "bfTicked"      "LgrTck" (IDeltaTM (bfTicked  .beForge)) "" ""
    , fGrp' "bfMemSnap"     "MemSna" (IDeltaTM (bfMemSnap .beForge)) "" ""
    , fGrp' "bfForged"      "Forge"  (IDeltaT (bfForged   .beForge)) "" ""
    , fGrp' "bfAnnounced"   "Announ" (IDeltaT (bfAnnounced.beForge)) "" ""
    , fGrp' "bfSending"     "Sendin" (IDeltaT (bfSending  .beForge)) "" ""
    , fGrp' "bfAdopted"     "Adopt"  (IDeltaT (bfAdopted  .beForge)) "" ""
    , fGrp' "bfAnnouncedCum" "AnnCum"(IDeltaT (bfAnnouncedCum.beForge)) "" ""
    ]
   <> fGrp "-- Peer event Δt averages: --"
            W4 Sec P3 Log Free
    [ fGrp' "boNoticed"     "Notic"  (IDeltaT (af  boNoticed   . valids)) "" ""
    , fGrp' "boRequested"   "Requd"  (IDeltaT (af  boRequested . valids)) "" ""
    , fGrp' "boFetched"     "Fetch"  (IDeltaT (af  boFetched   . valids)) "" ""
    , fGrp' "boAnnounced"   "Annou"  (IDeltaT (af' boAnnounced . valids)) "" ""
    , fGrp' "boSending"     "Send"   (IDeltaT (af' boSending   . valids)) "" ""
    , fGrp' "pAdopted"      "Adopt"  (IDeltaT (af' boAdopted   . valids)) "" ""
    ]
   <> fGrp "Propagation Δt:"
            W4 Sec P3 Log Free
    [ fGrp' "0.50"          "0.5"  (IDeltaT (percSpec 0.50 . bePropagation)) "" ""
    , fGrp' "0.80"          "0.8"  (IDeltaT (percSpec 0.80 . bePropagation)) "" ""
    , fGrp' "0.96"          "0.96" (IDeltaT (percSpec 0.96 . bePropagation)) "" ""
    , fGrp' "1.00"          "1.0"  (IDeltaT (high .cdfRange. bePropagation)) "" ""
    ]
   <> fBoth' "beAcceptance" "va-" "lid" W3 Sig P0 Lin Free (IText (bool "-" "+" . (== 0) . length
                                                          . filter (not . snd) . beAcceptance))
   <> fBoth' "valids"     "good" "obsv" W3 Ev  P0 Lin Free (IInt  (length.valids))
   <> fBoth' "beErrors"   "all"  "errs" W5 Ev  P0 Lin Free (IInt  (length.beErrors))

   <> fGrp "Missing"
            W3 Ev P0 Lin Free
    [ fGrp' "missNotic"   "ntc" (IInt (count (bpeIsMissing Notice)  .beErrors)) "" ""
    , fGrp' "missReque"   "req" (IInt (count (bpeIsMissing Request) .beErrors)) "" ""
    , fGrp' "missFetch"   "fch" (IInt (count (bpeIsMissing Fetch)   .beErrors)) "" ""
    , fGrp' "missAnnou"   "ann" (IInt (count (bpeIsMissing Announce).beErrors)) "" ""
    , fGrp' "missAdopt"   "ado" (IInt (count (bpeIsMissing Adopt)   .beErrors)) "" ""
    , fGrp' "missSend"    "snd" (IInt (count (bpeIsMissing Send)    .beErrors)) "" ""
    ]
   <> fGrp "Negative"
            W3 Ev P0 Lin Free
    [ fGrp' "negAnnou"   "ann" (IInt (count (bpeIsNegative Announce).beErrors)) "" ""
    , fGrp' "negSend"    "snd" (IInt (count (bpeIsNegative Send)    .beErrors)) "" ""
    ]
   where
     valids = filter isValidBlockObservation . beObservations

     percSpec :: Double -> CDF I NominalDiffTime -> NominalDiffTime
     percSpec ps d = unI $ Centile ps `projectCDF` d
       & fromMaybe (error $ printf "No centile %f in distribution." ps)
     af  f = avg . fmap f
     af' f = avg . mapSMaybe f
     avg :: [NominalDiffTime] -> NominalDiffTime
     avg [] = 0
     avg xs =  (/ fromInteger (fromIntegral $ length xs)) $ sum xs

     bpeIsFork :: BPError -> Bool
     bpeIsFork BPError{eDesc=BPEFork{}} = True
     bpeIsFork _ = False

     bpeIsMissing, bpeIsNegative  :: Phase -> BPError -> Bool
     bpeIsMissing  p BPError{eDesc=BPEMissingPhase p'} = p == p'
     bpeIsMissing  _ _ = False
     bpeIsNegative p BPError{eDesc=BPENegativePhase p' _} = p == p'
     bpeIsNegative _ _ = False

  data TimelineComments BlockEvents
    = BEErrors
    | BEFilterOuts
    deriving Show

  rtCommentary BlockEvents{..} =
    \case
      BEErrors     -> ("           " <>) . show <$> beErrors
      BEFilterOuts -> ("           " <>) . show <$> filter (not . snd) beAcceptance

perfSubsetFn :: PerfSubset -> (Field DSelect p a -> Bool)
perfSubsetFn = \case
  PerfFull   -> const True
  PerfReport -> dFields mtFieldsReport

mtFieldsReport :: [FieldName]
mtFieldsReport =
  [ "CentiCpu", "CentiGC", "CentiMut", "cdfSpanLensCpu", "RSS", "Heap", "Live", "Alloc", "GcsMinor", "GcsMajor", "NetRd", "NetWr", "FsRd", "FsWr", "cdfStarts" ]

instance CDFFields MachPerf p where
  cdfFields =
      fBoth "cdfStarts" "Loop" "starts" W4 Uni P0 Lin (Z0 1)  (DWord64 cdfStarts)
      "Forge loop starts"
      "For any given slot, how many forging loop starts were registered"

   <> fGrp "----------- Δt -----------" W4 Sec P3 Log Free
    [ fGrp' "cdfStarted"     "Start"       (DDeltaT cdfStarted)
      "Forge loop tardiness"
      "Forge loop iteration start delay (TraceStartLeadershipCheck), relative to slot start"

    , fGrp' "cdfBlkCtx"      "BlkCt"       (DDeltaT cdfBlkCtx)
      "Block context acquisition delay"
      "Block context acquired (TraceBlockContext), relative to forge loop beginning"

    , fGrp' "cdfLgrState"    "LgrSt"       (DDeltaT cdfLgrState)
      "Ledger state acquisition delay"
      "Ledger state acquired (TraceLedgerState), relative to block context acquisition"

    , fGrp' "cdfLgrView"     "LgrVi"       (DDeltaT cdfLgrView)
      "Ledger view acquisition delay"
      "Ledger view acquired (TraceLedgerView), relative to ledger state acquisition"

    , fGrp' "cdfLeading"     "Lead"        (DDeltaT cdfLeading)
      "Leadership check duration"
      "Leadership check duration (TraceNodeIsNotLeader, TraceNodeIsLeader), relative  to ledger view acquisition"
    ]
   <> fBoth "cdfBlockGap" "Block" "gap"  W4 Sec P2 Lin Free (DWord64 cdfBlockGap)
      "Interblock gap"
      "Time between blocks"

   <> fBoth "cdfDensity" "Chain" "dens." W5 Rto P2 Lin Free (DFloat cdfDensity)
      "Chain density"
      "Block/slot ratio, for the last 'k' slots"

   <> fPct  "CentiCpu"            "CPU" Free (DWord64 (rCentiCpu.mpResourceCDFs))
      "Process CPU usage"
      "Kernel-reported CPU process usage, % of a single core"

   <> fPct  "CentiGC"              "GC" Free (DWord64 (rCentiGC .mpResourceCDFs))
      "RTS GC CPU usage"
      "RTS-reported GC CPU usage, % of a single core"

   <> fPct  "CentiMut"            "MUT" Free (DWord64 (rCentiMut.mpResourceCDFs))
      "RTS Mutator CPU usage"
      "RTS-reported mutator CPU usage, % of a single core"
   <> fW64 "GcsMajor"        "GC" "Maj" W3 Ev    (DWord64 (rGcsMajor.mpResourceCDFs))
      "Major GCs"
      "Major garbage collection RTS events"

   <> fW64 "GcsMinor"        "GC" "Min" W3 Ev    (DWord64 (rGcsMinor.mpResourceCDFs))
      "Minor GCs"
      "Minor garbage collection RTS events"

   <> fGrp  "Memory usage, MB"          W5 MB P0 Lin Free
    [ fGrp' "RSS"                 "RSS"    (DWord64 (rRSS.mpResourceCDFs))
      "Kernel RSS"
      "Kernel-reported RSS (Resident Set Size) of the process, MB"

    , fGrp' "Heap"               "Heap"    (DWord64 (rHeap.mpResourceCDFs))
      "RTS heap size"
      "RTS-reported heap size, MB"

    , fGrp' "Live"               "Live"    (DWord64 (rLive.mpResourceCDFs))
      "RTS live GC dateset"
      "RTS-reported GC live data size, MB"
    ]
   <> fBoth "Alloc"  "Alloc" "MB/s"     W5 MBs P0 Lin Free (DWord64 (rAlloc.mpResourceCDFs))
      "RTS alloc rate"
      "RTS-reported allocation rate, MB/sec"

   <> fGrp  "NetIO, kB/s"               W5 KBs P0 Lin Free
    [ fGrp' "NetRd"        "recv"          (DWord64 (rNetRd.mpResourceCDFs))
      "Network reads"
      "Network reads, kB/sec"

    , fGrp' "NetWr"        "send"          (DWord64 (rNetWr.mpResourceCDFs))
      "Network writes"
      "Network writes, kB/sec"
    ]
   <> fGrp  "FS IO, kB/s"               W5 KBs P0 Lin Free
    [ fGrp' "FsRd"         "read"          (DWord64 (rFsRd.mpResourceCDFs))
      "Filesystem reads"
      "Number of bytes which this process really did cause to be fetched from the storage layer, per second"

    , fGrp' "FsWr"         "write"         (DWord64 (rFsWr.mpResourceCDFs))
      "Filesystem writes"
      "Number of bytes which this process caused to be sent to the storage layer, modulo truncate(), per second"
    ]

   <> fGrp  "CPU-85%span"               W5 Slo P0 Lin Free
    [ fGrp' "cdfSpanLensCpu"        "All" (DInt cdfSpanLensCpu)
      "CPU 85% spans"
      "Length of over-85% CPU usage peaks, slots"

    , fGrp' "cdfSpanLensCpuEpoch" "Epoch" (DInt cdfSpanLensCpuEpoch)
      "CPU spans at Ep boundary"
      "Length of over-85% CPU usage peaks, starting at epoch boundary, slots"
    ]

  fieldJSONOverlay f kvs =
    [ overlay kvs
    , alterSubObject overlay "mpResourceCDFs" kvs
    ]
   where overlay = tryOverlayFieldDescription f
         _dumpJSON _x = error $ "kvs:\n" <> (T.unpack . ST.toText . fromMaybe "" . ST.fromByteString . LBS.toStrict . AE.encode $ _x)

instance TimelineFields (SlotStats NominalDiffTime) where
  timelineFields =

      fW64' "slot"       "abs." "slot#" W5 Slo (IWord64 (unSlotNo      .slSlot))
   <> fW64' "epochSlot"   "ep."  "slot" W4 Slo (IWord64 (unEpochSlot   .slEpochSlot))
   <> fW64' "epoch"       "ep"      "#" W2 Epo (IWord64 (unEpochNo     .slEpoch))
   <> fW64' "safetyInt"   "saf"   "int" W3 Ix  (IWord64 (unEpochSafeInt.slEpochSafeInt))
   <> fGrp "block"                      W4 Blk P0 Lin Free
    [ fGrp' "block"                 "no."      (IWord64 (unBlockNo.slBlockNo)) "" ""
    , fGrp' "blockGap"              "gap"      (IWord64 slBlockGap) "" ""
    ]
   <> fGrp2                             W3 Ev  P0 Lin Free
    [ fGrp2' "forgeLoop"    "forg"  "loo"      (IWord64 slCountStarts) "" ""
    , fGrp2' "blockCtx"     "blk"   "ctx"      (IWord64 slCountBlkCtx) "" ""
    , fGrp2' "ledgerState"  "lgr"   "sta"      (IWord64 slCountLgrState) "" ""
    , fGrp2' "ledgerView"   "lgr"   "viw"      (IWord64 slCountLgrView) "" ""
    , fGrp2' "leadShips"    "ldr"   "win"      (IWord64 slCountLeads) "" ""
    , fGrp2' "forges"       "For"   "ge"       (IWord64 slCountForges) "" ""
    , fGrp2' "CDBSnap"      "CDB"   "snap"     (IWord64 slChainDBSnap) "" ""
    , fGrp2' "rejTxs"       "rej"   "txs"      (IWord64 slRejectedTx) "" ""
    ]
   <> fGrp2                             W5 Sec P3 Log Free
    [ fGrp2' "startDelay"   "loop"  "start"    (IDeltaTM slStarted) "" ""
    , fGrp2' "blkCtx"       "block" "ctx"      (IDeltaTM slBlkCtx) "" ""
    , fGrp2' "lgrState"     "ledgr" "state"    (IDeltaTM slLgrState) "" ""
    , fGrp2' "lgrView"      "ledgr" "view"     (IDeltaTM slLgrView) "" ""
    , fGrp2' "leadChecked"  "ledsh" "chekd"    (IDeltaTM slLeading) "" ""
    , fGrp2' "lgrTicked"    "ledgr" "tickd"    (IDeltaTM slTicked) "" ""
    , fGrp2' "mempoolSnap"  "mpool" "snapd"    (IDeltaTM slMemSnap) "" ""
    , fGrp2' "forge"        "forge" "done"     (IDeltaTM slForged) "" ""
    ]
   <> fGrpF  ",-mempool tx work-."      W4
    [ fGrpF' "SpanTxsMem"          "span" Sec P3 Log Free (IDeltaTM slSpanTxsMem) "" ""
    , fGrpF' "TxsCollected"        "cold" Uni P0 Lin Free (IWord64 slTxsCollected) "" ""
    , fGrpF' "TxsAccepted"         "accd" Uni P0 Lin Free (IWord64 slTxsAccepted) "" ""
    , fGrpF' "TxsRejected"         "rejd" Uni P0 Lin Free (IWord64 slTxsRejected) "" ""
    ]
   <> fBoth "chDensity"    "chain" "dens." W5 Rto P2 Lin Free (IFloat  slDensity) "" ""

   <> fGrp  "%CPU"                      W4 Pct P1 Lin (Z1 200)
    [ fGrp' "CPU%"                 "all"  (IWord64M (fm rCentiCpu.slResources)) "" ""
    , fGrp' "GC%"                  "GC"   (IWord64M (fm rCentiGC .slResources)) "" ""
    , fGrp' "MUT%"                 "mut"  (IWord64M (fm rCentiMut.slResources)) "" ""
    ]
   <> fGrp  "GCs"                       W4 Ev  P0 Lin Free
    [ fGrp' "majFlt"               "maj"  (IWord64M (fm rGcsMajor.slResources)) "" ""
    , fGrp' "minFlt"               "min"  (IWord64M (fm rGcsMinor.slResources)) "" ""   ]
   <> fGrp  "Memory use, MB"            W5 Pct P0 Lin (Z1 200)
    [ fGrp' "rssMB"                "RSS"  (IWord64M (fm rRSS  .slResources)) "" ""
    , fGrp' "heapMB"              "Heap"  (IWord64M (fm rHeap .slResources)) "" ""
    , fGrp' "liveMB"              "Live"  (IWord64M (fm rLive .slResources)) "" ""
    ]
   <> fBoth "allocatedMB" "Allocd" "MB"  W6 MBs P0 Lin Free (IWord64M (fm rAlloc.slResources)) "" ""
   <> fBoth "mempoolTxs" "Mempool" "txs" W7 Uni P0 Lin Free (IWord64 slMempoolTxs) "" ""
   <> fBoth "utxoSize"  "UTxO" "entries" W9 Uni P0 Lin Free (IWord64 slUtxoSize) "" ""
   where fm = fmap

  data TimelineComments (SlotStats NominalDiffTime)
    = SSLogObjects
    deriving Show

  rtCommentary SlotStats{..} =
    \case
      SSLogObjects -> ("           " <>) . LO.loPretty <$> reverse slLogObjects

-- * Instances, depending on the metrics' instances:
--
-- instance (ToJSON (f NominalDiffTime), ToJSON (f Int), ToJSON (f Double), ToJSON (f (Count BlockEvents)), ToJSON (f (DataDomain f SlotNo)), ToJSON (f (DataDomain BlockNo))) => ToJSON (BlockProp f) where
--   toJSON x = AE.genericToJSON AE.defaultOptions x
--              & \case
--                  Object o -> Object $ processFieldOverlays x o
--                  _ -> error "Heh, serialised BlockProp to a non-Object."

-- instance (ToJSON (a Double), ToJSON (a Int), ToJSON (a NominalDiffTime), ToJSON (a (DataDomain UTCTime)), ToJSON (a Word64), ToJSON (a (DataDomain SlotNo)), ToJSON (a (DataDomain BlockNo))) => ToJSON (MachPerf a) where
--   toJSON x = AE.genericToJSON AE.defaultOptions x
--              & \case
--                  Object o -> Object $ processFieldOverlays x o
--                  _ -> error "Heh, serialised BlockProp to a non-Object."

deriving newtype instance ToJSON MultiClusterPerf

-- * Field definition auxiliaries:
--
fScalar :: Text -> Width -> Unit -> s p a -> Text -> Text -> [Field s p a]
fScalar id w u sel sd d = [Field id "" "" w u P0 Lin Free sel sd d]

fBoth :: Text -> Text -> Text -> Width -> Unit -> Precision -> Scale -> Range -> s p a -> Text -> Text -> [Field s p a]
fBoth id h1 h2 wi u p s r sel sd d = [Field id h1 h2 wi u p s r sel sd d]

fBoth' :: Text -> Text -> Text -> Width -> Unit -> Precision -> Scale -> Range -> s p a -> [Field s p a]
fBoth' id h1 h2 wi u p s r sel = fBoth id h1 h2 wi u p s r sel "" ""

fW64 :: Text -> Text -> Text -> Width -> Unit -> s p a -> Text -> Text -> [Field s p a]
fW64 id h1 h2 wi u = fBoth id h1 h2 wi u P0 Lin Free

fW64' :: Text -> Text -> Text -> Width -> Unit -> s p a -> [Field s p a]
fW64' id h1 h2 wi u sel = fBoth id h1 h2 wi u P0 Lin Free sel "" ""

fGrp :: Text -> Width -> Unit -> Precision -> Scale -> Range -> [Unit -> Precision -> Scale -> Range -> Width -> Text -> [Field s p a]] -> [Field s p a]
fGrp hTop w u p s r fs = mconcat $
  zip fs (nChunksEachOf (length fs)
                        (unsafeUnWidth ("width of group " <> T.unpack hTop) w + 1)
                        hTop)
    <&> \(f, chunk) -> f u p s r w chunk

-- fUni :: Text -> Text         -> Width -> Unit -> Precision -> Scale -> Range -> s p a -> Text -> Text -> [Field s p a]
-- fUni id h1 wi u p s r sel sd d = [Field id h1 (renderUnit u) wi u p s r sel sd d]

fGrp' :: Text -> Text -> s p a -> Text -> Text -> Unit -> Precision -> Scale -> Range -> Width -> Text -> [Field s p a]
fGrp' id h2 sel sd d u p s r wi h1 = [Field id h1 h2 wi u p s r sel sd d]

fGrp2 :: Width -> Unit -> Precision -> Scale -> Range -> [Unit -> Precision -> Scale -> Range -> Width -> [Field s p a]] -> [Field s p a]
fGrp2 w u p s r fs = mconcat $
  fs <&> \f -> f u p s r w

fGrp2' :: Text -> Text -> Text -> s p a -> Text -> Text -> Unit -> Precision -> Scale -> Range -> Width -> [Field s p a]
fGrp2' id h1 h2 sel sd d u p s r wi = [Field id h1 h2 wi u p s r sel sd d]

fGrpF :: Text -> Width -> [Width -> Text -> [Field s p a]] -> [Field s p a]
fGrpF hTop w fs = mconcat $
  zip fs (nChunksEachOf (length fs)
                        (unsafeUnWidth ("width of group " <> T.unpack hTop) w + 1)
                        hTop)
    <&> \(f, chunk) -> f w chunk

fGrpF' :: Text -> Text -> Unit -> Precision -> Scale -> Range -> s p a -> Text -> Text -> Width -> Text -> [Field s p a]
fGrpF' id h2 u p s r sel sd d wi h1 = [Field id h1 h2 wi u p s r sel sd d]

fPct :: Text -> Text -> Range -> s p a -> Text -> Text -> [Field s p a]
fPct id h1 r sel sd d = [Field id h1 (renderUnit Pct) W3 Pct P0 Lin r sel sd d]
