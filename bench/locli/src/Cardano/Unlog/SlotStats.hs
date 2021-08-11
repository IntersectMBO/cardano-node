{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use head" -}
module Cardano.Unlog.SlotStats (module Cardano.Unlog.SlotStats) where

import           Prelude ((!!))
import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Text as T
import           Data.List (dropWhileEnd)
import           Data.List.Split (splitOn)

import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Text.Printf

import           Ouroboros.Network.Block (SlotNo(..))

import           Data.Accum
import           Cardano.Analysis.Profile
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources


-- type Text = ShortText

data SlotStats
  = SlotStats
    { slSlot         :: !SlotNo
    , slEpoch        :: !Word64
    , slEpochSlot    :: !Word64
    , slStart        :: !SlotStart
    , slCountChecks  :: !Word64
    , slCountLeads   :: !Word64
    , slChainDBSnap  :: !Word64
    , slRejectedTx   :: !Word64
    , slBlockNo      :: !Word64
    , slBlockless    :: !Word64
    , slOrderViol    :: !Word64
    , slEarliest     :: !UTCTime
    , slSpanCheck    :: !NominalDiffTime
    , slSpanLead     :: !NominalDiffTime
    , slMempoolTxs   :: !Word64
    , slTxsMemSpan   :: !(Maybe NominalDiffTime)
    , slTxsCollected :: !Word64
    , slTxsAccepted  :: !Word64
    , slTxsRejected  :: !Word64
    , slUtxoSize     :: !Word64
    , slDensity      :: !Float
    , slResources    :: !(Resources (Maybe Word64))
    }
  deriving (Generic, Show)

instance RenderTimeline SlotStats where
  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "abs.slot"     "abs."  "slot#"   $ IWord64 (unSlotNo . slSlot)
    , Field 4 0 "slot"         "  epo" "slot"    $ IWord64 slEpochSlot
    , Field 2 0 "epoch"        "ch "   "#"       $ IWord64 slEpoch
    , Field 5 0 "block"        "block" "no."     $ IWord64 slBlockNo
    , Field 5 0 "blockGap"     "block" "gap"     $ IWord64 slBlockless
    , Field 3 0 "leadChecks"   "lead"  "chk"     $ IWord64 slCountChecks
    , Field 3 0 "leadShips"    "ship"  "win"     $ IWord64 slCountLeads
    , Field 4 0 "CDBSnap"      "CDB"   "snap"    $ IWord64 slChainDBSnap
    , Field 3 0 "rejTxs"       "rej"   "txs"     $ IWord64 slRejectedTx
    , Field 7 0 "checkSpan"    "check" "span"    $ IDeltaT slSpanCheck
    , Field 5 0 "leadSpan"     "lead"  "span"    $ IDeltaT slSpanLead
    , Field 4 0 "mempoolTxSpan" (t 4!!0) "span"  $ IText (maybe "" show.slTxsMemSpan)
    , Field 4 0 "txsColl"     (t 4!!1) "cold"    $ IWord64 slTxsCollected
    , Field 4 0 "txsAcc"      (t 4!!2) "accd"    $ IWord64 slTxsAccepted
    , Field 4 0 "txsRej"      (t 4!!3) "rejd"    $ IWord64 slTxsRejected
    , Field 5 1 "chDensity"    "chain" "dens."   $ IFloat  slDensity
    , Field 3 0 "CPU%"        (c 3!!0) "all"     $ IText (d 3.rCentiCpu.slResources)
    , Field 3 0 "GC%"         (c 3!!1) "GC"      $ IText (d 3.fmap (min 999).rCentiGC.slResources)
    , Field 3 0 "MUT%"        (c 3!!2) "mut"     $ IText (d 3.fmap (min 999).rCentiMut.slResources)
    , Field 3 0 "majFlt"      (g 3!!0) "maj"     $ IText (d 3.rGcsMajor.slResources)
    , Field 3 0 "minFlt"      (g 3!!1) "min"     $ IText (d 3.rGcsMinor.slResources)
    , Field 6 0 "productiv"   "Produc" "tivity"  $ IText
      (\SlotStats{..}->
          f 4 $ calcProd <$> (min 6 . -- workaround for ghc-8.10.2
                              fromIntegral <$> rCentiMut slResources :: Maybe Float)
          <*> (fromIntegral <$> rCentiCpu slResources))
    , Field 5 0 "rssMB"       (m 5!!0) "RSS"     $ IText (d 5.rRSS  .slResources)
    , Field 5 0 "heapMB"      (m 5!!1) "Heap"    $ IText (d 5.rHeap .slResources)
    , Field 5 0 "liveMB"      (m 5!!2) "Live"    $ IText (d 5.rLive .slResources)
    , Field 5 0 "allocatedMB"  "Allocd" "MB"     $ IText (d 5.rAlloc.slResources)
    , Field 6 0 "allocMut"     "Alloc/" "mutSec" $ IText
      (\SlotStats{..}->
          d 5 $
          (ceiling :: Float -> Int)
          <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
                <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources)))
    , Field 7 0 "mempoolTxs"   "Mempool" "txs"   $ IWord64 slMempoolTxs
    , Field 9 0 "utxoEntries"  "UTxO"  "entries" $ IWord64 slUtxoSize
    , Field 10 0 "absSlotTime" "Absolute" "slot time" $ IText
      (\SlotStats{..}->
         T.pack $ " " `splitOn` show slStart !! 1)
    ]
   where
     t w = nChunksEachOf 4 (w + 1) "mempool tx"
     c w = nChunksEachOf 3 (w + 1) "%CPU"
     g w = nChunksEachOf 2 (w + 1) "GCs"
     m w = nChunksEachOf 3 (w + 1) "Memory use, MB"

     d, f :: PrintfArg a => Int -> Maybe a -> Text
     d width = \case
       Just x  -> T.pack $ printf ("%"<>"" --(if exportMode then "0" else "")
                                      <>show width<>"d") x
       Nothing -> mconcat (replicate width "-")
     f width = \case
       Just x  -> T.pack $ printf ("%0."<>show width<>"f") x
       Nothing -> mconcat (replicate width "-")

     calcProd :: Float -> Float -> Float
     calcProd mut' cpu' = if cpu' == 0 then 1 else mut' / cpu'

instance ToJSON SlotStats

-- | Initial and trailing data are noisy outliers: drop that.
--
--   The initial part is useless until the node actually starts
--   to interact with the blockchain, so we drop all slots until
--   they start getting non-zero chain density reported.
--
--   On the trailing part, we drop everything since the last leadership check.
cleanupSlotStats :: [SlotStats] -> [SlotStats]
cleanupSlotStats =
  -- dropWhile ((== 0) . slDensity) .
  dropWhile    ((/= 500) . slSlot) .
  dropWhileEnd ((== 0)   . slCountChecks)

zeroSlotStats :: SlotStats
zeroSlotStats =
  SlotStats
  { slSlot = 0
  , slEpoch = 0
  , slEpochSlot = 0
  , slStart = SlotStart zeroUTCTime
  , slCountChecks = 0
  , slCountLeads = 0
  , slOrderViol = 0
  , slEarliest = zeroUTCTime
  , slSpanCheck = realToFrac (0 :: Int)
  , slSpanLead = realToFrac (0 :: Int)
  , slMempoolTxs = 0
  , slTxsMemSpan = Nothing
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

