{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Unlog.SlotStats (module Cardano.Unlog.SlotStats) where

import qualified Prelude as P
import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import           Data.List.Split (splitOn)

import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Text.Printf

import           Ouroboros.Network.Block (SlotNo(..))

import           Data.Accum
import           Cardano.Unlog.Resources


-- type Text = ShortText

data SlotStats
  = SlotStats
    { slSlot         :: !SlotNo
    , slEpoch        :: !Word64
    , slEpochSlot    :: !Word64
    , slStart        :: !UTCTime
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

instance ToJSON SlotStats

slotHeadE, slotFormatE :: Text
slotHeadP, slotFormatP :: Text
slotHeadP =
  "abs.  slot    block block lead  leader CDB rej  check  lead    mempool tx       chain      %CPU      GCs   Produc-    Memory use, kB      Alloc rate  Mempool  UTxO  Absolute" <>"\n"<>
  "slot#   epoch  no. -less checks ships snap txs  span   span  span col acc rej  density all/ GC/mut maj/min tivity   RSS  Heap  Live Alloc /mut sec,kB  txs   entries  slot time"
slotHeadE =
  "abs.slot#,slot,epoch,block,blockless,checkSpan,leadSpan,leadShips,cdbSnap,rejTx,checkSpan,mempoolTxSpan,chainDens,%CPU,%GC,%MUT,Productiv,MemLiveKb,MemAllocKb,MemRSSKb,AllocRate/Mut,MempoolTxs,UTxO"
slotFormatP = "%5d %4d:%2d %4d    %2d    %2d   %2d    %2d  %2d %7s %5s %5s  %2d  %2d  %2d   %0.3f  %3s %3s %3s  %2s %4s %5s %5s %5s %5s %4s  %8s   %4d %9d %s"
slotFormatE = "%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%s,%s,%d,%d%0.3f,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%d,%d,%s"

slotLine :: Bool -> Text -> SlotStats -> Text
slotLine exportMode leadershipF SlotStats{..} = Text.pack $
  printf (Text.unpack leadershipF)
         sl epsl epo blk blkl chks  lds cdbsn rejtx spanC spanL subdt scol sacc srej dens cpu gc mut majg ming   pro rss hea liv alc atm mpo utx start
 where sl    = unSlotNo slSlot
       epsl  = slEpochSlot
       epo   = slEpoch
       blk   = slBlockNo
       blkl  = slBlockless
       chks  = slCountChecks
       lds   = slCountLeads
       cdbsn = slChainDBSnap
       rejtx = slRejectedTx
       subdt = maybe "" (Text.init . show) slTxsMemSpan
       scol  = slTxsCollected
       sacc  = slTxsAccepted
       srej  = slTxsRejected
       spanC = Text.init $ show slSpanCheck
       spanL = Text.init $ show slSpanLead
       cpu   = d 3 $ rCentiCpu slResources
       dens  = slDensity
       gc    = d 2 $ min 999 -- workaround for ghc-8.10.x
                  <$> rCentiGC  slResources
       mut   = d 2 $ min 999 -- workaround for ghc-8.10.x
                  <$> rCentiMut slResources
       majg  = d 2 $ rGcsMajor slResources
       ming  = d 2 $ rGcsMinor slResources
       pro   = f 2 $ calcProd <$> (min 6 . -- workaround for ghc-8.10.2
                                   fromIntegral <$> rCentiMut slResources :: Maybe Float)
                              <*> (fromIntegral <$> rCentiCpu slResources)
       rss   = d 5 (rRSS      slResources)
       hea   = d 5 (rHeap     slResources)
       liv   = d 5 (rLive     slResources)
       alc   = d 5 (rAlloc    slResources)
       atm   = d 8 $
               (ceiling :: Float -> Int)
               <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
                        <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources))
       mpo   = slMempoolTxs
       utx   = slUtxoSize
       start = " " `splitOn` show slStart P.!! 1

       calcProd :: Float -> Float -> Float
       calcProd mut' cpu' = if cpu' == 0 then 1 else mut' / cpu'

       d, f :: PrintfArg a => Int -> Maybe a -> Text
       d width = \case
         Just x  -> Text.pack $ printf ("%"<>(if exportMode then "0" else "")
                                           <>show width<>"d") x
         Nothing -> mconcat (replicate width "-")
       f width = \case
         Just x  -> Text.pack $ printf ("%0."<>show width<>"f") x
         Nothing -> mconcat (replicate width "-")

renderSlotTimeline :: Text -> Text -> Bool -> Seq SlotStats -> Handle -> IO ()
renderSlotTimeline leadHead fmt exportMode slotStats hnd = do
  forM_ (zip (toList slotStats) [(0 :: Int)..]) $ \(l, i) -> do
    when (i `mod` 33 == 0 && (i == 0 || not exportMode)) $
      hPutStrLn hnd leadHead
    hPutStrLn hnd $ slotLine exportMode fmt l

-- | Initial and trailing data are noisy outliers: drop that.
--
--   The initial part is useless until the node actually starts
--   to interact with the blockchain, so we drop all slots until
--   they start getting non-zero chain density reported.
--
--   On the trailing part, we drop everything since the last leadership check.
cleanupSlotStats :: Seq SlotStats -> Seq SlotStats
cleanupSlotStats =
  -- Seq.dropWhileL ((== 0) . slDensity) .
  Seq.dropWhileL ((/= 500) . slSlot) .
  Seq.dropWhileR ((== 0)   . slCountChecks)

zeroSlotStats :: SlotStats
zeroSlotStats =
  SlotStats
  { slSlot = 0
  , slEpoch = 0
  , slEpochSlot = 0
  , slStart = zeroUTCTime
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

