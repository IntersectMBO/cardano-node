{-# LANGUAGE CPP #-}

module Cardano.Logging.Resources.Linux
    (
      readResourceStatsInternal
    ) where

import           Cardano.Logging.Resources.Types
import           Data.Maybe                      (fromMaybe)
import           Data.Word
import qualified GHC.Stats                       as GhcStats
import           System.Posix.Files              (fileMode, getFileStatus,
                                                  intersectFileModes,
                                                  ownerReadMode)
import           Text.Read                       (readMaybe)

-- | TODO we have to expand the |readMemStats| function
-- to read full data from |proc|
readResourceStatsInternal :: IO (Maybe ResourceStats)
readResourceStatsInternal = do
  rts <- GhcStats.getRTSStats
  mkProcStats rts . fmap fromIntegral <$> readProcList "/proc/self/stat"
 where
   mkProcStats :: GhcStats.RTSStats -> [Word64] -> Maybe ResourceStats
   mkProcStats rts
               (_:_:_:_:_:_:_:_:_:_            -- 00-09
               :_:_:_:user:sys:_:_:_:_:threads -- 10-19
               :_:_:_:rss:_:_:_:_:_:_          -- 20-29
               :_:_:_:_:_:_:_:_:_:_            -- 30-39
               :_:blkio:_rest) =               -- 40-42
     Just $ Resources
       { rCentiCpu   = user + sys
       , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
       , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
       , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
       , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
       , rAlloc      = GhcStats.allocated_bytes rts
       , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
       , rHeap       = GhcStats.gcdetails_mem_in_use_bytes $ GhcStats.gc rts
       , rRSS        = rss * 4096 -- TODO:  this is really PAGE_SIZE.
       , rCentiBlkIO = blkio
       , rThreads    = threads
       }
   mkProcStats _ _ = Nothing
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = floor . (/ (10000000 :: Double)) . fromIntegral

readProcList :: FilePath -> IO [Integer]
readProcList fp = do
    fs <- getFileStatus fp
    if readable fs
    then do
        cs <- readFile fp
        return $ map (\s -> fromMaybe 0 (readMaybe s :: Maybe Integer)) (words cs)
    else
        return []
  where
    readable fs = intersectFileModes (fileMode fs) ownerReadMode == ownerReadMode
