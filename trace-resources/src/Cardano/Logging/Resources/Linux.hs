{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Logging.Resources.Linux
    (
      readResourceStatsInternal
    ) where

import           Cardano.Logging.Resources.Types
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text.Read as T (decimal)
import           Data.Word
import qualified GHC.Stats as GhcStats
import           System.Posix.Files (fileMode, getFileStatus, intersectFileModes, ownerReadMode)

-- * Disk IO stats:
-- /proc/[pid]/io (since kernel 2.6.20)
--        This file contains I/O statistics for the process, for example:
--
--               # cat /proc/3828/io
--               rchar: 323934931
--               wchar: 323929600
--               syscr: 632687
--               syscw: 632675
--               read_bytes: 0
--               write_bytes: 323932160
--               cancelled_write_bytes: 0
--
--        The fields are as follows:
--
--        rchar: characters read
--               The number of bytes which this task has caused to be read from storage.  This is simply the  sum
--               of bytes which this process passed to read(2) and similar system calls.  It includes things such
--               as terminal I/O and is unaffected by whether or not actual physical disk I/O was  required  (the
--               read might have been satisfied from pagecache).
--
--        wchar: characters written
--               The  number  of bytes which this task has caused, or shall cause to be written to disk.  Similar
--               caveats apply here as with rchar.
--
--        syscr: read syscalls
--               Attempt to count the number of read I/O operations-that is, system calls  such  as  read(2)  and
--               pread(2).
--
--        syscw: write syscalls
--               Attempt  to  count the number of write I/O operations-that is, system calls such as write(2) and
--               pwrite(2).
--
--        read_bytes: bytes read
--               Attempt to count the number of bytes which this process really did cause to be fetched from  the
--               storage layer.  This is accurate for block-backed filesystems.
--
--        write_bytes: bytes written
--               Attempt to count the number of bytes which this process caused to be sent to the storage layer.
--
--        cancelled_write_bytes:
--               The  big  inaccuracy  here  is truncate.  If a process writes 1MB to a file and then deletes the
--               file, it will in fact perform no writeout.  But it will have been accounted as having caused 1MB
--               of  write.   In other words: this field represents the number of bytes which this process caused
--               to not happen, by truncating pagecache.  A task can cause "negative"  I/O  too.   If  this  task
--               truncates  some  dirty  pagecache,  some  I/O  which another task has been accounted for (in its
--               write\_bytes) will not be happening.
--
--        Note: In the current implementation, things are a bit racy  on  32-bit  systems:  if  process  A  reads
--        process  B's  /proc/[pid]/io  while process B is updating one of these 64-bit counters, process A could
--        see an intermediate result.
--
--        Permission to access this file is governed by a ptrace access mode PTRACE\_MODE\_READ\_FSCREDS check;  see
--        ptrace(2).
--
readProcBlockInOut :: IO (Word64, Word64)
readProcBlockInOut = do
    fields <- readProcList "/proc/self/io"
    case -- We're only interested in 'read_bytes' & 'write_bytes':
      fmap fromInteger . take 3 . drop 9 $ fields of
      [fsRd, _, fsWr] -> pure (fsRd, fsWr)
      _ -> pure (0, 0)

-- * Network stats:
--   grep IpExt /proc/<pid>/net/netstat
--   IpExt: InNoRoutes InTruncatedPkts InMcastPkts OutMcastPkts InBcastPkts OutBcastPkts InOctets OutOctets InMcastOctets OutMcastOctets InBcastOctets OutBcastOctets InCsumErrors InNoECTPkts InECT1Pkts InECT0Pkts InCEPkts
--   IpExt: 0 0 20053 8977 2437 23 3163525943 196480057 2426648 1491754 394285 5523 0 3513269 0 217426 0
--
readProcNetInOut :: IO (Word64, Word64)
#ifdef WITH_NETSTAT
readProcNetInOut = do
  fields <- T.words . fourthLine . T.lines <$> T.readFile "/proc/self/net/netstat"
  case -- We're only interested in 'InOctets' & 'OutOctets':
    fmap readMaybeText . take 2 . drop 7 $ fields of
      [Just netIn, Just netOut] -> pure (netIn, netOut)
      _ -> pure (0, 0)
  where
    -- Assumption: 'IpExt:' values are on the fourth line of how the kernel displays the buffer
    fourthLine ls = case drop 3 ls of
      l:_ -> l
      _   -> T.empty
#else
readProcNetInOut = pure (0, 0)
#endif

-- | TODO we have to expand the |readMemStats| function
-- to read full data from |proc|
readResourceStatsInternal :: IO (Maybe ResourceStats)
readResourceStatsInternal = do
  rts <- GhcStats.getRTSStats
  net <- readProcNetInOut
  fs  <- readProcBlockInOut
  mkProcStats rts net fs . fmap fromIntegral <$> readProcList "/proc/self/stat"
 where
   mkProcStats :: GhcStats.RTSStats -> (Word64, Word64) -> (Word64, Word64) -> [Word64] -> Maybe ResourceStats
   mkProcStats rts
               (rNetRd, rNetWr)
               (rFsRd,  rFsWr)
               (_:_:_:_:_:_:_:_:_:_             -- 00-09
               :_:_:_:user:sys:_:_:_:_:rThreads -- 10-19
               :_:_:_:rss:_:_:_:_:_:_           -- 20-29
               :_:_:_:_:_:_:_:_:_:_             -- 30-39
               :_:rCentiBlkIO:_rest) =          -- 40-42
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
       , ..
       }
   mkProcStats _ _ _ _ = Nothing
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = floor . (/ (10000000 :: Double)) . fromIntegral

readProcList :: FilePath -> IO [Integer]
readProcList fp = do
    fs <- getFileStatus fp
    if readable fs
    then do
        cs <- T.readFile fp
        return $ map (fromMaybe 0 . readMaybeText) (T.words cs)
    else
        return []
  where
    readable fs = intersectFileModes (fileMode fs) ownerReadMode == ownerReadMode

readMaybeText :: Integral a => T.Text -> Maybe a
readMaybeText t =
  case T.decimal t of
    Right (v, _)  -> Just v
    _             -> Nothing
