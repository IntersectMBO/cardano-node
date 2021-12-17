
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Logging.Resources.Windows
    ( readRessoureStatsInternal
    ) where


import           Data.Word (Word64)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Error
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.Stats as GhcStats
import           System.Win32.Process (ProcessId, getCurrentProcessId)
import           System.Win32.Types

import           Cardano.Logging.Resources.Types


-- use PsAPI version 2
#define PSAPI_VERSION 2

#include <windows.h>
#include <psapi.h>

#include "os-support-win.h"


{- type aliases -}
type ULONGLONG = Word64

{- memory information -}

{- https://docs.microsoft.com/de-de/windows/win32/api/psapi/ns-psapi-process_memory_counters
typedef struct _PROCESS_MEMORY_COUNTERS {
  DWORD  cb;
  DWORD  PageFaultCount;
  SIZE_T PeakWorkingSetSize;
  SIZE_T WorkingSetSize;
  SIZE_T QuotaPeakPagedPoolUsage;
  SIZE_T QuotaPagedPoolUsage;
  SIZE_T QuotaPeakNonPagedPoolUsage;
  SIZE_T QuotaNonPagedPoolUsage;
  SIZE_T PagefileUsage;
  SIZE_T PeakPagefileUsage;
} PROCESS_MEMORY_COUNTERS; -}

data ProcessMemoryCounters = ProcessMemoryCounters
  { _cb :: DWORD
  , _pageFaultCount :: DWORD
  , _peakWorkingSetSize :: SIZE_T
  , _workingSetSize :: SIZE_T
  , _quotaPeakPagedPoolUsage :: SIZE_T
  , _quotaPagedPoolUsage :: SIZE_T
  , _quotaPeakNonPagedPoolUsage :: SIZE_T
  , _quotaNonPagedPoolUsage :: SIZE_T
  , _pagefileUsage :: SIZE_T
  , _peakPagefileUsage :: SIZE_T
  }

instance Storable ProcessMemoryCounters where
  alignment _ = #const offsetof(struct {char x__; PROCESS_MEMORY_COUNTERS (y__); }, y__)
  sizeOf _    = #size PROCESS_MEMORY_COUNTERS
  peek ptr    = ProcessMemoryCounters
                <$> (#peek PROCESS_MEMORY_COUNTERS, cb) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PageFaultCount) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PeakWorkingSetSize) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, WorkingSetSize) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaPeakPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaPeakNonPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaNonPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PagefileUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PeakPagefileUsage) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_process_memory_info2 :: Ptr ProcessMemoryCounters -> CInt -> IO CInt

{- I/O counters -}
{- https://docs.microsoft.com/de-de/windows/win32/api/winnt/ns-winnt-io_counters
typedef struct _IO_COUNTERS {
  ULONGLONG ReadOperationCount;
  ULONGLONG WriteOperationCount;
  ULONGLONG OtherOperationCount;
  ULONGLONG ReadTransferCount;
  ULONGLONG WriteTransferCount;
  ULONGLONG OtherTransferCount;
} IO_COUNTERS; -}

data IOCounters = IOCounters
  { _readOperationCount :: ULONGLONG
  , _writeOperationCount :: ULONGLONG
  , _otherOperationCount :: ULONGLONG
  , _readTransferCount :: ULONGLONG
  , _writeTransferCount :: ULONGLONG
  , _otherTransferCount :: ULONGLONG
  }

instance Storable IOCounters where
  alignment _ = #const offsetof(struct {char x__; IO_COUNTERS (y__); }, y__)
  sizeOf _    = #size IO_COUNTERS
  peek ptr    = IOCounters
                <$> (#peek IO_COUNTERS, ReadOperationCount) ptr
                <*> (#peek IO_COUNTERS, WriteOperationCount) ptr
                <*> (#peek IO_COUNTERS, OtherOperationCount) ptr
                <*> (#peek IO_COUNTERS, ReadTransferCount) ptr
                <*> (#peek IO_COUNTERS, WriteTransferCount) ptr
                <*> (#peek IO_COUNTERS, OtherTransferCount) ptr
  poke _ _    = pure ()

data CpuTimes = CpuTimes {
    usertime :: ULONGLONG
  , systime :: ULONGLONG
  , _idletime :: ULONGLONG
  }

instance Storable CpuTimes where
  alignment _ = #const offsetof(struct {char x__; CPU_TIMES (y__); }, y__)
  sizeOf _    = #size CPU_TIMES
  peek ptr    = CpuTimes
                <$> (#peek CPU_TIMES, usertime) ptr
                <*> (#peek CPU_TIMES, systime) ptr
                <*> (#peek CPU_TIMES, idletime) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_proc_cpu_times2 :: Ptr CpuTimes -> CInt -> IO CInt


getMemoryInfo :: ProcessId -> IO ProcessMemoryCounters
getMemoryInfo pid =
    allocaBytes 128 $ \ptr -> do
      throwIfNeg_ (\res -> "c_get_process_memory_info2: failure returned: " ++ show res)
                    (c_get_process_memory_info2 ptr (fromIntegral pid))
      peek ptr

readRessoureStatsInternal :: IO (Maybe ResourceStats)
readRessoureStatsInternal = getCurrentProcessId >>= \pid -> do
  cpu <- getCpuTimes   pid
  mem <- getMemoryInfo pid
  rts <- GhcStats.getRTSStats
  pure . Just $
    Resources
    { rCentiCpu   = usecsToCenti $ usertime cpu + systime cpu
    , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
    , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
    , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
    , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
    , rAlloc      = GhcStats.allocated_bytes rts
    , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
    , rHeap       = GhcStats.gcdetails_mem_in_use_bytes $ GhcStats.gc rts
    , rRSS        = fromIntegral (_workingSetSize mem)
    , rCentiBlkIO = 0
    , rThreads    = 0
    }
 where
   usecsToCenti :: ULONGLONG -> Word64
   usecsToCenti ul = ul `div` 10000
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = fromIntegral . (`div` 10000000)


getCpuTimes :: ProcessId -> IO CpuTimes
getCpuTimes pid =
  allocaBytes 128 $ \ptr -> do
    res <- c_get_proc_cpu_times2 ptr (fromIntegral pid)
    if res <= 0
      then do
        putStrLn $ "c_get_proc_cpu_times2: failure returned: " ++ (show res)
        return $ CpuTimes 0 0 0
      else
        peek ptr
