{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Cardano.Logging.Resources.Darwin
    ( readResourceStatsInternal
    ) where

#include "os-support-darwin.h"

import           Data.Word (Word64)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Error
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.Stats as GhcStats
import           System.Posix.Process (getProcessID)
import           System.Posix.Types (ProcessID)
import           Cardano.Logging.Resources.Types

{- type aliases -}
type MACH_VM_SIZE_T = Word64
data TIME_VALUE_T = TIME_VALUE_T Word64 Word64

{- memory information -}

{- mach/task_info.h
struct time_value {
	integer_t seconds;
	integer_t microseconds;
};
struct mach_task_basic_info {
	mach_vm_size_t  virtual_size;       /* virtual memory size (bytes) */
	mach_vm_size_t  resident_size;      /* resident memory size (bytes) */
	mach_vm_size_t  resident_size_max;  /* maximum resident memory size (bytes) */
	time_value_t    user_time;          /* total user run time for
	                                     *  terminated threads */
	time_value_t    system_time;        /* total system run time for
	                                     *  terminated threads */
	policy_t        policy;             /* default policy for new threads */
	integer_t       suspend_count;      /* suspend count for task */
}; -}

data MachTaskBasicInfo = MachTaskBasicInfo
  { _virtual_size :: !MACH_VM_SIZE_T
  , _resident_size :: !MACH_VM_SIZE_T
  , _resident_size_max :: !MACH_VM_SIZE_T
  , _user_time :: !TIME_VALUE_T
  , _system_time :: !TIME_VALUE_T
  , _policy :: !Word64
  , _suspend_count :: !Word64
  }

instance Storable TIME_VALUE_T where
  alignment _ = #const offsetof(struct {char x__; struct time_value (y__); }, y__)
  sizeOf _    = #size struct time_value
  peek ptr    = TIME_VALUE_T
                <$> (#peek struct time_value, seconds) ptr
                <*> (#peek struct time_value, microseconds) ptr
  poke _ _    = pure ()

instance Storable MachTaskBasicInfo where
  alignment _ = #const offsetof(struct {char x__; struct mach_task_basic_info (y__); }, y__)
  sizeOf _    = #size struct mach_task_basic_info
  peek ptr    = MachTaskBasicInfo
                <$> (#peek struct mach_task_basic_info, virtual_size) ptr
                <*> (#peek struct mach_task_basic_info, resident_size) ptr
                <*> (#peek struct mach_task_basic_info, resident_size_max) ptr
                <*> (#peek struct mach_task_basic_info, user_time) ptr
                <*> (#peek struct mach_task_basic_info, system_time) ptr
                <*> (#peek struct mach_task_basic_info, policy) ptr
                <*> (#peek struct mach_task_basic_info, suspend_count) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_process_memory_info2 :: Ptr MachTaskBasicInfo -> CInt -> IO CInt


getMemoryInfo :: ProcessID -> IO MachTaskBasicInfo
getMemoryInfo pid =
    allocaBytes 128 $ \ptr -> do
      throwIfNeg_ (\res -> "c_get_process_memory_info2: failure returned: " ++ show (pred res))
                    (succ <$> c_get_process_memory_info2 ptr (fromIntegral pid))
      peek ptr

readResourceStatsInternal :: IO (Maybe ResourceStats)
readResourceStatsInternal = getProcessID >>= \pid -> do
  cpu <- getMemoryInfo pid
  rts <- GhcStats.getRTSStats
  mem <- getMemoryInfo pid
  pure . Just $
    Resources
    { rCentiCpu   = timeValToCenti (_user_time cpu)
                  + timeValToCenti (_system_time cpu)
    , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
    , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
    , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
    , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
    , rAlloc      = GhcStats.allocated_bytes rts
    , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
    , rHeap       = GhcStats.gcdetails_mem_in_use_bytes $ GhcStats.gc rts
    , rRSS        = _resident_size mem
    , rCentiBlkIO = 0
    , rThreads    = 0
    }
 where
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = fromIntegral . (`div` 10000000)
   timeValToCenti :: TIME_VALUE_T -> Word64
   timeValToCenti tv = 10000 `div` (usFromTimeValue tv)

usFromTimeValue :: TIME_VALUE_T -> Word64
usFromTimeValue (TIME_VALUE_T s us) = s * 1000000 + us
