
https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag--H%20[%E2%9F%A8size%E2%9F%A9]
-H [⟨size⟩]
Default:	0
This option provides a “suggested heap size” for the garbage collector. Think of
-Hsize as a variable -A ⟨size⟩ option. It says: I want to use at least ⟨size⟩
bytes, so use whatever is left over to increase the -A value.

This option does not put a limit on the heap size: the heap may grow beyond the
given size as usual.

If ⟨size⟩ is omitted, then the garbage collector will take the size of the heap
at the previous GC as the ⟨size⟩. This has the effect of allowing for a larger
-A value but without increasing the overall memory requirements of the program.
It can be useful when the default small -A value is suboptimal, as it can be in
programs that create large amounts of long-lived data.

https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag--K%20%E2%9F%A8size%E2%9F%A9
-K ⟨size⟩
Default:	80% of physical memory
Set the maximum stack size for an individual thread to ⟨size⟩ bytes. If the
thread attempts to exceed this limit, it will be sent the StackOverflow
exception. The limit can be disabled entirely by specifying a size of zero.

This option is there mainly to stop the program eating up all the available
memory in the machine if it gets into an infinite loop.

https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag--M%20%E2%9F%A8size%E2%9F%A9
-M ⟨size⟩
Default:	unlimited
Set the maximum heap size to ⟨size⟩ bytes. The heap normally grows and
shrinks according to the memory requirements of the program. The only reason
for having this option is to stop the heap growing without bound and filling up
all the available swap space, which at the least will result in the program
being summarily killed by the operating system.

The maximum heap size also affects other garbage collection parameters: when the
amount of live data in the heap exceeds a certain fraction of the maximum heap
size, compacting collection will be automatically enabled for the oldest
generation, and the -F parameter will be reduced in order to avoid exceeding the
maximum heap size.

https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag--Mgrace=%E2%9F%A8size%E2%9F%A9
-Mgrace=⟨size⟩
Default:	1M
If the program’s heap exceeds the value set by -M ⟨size⟩, the RTS throws an
exception to the program, and the program gets an additional quota of allocation
before the exception is raised again, the idea being so that the program can
execute its exception handlers. -Mgrace= controls the size of this additional
quota.

RTS (master 2024-04-25)

https://github.com/ghc/ghc/blob/daeda83478d5b800d29661408dd67cc4b23df374/compiler/cbits/cutils.c#L17
```
void
setHeapSize( HsInt size )
{
    RtsFlags.GcFlags.heapSizeSuggestion = size / BLOCK_SIZE;
    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
        RtsFlags.GcFlags.heapSizeSuggestion > RtsFlags.GcFlags.maxHeapSize) {
        RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
    }
}
```

https://github.com/ghc/ghc/blob/daeda83478d5b800d29661408dd67cc4b23df374/rts/sm/GC.c#L1075
```
      /* Also, if user set heap size, do not drop below it.
       */
      need = stg_max(RtsFlags.GcFlags.heapSizeSuggestion, need);

      /* But with a large nursery, the above estimate might exceed
       * maxHeapSize.  A large resident set size might make the OS
       * kill this process, or swap unnecessarily.  Therefore we
       * ensure that our estimate does not exceed maxHeapSize.
       */
      if (RtsFlags.GcFlags.maxHeapSize != 0) {
          need = stg_min(RtsFlags.GcFlags.maxHeapSize, need);
      }
```

https://github.com/ghc/ghc/blob/94da936507c685aa8101a714e7619b4d428d0187/rts/RtsFlags.c#L139C1-L147C46
```
void initRtsFlagsDefaults(void)
{
    StgWord64 maxStkSize = 8 * getPhysicalMemorySize() / 10;
    // if getPhysicalMemorySize fails just move along with an 8MB limit
    if (maxStkSize == 0)
        maxStkSize = 8 * 1024 * 1024;
    // GcFlags.maxStkSiz is 32-bit, so we need to cap to prevent overflow (#17019)
    else if (maxStkSize > UINT32_MAX * sizeof(W_))
        maxStkSize = UINT32_MAX * sizeof(W_);
```

https://github.com/ghc/ghc/blob/94da936507c685aa8101a714e7619b4d428d0187/rts/posix/OSMem.c#L423
```
/* Returns 0 if physical memory size cannot be identified */
StgWord64 getPhysicalMemorySize (void)
{
    static StgWord64 physMemSize = 0;
    if (!physMemSize) {
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
        /* So, darwin doesn't support _SC_PHYS_PAGES, but it does
           support getting the raw memory size in bytes through
           sysctlbyname(hw.memsize); */
        size_t len = sizeof(physMemSize);
        int ret = -1;

        /* Note hw.memsize is in bytes, so no need to multiply by page size. */
        ret = sysctlbyname("hw.memsize", &physMemSize, &len, NULL, 0);
        if (ret == -1) {
            physMemSize = 0;
            return 0;
        }
#else
        /* We'll politely assume we have a system supporting _SC_PHYS_PAGES
         * otherwise.  */
        W_ pageSize = getPageSize();
        long ret = sysconf(_SC_PHYS_PAGES);
        if (ret == -1) {
#if defined(DEBUG)
            errorBelch("warning: getPhysicalMemorySize: cannot get "
                       "physical memory size");
#endif
            return 0;
        }
        physMemSize = ret * pageSize;
#endif /* darwin_HOST_OS */
    }
    return physMemSize;
}
```

