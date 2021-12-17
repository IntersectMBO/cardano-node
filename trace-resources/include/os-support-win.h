
typedef struct _CPU_TIMES {
  ULONGLONG usertime;
  ULONGLONG systime;
  ULONGLONG idletime;
} CPU_TIMES;

int c_get_process_memory_info2 (PROCESS_MEMORY_COUNTERS *counters, DWORD pid);
int c_get_io_counters2 (IO_COUNTERS *counters, DWORD pid);
int c_get_sys_cpu_times2 (CPU_TIMES *cputimes);
int c_get_proc_cpu_times2 (CPU_TIMES *cputimes, DWORD pid);
int c_get_system_info2 (SYSTEM_INFO *sysinfo);
int c_get_win_bits2 (DWORD pid);

