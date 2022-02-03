def format_specs:

{ config_specs:
    [{ key: "config",  header: "Config name"
     , path: ["config_name"]}
    ,{ key: "flags",   header: "RTS Flags"
     , path: ["data", "flags"]
     }
    ,{ key: "failed",  header: "Failed"
     , path: ["data", "failed"]
     # , post: not
     }]
, result_specs:
    [{ key: "Average-RSS",      header: "Average RSS",      unit: "MB"
     , path: ["RSS",  "avg"]
     , round: true
     }
    ,{ key: "Peak-RSS",         header: "Peak RSS",         unit: "MB"
     , path: ["RSS",  "max"]
     , round: true
     }
    ,{ key: "Average-RTS-heap", header: "Average RTS heap", unit: "MB"
     , path: ["Heap", "avg"]
     , round: true
     }
    ,{ key: "Peak-RTS-heap",    header: "Peak RTS heap",    unit: "MB"
     , path: ["Heap", "max"]
     , round: true
     }
    ,{ key: "Wall-clock-time",  header: "Wall clock time",  unit: "s"
     , path: ["totaltime"]
     , round: true
     }
    ,{ key: "OS-CPU-time",      header: "OS CPU time",      unit: "s"
     , path: ["CentiCpuMax"]
     , round: true, scale: 100
     }
    ,{ key: "RTS-mutator-time", header: "RTS mutator time", unit: "s"
     , path: ["CentiMutMax"]
     , round: true, scale: 100
     }
    ,{ key: "RTS-GC-time",      header: "RTS GC time",      unit: "s"
     , path: ["SecGC"]
     , round: true
     }
    ]
};
