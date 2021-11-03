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
    [{ key: "RssAvg",  header: "Avg RSS, MB"
     , path: ["RSS",  "avg"], round: true
     }
    ,{ key: "RssMax",  header: "Max RSS, MB"
     , path: ["RSS",  "max"], round: true
     }
    ,{ key: "HeapAvg", header: "Avg heap, MB"
     , path: ["Heap", "avg"], round: true
     }
    ,{ key: "HeapMax", header: "Max heap, MB"
     , path: ["Heap", "max"], round: true
     }
    ,{ key: "WallSec", header: "Wall, s"
     , path: ["totaltime"], round: true
     }
    ,{ key: "CpuMax",  header: "OS CPU, s"
     , path: ["CentiCpuMax"], scale: 100, round: true
     }
    ,{ key: "MutMax",  header: "Mutator, s"
     , path: ["CentiMutMax"], scale: 100, round: true
     }
    ,{ key: "GCSec",   header: "GC time, s"
     , path: ["SecGC"], round: true
     }
    ]
};
