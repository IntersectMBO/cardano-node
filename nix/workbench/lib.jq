def color:
  { red:     "\u001b[31m"
  , green:   "\u001b[32m"
  , yellow:  "\u001b[33m"
  , blue:    "\u001b[34m"
  , magenta: "\u001b[35m"
  , cyan:    "\u001b[36m"
  , white:   "\u001b[37m"
  , off:     "\u001b[39m"
  };

def colorly($col; $x):
  "\(color[$col])\($x)\(color["off"])";

def drop_nulls:
  map(select(. != null));
