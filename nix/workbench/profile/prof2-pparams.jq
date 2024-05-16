###
### Sampling the protocol parameters timeline at arbitrary epoch#.
###
import "epoch-timeline"    as timeline;
import "delta-blockbudget" as blockbudget;
import "delta-v8-preview"  as v8preview;

def filterMapPParams(flt; map):
    timeline::epochs
  | to_entries
  | sort_by(.value.epoch)
  | map(select(flt) | .value)
  | reduce .[] as $x ({}; . * $x)
  | map;

def epochPParams(upto):
  filterMapPParams(.value.epoch <= upto; .);

def epochPP(x):
  epochPParams(x);

###
### Combining timeline with arbitrary extensions:
###
def overlays:
  { "doublebudget": blockbudget::delta_doublebudget
  , "stepshalf":    blockbudget::delta_stepshalf
  , "v8-preview":   v8preview::delta
  };

def pParamsWithOverlays(epoch; overlay_names):
  if epoch == null then error("null passed for:  epoch") else
  filterMapPParams(.value.epoch <= epoch; .)
  *
  reduce overlay_names[] as $over
    ({};
     . * (overlays[$over] // error("Unregistered PParams overlay:  \($over)")))
  end;
