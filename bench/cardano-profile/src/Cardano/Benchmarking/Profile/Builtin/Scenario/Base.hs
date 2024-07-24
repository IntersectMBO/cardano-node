{-# LANGUAGE Trustworthy #-}

-- Common for all non-`FixedLoaded` scenarios.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Scenario.Base (
  base
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

base :: Types.Profile -> Types.Profile
base =
   -- TODO: "tracer-only" and "idle" have `P.delegators 6`. It's not needed!
   --       Remove them, make delegators zero by using `V.datasetEmpty` here.
   --       It was kept to make test pass because "prof0-defaults.jq" set it,
   --       `n_pools` used as "effective_delegators" when delegators zero/null.
    P.utxo 0 . P.dreps 0
  . V.fundsDefault
  . V.valueCloud -- TODO: Why TPS=12 ? make for all these generator=null!
  -- TODO: dummy "generator.epochs" ignored in favor of "--shutdown-on".
  --       Create a "time.epochs" or "time.blocks" or similar, IDK!
  -- This applies to all profiles!
  . P.generatorEpochs 3 . P.initCooldown 5
  . P.shutdownOnOff
