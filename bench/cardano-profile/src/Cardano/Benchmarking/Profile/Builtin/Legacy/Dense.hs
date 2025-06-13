{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- Profiles using `pools`.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Legacy.Dense (
  profilesNoEraDense
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Empty as E
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

profilesNoEraDense :: [Types.Profile]
profilesNoEraDense =
  let ciTestDense =
          P.empty & E.base
        -- Biggest difference is using `P.pools` instead of `P.hosts`.
        . P.uniCircle . P.pools 10 . P.loopback
        . V.genesisVariant300
        . E.ciTestDuration
        . P.cBlockMinimumAdoptions 9
  in [
    ciTestDense & P.name "ci-test-dense10" . V.valueLocal . P.traceForwardingOn . P.newTracing . P.p2pOff
  ]
