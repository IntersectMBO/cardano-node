{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- UTxO scale / "-nomadperfssd" cluster.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Extra.Voting (
  profilesNoEraVoting
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
--import qualified Cardano.Benchmarking.Profile.Builtin.Empty as E
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V
import qualified Cardano.Benchmarking.Profile.Workload.Voting as W

--------------------------------------------------------------------------------

compressedFor3Epochs :: Types.Profile -> Types.Profile
compressedFor3Epochs =
    V.timescaleCompressed
  . P.generatorEpochs 3 . P.initCooldown 5
  . P.shutdownOnOff

--------------------------------------------------------------------------------

profilesNoEraVoting :: [Types.Profile]
profilesNoEraVoting =
  let base =
          P.empty &
          P.fixedLoaded
        . P.uniCircle . V.hosts 2 . P.loopback
        . V.genesisVariantVoltaire . P.voting . P.v10Preview
        . V.datasetMiniature
        . V.fundsVoting
        . compressedFor3Epochs
        . V.plutusDoublePlusSaturation . P.txFee 1000000
        . P.analysisStandard
        . V.clusterDefault -- TODO: "cluster" should be "null" here.
  in [
    base & P.name "development-voting"
         . P.dreps 1000
         . P.workloadAppend W.votingWorkloadx2
         . P.traceForwardingOn . P.newTracing . P.p2pOff
  ]
