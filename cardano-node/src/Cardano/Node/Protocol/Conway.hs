{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Conway
  ( readGenesis
  ) where


import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Prelude
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Cardano.Ledger.Conway.Genesis as Conway
import           Cardano.Ledger.Crypto (StandardCrypto)

--
-- Conway genesis
--

readGenesis
  :: GenesisFile
  -> Maybe GenesisHash
  -> ExceptT GenesisReadError IO (Conway.ConwayGenesis StandardCrypto, GenesisHash)
readGenesis = readGenesisAny

