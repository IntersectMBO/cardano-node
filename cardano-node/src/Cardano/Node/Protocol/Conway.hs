{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}

module Cardano.Node.Protocol.Conway
  ( readGenesis
  ) where

import           Prelude

-- import           Cardano.Binary (ToCBOR (..))
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Prelude
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

-- import qualified Cardano.Crypto.Hash as Crypto
-- import qualified Cardano.Crypto.Hash.Class as Crypto
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

-- TODO: Requires ToCBOR Conway.ConwayGenesis in ledger
-- conwayGenesisHash :: Conway.ConwayGenesis StandardCrypto -> GenesisHash
-- conwayGenesisHash g =
--   let h = Crypto.hashWithSerialiser @Crypto.Blake2b_256 toCBOR g
--   in GenesisHash $ read $ Crypto.hashToStringAsHex h
