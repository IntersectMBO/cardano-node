{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Protocol.Conway
  ( ConwayProtocolInstantiationError(..)
    -- * Reusable parts
  , readGenesis
  , readGenesisMaybe
  , validateGenesis
  ) where

import           Cardano.Api

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Binary as L
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import qualified Cardano.Ledger.Conway.Genesis as Conway
import           Cardano.Ledger.Conway.PParams
import qualified Cardano.Ledger.Plutus.CostModels as L
import qualified Cardano.Ledger.Plutus.Language as L
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Default.Class as DefaultClass
import           Data.Functor.Identity (Identity (..))
import           Data.Int
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word

--
-- Conway genesis
--

-- The Conway genesis values are not going to be ready in time for
-- cardano-node 9.0. Therefore when the conway genesis file is not set
-- we will restrict the max protocol version to 8 and use a dummy conway
-- genesis value. This will prevent an unintentional hardfork.
readGenesisMaybe :: Maybe GenesisFile
                 -> Maybe GenesisHash
                 -> ExceptT GenesisReadError IO
                            (Conway.ConwayGenesis StandardCrypto, GenesisHash)
readGenesisMaybe (Just genFp) mHash = readGenesis genFp mHash
readGenesisMaybe Nothing _ = do
  case L.mkCostModelsLenient plutusV3CostModel >>= Map.lookup L.PlutusV3 . L.costModelsValid of
    Nothing -> error "readGenesisMaybe: missing PlutusV3 cost model in default cost models."
    Just cm -> do
      let protocolVerson = L.natVersion @8 -- For empty conway genesis we stay in Babbage (protocol version 8)
          conwayGenesis = emptyConwayGenesis cm
          genesisHash = GenesisHash (Crypto.hashWith id $ LB.toStrict $ L.serialize protocolVerson conwayGenesis)
      return (conwayGenesis, genesisHash)

plutusV3CostModel :: Map Word8 [Int64]
plutusV3CostModel = Map.singleton 2 plutusV3ExampleValues

plutusV3ExampleValues :: [Int64]
plutusV3ExampleValues =
  [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475
  , 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100
  , 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32
  , 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000
  , 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436
  , 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145
  , 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558
  , 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091
  , 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670
  , 0, 2, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812
  , 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32
  , 35190005, 10, 57996947, 18975, 10, 39121781, 32260, 10, 23000, 100, 23000, 100, 832808
  , 18, 3209094, 6, 331451, 1, 65990684, 23097, 18, 114242, 18, 94393407, 87060, 18, 16420089
  , 18, 2145798, 36, 3795345, 12, 889023, 1, 204237282, 23271, 36, 129165, 36, 189977790
  , 85902, 36, 33012864, 36, 388443360, 1, 401885761, 72, 2331379, 72, 1927926, 82523
  , 4, 117366, 10475, 4, 1292075, 24469, 74, 0, 1, 936157, 49601, 237, 0, 1
  ]
emptyConwayGenesis :: L.CostModel -> ConwayGenesis StandardCrypto
emptyConwayGenesis cm =
  let upgradePParamsDef :: (UpgradeConwayPParams Identity) -- TODO: need to define values
  -- the default instance is for StrictMaybe
      upgradePParamsDef =  UpgradeConwayPParams
                            { ucppPoolVotingThresholds = DefaultClass.def
                            , ucppDRepVotingThresholds = DefaultClass.def
                            , ucppCommitteeMinSize = 0
                            , ucppCommitteeMaxTermLength = EpochInterval 0
                            , ucppGovActionLifetime = EpochInterval 0
                            , ucppGovActionDeposit = 0
                            , ucppDRepDeposit = 0
                            , ucppDRepActivity = EpochInterval 0
                            , ucppMinFeeRefScriptCostPerByte = minBound
                            , ucppPlutusV3CostModel = cm
                            }
  in ConwayGenesis { cgUpgradePParams = upgradePParamsDef
                   , cgConstitution = DefaultClass.def
                   , cgCommittee = DefaultClass.def
                   , cgDelegs = mempty
                   , cgInitialDReps = mempty
                   }


readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Conway.ConwayGenesis StandardCrypto, GenesisHash)
readGenesis = readGenesisAny

validateGenesis :: Conway.ConwayGenesis StandardCrypto
                -> ExceptT ConwayProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO conway: do the validation

data ConwayProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | ConwayCostModelFileError !(FileError ())
  | ConwayCostModelDecodeError !FilePath !String
  deriving Show

instance Error ConwayProtocolInstantiationError where
  prettyError (InvalidCostModelError fp) =
    "Invalid cost model: " <> pshow fp
  prettyError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> pshow fp
  prettyError (ConwayCostModelFileError err) =
    prettyError err
  prettyError (ConwayCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> pshow fp <> " Error: " <> pshow err
