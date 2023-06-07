{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | All Byron and Shelley Genesis related functionality
module Testnet.Commands.Genesis
  ( createShelleyGenesisInitialTxIn
  , defaultAlonzoGenesis
  , defaultConwayGenesis
  ) where

import           Prelude

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import           Data.Ratio
import           Data.Word
import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Cardano.Ledger.Alonzo.Core (CoinPerWord (..))
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Conway.Genesis
import           Cardano.Ledger.Keys

import qualified Cardano.Api.Shelley as Api

import           Hedgehog.Internal.Property

import           Cardano.Ledger.Crypto (StandardCrypto)
import           Testnet.Util.Process



-- | The Shelley initial UTxO is constructed from the 'sgInitialFunds' field which
-- is not a full UTxO but just a map from addresses to coin values. Therefore this
-- command creates a transaction input that defaults to the 0th index and therefore
-- we can spend spend this tx input in a transaction.
createShelleyGenesisInitialTxIn
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -> FilePath -> m String
createShelleyGenesisInitialTxIn testnetMagic vKeyFp =
  withFrozenCallStack $ execCli
      [ "genesis", "initial-txin"
      , "--testnet-magic", show @Int testnetMagic
      , "--verification-key-file", vKeyFp
      ]

instance Api.Error AlonzoGenesisError where
  displayError (AlonzoGenErrCostModels e) =
    "Error in Alonzo genesis cost models: " <> show e
  displayError (AlonzoGenErrTooMuchPrecision r) =
    "Too much precision for bounded rational in Alonzo genesis: " ++ show r

data AlonzoGenesisError
  = AlonzoGenErrTooMuchPrecision Rational
  | AlonzoGenErrCostModels Api.ProtocolParametersConversionError
  deriving Show

defaultAlonzoGenesis :: Either AlonzoGenesisError AlonzoGenesis
defaultAlonzoGenesis = do
  es <- checkBoundedRational priceExecStepsRat
  ms <- checkBoundedRational priceMemStepsRat
  let execPrices = Prices es ms
  costModels <- first AlonzoGenErrCostModels
                  $ Api.toAlonzoCostModels apiCostModels
  return $ AlonzoGenesis
    { agCoinsPerUTxOWord = CoinPerWord $ Coin 34482
    , agCostModels = costModels
    , agPrices = execPrices
    , agMaxTxExUnits = maxTxExUnits
    , agMaxBlockExUnits = maxBlockExUnits
    , agMaxValSize = 5000
    , agCollateralPercentage = 150
    , agMaxCollateralInputs = 3
    }
  where

    priceExecStepsRat = promoteRatio $ 721 % (10000000 :: Word64)
    priceMemStepsRat = promoteRatio $ 577 % (10000 :: Word64)

    checkBoundedRational r =
      case boundRational r of
        Nothing -> Left $ AlonzoGenErrTooMuchPrecision r
        Just s -> return s

    maxTxExUnits = Api.toAlonzoExUnits
                     $ Api.ExecutionUnits
                         { Api.executionSteps = 16000000
                         , Api.executionMemory = 10000000000
                         }
    maxBlockExUnits = Api.toAlonzoExUnits
                        $ Api.ExecutionUnits
                            { Api.executionSteps = 40000000000
                            , Api.executionMemory = 80000000
                            }
    apiCostModels =
      let pv1 = Api.AnyPlutusScriptVersion Api.PlutusScriptV1
          pv2 = Api.AnyPlutusScriptVersion Api.PlutusScriptV2
      in mconcat [ Map.singleton pv1 defaultV1CostModel
                 , Map.singleton pv2 defaultV2CostModel
                 ]
    defaultV1CostModel = Api.CostModel
                           [ 4, 1000, 100, 103599, 1, 621, 29175, 150000, 1000, 150000, 61516, 100, 150000, 150000
                           , 150000, 32, 150000, 29773, 150000, 0, 150000, 0, 1, 118, 150000, 150000, 32, 136542
                           , 82363, 5000, 150000, 179690, 150000, 0, 118, 1, 150000, 145276, 1, 32, 32, 150000, 1
                           , 1, 0, 4, 32, 32, 150000, 32, 1, 32, 248, 0, 100, 0, 32, 118, 29773, 1, 29773, 29175
                           , 1, 1, 1, 150000, 150000, 29773, 150000, 1, 1000, 1, 1366, 32, 0, 150000, 1, 32, 32
                           , 197209, 8, 150000, 150000, 150000, 148000, 1, 100, 150000, 150000, 1326, 100, 197209
                           , 425507, 0, 100, 2477736, 148000, 150000, 150000, 1000, 1, 11218, 396231, 248, 1, 0
                           , 10000, 0, 150000, 150000, 1, 29773, 1, 3345831, 32, 32, 1, 4, 1, 32, 247, 150000, 118
                           , 100, 1, 1, 100, 0, 2477736, 425507, 1, 32, 150000, 150000, 32, 4, 32, 32, 29773, 1
                           , 103599, 1000, 1, 32, 148000, 29773, 8, 425507, 32, 1000, 148000, 1, 32, 0, 150000, 0
                           , 32, 112536, 1, 497, 425507, 1, 0, 1, 100, 150000
                           ]
    defaultV2CostModel = Api.CostModel
                           [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
                           , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
                           , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
                           , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
                           , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
                           , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473
                           , 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
                           , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0
                           , 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990, 30482, 4, 1927926
                           , 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220
                           , 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10, 9462713, 1021, 10, 38887044
                           , 32947, 10
                           ]


defaultConwayGenesis :: ConwayGenesis StandardCrypto
defaultConwayGenesis = ConwayGenesis { cgGenDelegs = GenDelegs mempty}
