{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | All Byron and Shelley Genesis related functionality
module Testnet.Defaults
  ( defaultAlonzoGenesis
  , defaultByronProtocolParamsJsonValue
  , defaultYamlConfig
  , defaultConwayGenesis
  , defaultShelleyGenesis
  , defaultShelleyGenesisFp
  , defaultYamlHardforkViaConfig
  , defaultMainnetTopology
  , plutusV3NonSpendingScript
  , plutusV3SpendingScript
  ) where

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..), pshow)
import qualified Cardano.Api.Shelley as Api

import           Cardano.Ledger.Alonzo.Core (CoinPerWord (..), PParams (..))
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Binary.Version ()
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Conway.Genesis
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Shelley as Ledger
import           Cardano.Ledger.Shelley.Genesis
import           Cardano.Node.Configuration.Topology
import           Cardano.Tracing.Config

import           Prelude

import           Control.Monad.Identity (Identity)
import           Data.Aeson (ToJSON (..), Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMapAeson
import           Data.Bifunctor
import qualified Data.Default.Class as DefaultClass
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Ratio
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import qualified Data.Vector as Vector
import           Data.Word
import           Lens.Micro
import           Numeric.Natural

import           Testnet.Start.Types


instance Api.Error AlonzoGenesisError where
  prettyError (AlonzoGenErrCostModels e) =
    "Error in Alonzo genesis cost models: " <> pshow e
  prettyError (AlonzoGenErrTooMuchPrecision r) =
    "Too much precision for bounded rational in Alonzo genesis: " <> pshow r

data AlonzoGenesisError
  = AlonzoGenErrTooMuchPrecision Rational
  | AlonzoGenErrCostModels Api.ProtocolParametersConversionError
  deriving Show

defaultAlonzoGenesis :: Either AlonzoGenesisError AlonzoGenesis
defaultAlonzoGenesis = do
  es <- checkBoundedRational priceExecStepsRat
  ms <- checkBoundedRational priceMemStepsRat
  let execPrices = Prices ms es
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
                         { Api.executionSteps = 10000000000
                         , Api.executionMemory = 140000000
                         }
    maxBlockExUnits = Api.toAlonzoExUnits
                        $ Api.ExecutionUnits
                            { Api.executionSteps = 20000000000
                            , Api.executionMemory = 62000000
                            }
    apiCostModels =
      let pv1 = Api.AnyPlutusScriptVersion Api.PlutusScriptV1
          pv2 = Api.AnyPlutusScriptVersion Api.PlutusScriptV2
          pv3 = Api.AnyPlutusScriptVersion Api.PlutusScriptV3
      in mconcat [ Map.singleton pv1 defaultV1CostModel
                 , Map.singleton pv2 defaultV2CostModel
                 , Map.singleton pv3 defaultV3CostModel
                 ]
    defaultV1CostModel = Api.CostModel
                           [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
                           , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
                           , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
                           , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
                           , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
                           , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473
                           , 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
                           , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0
                           , 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0
                           , 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357
                           , 32, 32247, 32, 38314, 32, 57996947, 18975, 10
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
    defaultV3CostModel = Api.CostModel
                           [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 117366, 10475, 4, 832808, 18
                           , 3209094, 6, 331451, 1, 65990684, 23097, 18, 114242, 18, 94393407
                           , 87060, 18, 16420089, 18, 2145798, 36, 3795345, 12, 889023, 1, 204237282, 23271, 36, 129165, 36, 189977790
                           , 85902, 36, 33012864, 36, 388443360, 1, 401885761, 72, 2331379, 72, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000
                           , 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100
                           , 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662
                           , 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32
                           , 43249, 1000, 32, 32, 80556, 1, 57667, 4, 1927926, 82523, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896
                           , 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0
                           , 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990
                           , 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182
                           , 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35190005, 10, 57996947, 18975, 10, 39121781, 32260, 10
                           ]

defaultConwayGenesis :: ConwayGenesis StandardCrypto
defaultConwayGenesis = DefaultClass.def



-- | Configuration value that allows you to hardfork to any Cardano era
-- at epoch 0.
defaultYamlHardforkViaConfig :: AnyCardanoEra -> KeyMapAeson.KeyMap Aeson.Value
defaultYamlHardforkViaConfig era =
  mconcat $ concat
    [ defaultYamlConfig
    , tracers
    , protocolVersions era
    , hardforkViaConfig era
    ]

 where
  -- The protocol version number gets used by block producing nodes as part
  -- of the system for agreeing on and synchronising protocol updates.
  -- NB: We follow the mainnet protocol versions and assume the latest
  -- protocol version for a given era that has had an intraera hardfork.
  protocolVersions :: AnyCardanoEra -> [KeyMapAeson.KeyMap Aeson.Value]
  protocolVersions (AnyCardanoEra era') =
    case era' of
      ByronEra ->
        map (uncurry KeyMapAeson.singleton)
          -- We assume Byron with Ouroboros permissive BFT
          [ ("LastKnownBlockVersion-Major", Aeson.Number 1)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 2)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      AllegraEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 3)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      MaryEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 4)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      AlonzoEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 5)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      BabbageEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 8)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ConwayEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 9)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]

  -- Allows a direct hardfork to an era of your choice via the configuration.
  -- This removes the usual requirement for submitting an update proposal,
  -- waiting for the protocol to change and then restarting the nodes.
  hardforkViaConfig :: AnyCardanoEra -> [KeyMapAeson.KeyMap Aeson.Value]
  hardforkViaConfig (AnyCardanoEra era') =
    case era' of
      ByronEra -> []
      ShelleyEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          ]
      AllegraEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          ]
      MaryEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          ]
      AlonzoEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          ]
      BabbageEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
          ]
      ConwayEra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
          , ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
          , ("TestConwayHardForkAtEpoch", Aeson.Number 0)
          ]


  -- | Various tracers we can turn on or off
  tracers :: [KeyMapAeson.KeyMap Aeson.Value]
  tracers = map (\(k,v) -> KeyMapAeson.singleton (Key.fromText k) v)
    [ (proxyName (Proxy @TraceBlockFetchClient), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchDecisions), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchProtocolSerialised), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockFetchServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceBlockchainTime), Aeson.Bool True)
    , (proxyName (Proxy @TraceChainDB), Aeson.Bool True)
    , (proxyName (Proxy @TraceChainSyncClient), Aeson.Bool False)
    , (proxyName (Proxy @TraceChainSyncBlockServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceChainSyncHeaderServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceChainSyncProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceDnsResolver), Aeson.Bool True)
    , (proxyName (Proxy @TraceDnsSubscription), Aeson.Bool True)
    , (proxyName (Proxy @TraceErrorPolicy), Aeson.Bool True)
    , (proxyName (Proxy @TraceLocalErrorPolicy), Aeson.Bool True)
    , (proxyName (Proxy @TraceForge), Aeson.Bool True)
    , (proxyName (Proxy @TraceHandshake), Aeson.Bool False)
    , (proxyName (Proxy @TraceIpSubscription), Aeson.Bool True)
    , (proxyName (Proxy @TraceLocalRootPeers), Aeson.Bool True)
    , (proxyName (Proxy @TracePublicRootPeers), Aeson.Bool True)
    , (proxyName (Proxy @TracePeerSelection), Aeson.Bool True)
    , (proxyName (Proxy @TracePeerSelection), Aeson.Bool False)
    , (proxyName (Proxy @TracePeerSelectionActions), Aeson.Bool True)
    , (proxyName (Proxy @TraceConnectionManager), Aeson.Bool True)
    , (proxyName (Proxy @TraceServer), Aeson.Bool True)
    , (proxyName (Proxy @TraceLocalConnectionManager), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalChainSyncProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalHandshake), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalTxSubmissionProtocol), Aeson.Bool False)
    , (proxyName (Proxy @TraceLocalTxSubmissionServer), Aeson.Bool False)
    , (proxyName (Proxy @TraceMempool), Aeson.Bool True)
    , (proxyName (Proxy @TraceMux), Aeson.Bool False)
    , (proxyName (Proxy @TraceTxInbound), Aeson.Bool False)
    , (proxyName (Proxy @TraceTxOutbound), Aeson.Bool False)
    , (proxyName (Proxy @TraceTxSubmissionProtocol), Aeson.Bool False)
    ]

defaultScribes :: Aeson.Value
defaultScribes =
  Aeson.Array $ Vector.fromList
    [ Aeson.Array $ Vector.fromList ["FileSK","logs/mainnet.log"]
    , Aeson.Array $ Vector.fromList ["StdoutSK","stdout"]
    ]


rotationObject :: Aeson.Value
rotationObject =
  Aeson.Object $
    mconcat $ map (uncurry KeyMapAeson.singleton)
      [ ("rpLogLimitBytes", Aeson.Number 5000000)
      , ("rpKeepFilesNum", Aeson.Number 3)
      , ("rpMaxAgeHours", Aeson.Number 24)
      ]

setupScribes :: Aeson.Value
setupScribes =
  Aeson.Array $ Vector.fromList
    [ Aeson.Object $ mconcat $ map (uncurry KeyMapAeson.singleton)
        [ ("scKind", "FileSK")
        , ("scName", "logs/node.log")
        , ("scFormat", "ScJson")
        ]
    , Aeson.Object $ mconcat $ map (uncurry KeyMapAeson.singleton)
        [ ("scKind", "StdoutSK")
        , ("scName", "stdout")
        , ("scFormat", "ScJson")
        ]
    ]

defaultYamlConfig :: [KeyMapAeson.KeyMap Aeson.Value]
defaultYamlConfig =
  map (uncurry KeyMapAeson.singleton)
    [
    -- The consensus protocol to use
      ("Protocol", "Cardano")

    -- Socket path of the node
    , ("SocketPath", "db/node.socket")
    , ("PBftSignatureThreshold", Aeson.Number (fromFloatDigits (0.6 :: Double)))

    -- Global logging severity filter. Messages must have at least this severity to pass.
    , ("minSeverity", "Debug")

    , ("EnableLogMetrics", Aeson.Bool False)
    , ("TurnOnLogMetrics", Aeson.Bool False)

    -- The maximum number of used peers during bulk sync.
    , ("MaxConcurrencyBulkSync", Aeson.Number 1)

    -- The maximum number of used peers when fetching newly forged blocks.
    , ("MaxConcurrencyDeadline", Aeson.Number 2)

    -- Turn logging on or off
    , ("EnableLogging", Aeson.Bool True)

    -- Genesis filepaths
    , ("ByronGenesisFile", "byron/genesis.json")
    , ("ShelleyGenesisFile", Aeson.String $ Text.pack defaultShelleyGenesisFp)
    , ("AlonzoGenesisFile", "shelley/genesis.alonzo.json")
    , ("ConwayGenesisFile", "shelley/genesis.conway.json")

    -- See: https://github.com/input-output-hk/cardano-ledger/blob/master/eras/byron/ledger/impl/doc/network-magic.md
    , ("RequiresNetworkMagic", "RequiresMagic")

    -- Enable peer to peer discovery
    , ("EnableP2P", Aeson.Bool False)

    -- Logging related
    , ("setupScribes", setupScribes)
    , ("rotation", rotationObject)
    , ("defaultScribes", defaultScribes)
    , ("setupBackends", Aeson.Array $ Vector.fromList ["KatipBK"])
    , ("defaultBackends", Aeson.Array $ Vector.fromList ["KatipBK"])
    , ("options", Aeson.object mempty)
    ]

-- | We need a Byron genesis in order to be able to hardfork to the later Shelley based eras.
-- The values here don't matter as the testnet conditions are ultimately determined
-- by the Shelley genesis.
defaultByronProtocolParamsJsonValue :: Value
defaultByronProtocolParamsJsonValue =
  Aeson.object
    [ "heavyDelThd" .= toJSON @String "300000000000"
    , "maxBlockSize" .= toJSON @String "2000000"
    , "maxTxSize" .= toJSON @String "4096"
    , "maxHeaderSize" .= toJSON @String "2000000"
    , "maxProposalSize" .= toJSON @String "700"
    , "mpcThd" .= toJSON @String "20000000000000"
    , "scriptVersion" .= toJSON @Int 0
    , "slotDuration" .= toJSON @String "1000"
    , "softforkRule" .= Aeson.object
      [ "initThd" .= toJSON @String "900000000000000"
      , "minThd" .= toJSON @String "600000000000000"
      , "thdDecrement" .= toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= Aeson.object
      [ "multiplier" .= toJSON @String "43946000000"
      , "summand" .= toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= toJSON @String "18446744073709551615"
    , "updateImplicit" .= toJSON @String "10000"
    , "updateProposalThd" .= toJSON @String "100000000000000"
    , "updateVoteThd" .= toJSON @String "1000000000000"
    ]

defaultShelleyGenesis
  :: UTCTime
  -> CardanoTestnetOptions
  -> Api.ShelleyGenesis StandardCrypto
defaultShelleyGenesis startTime testnetOptions =
  let testnetMagic = cardanoTestnetMagic testnetOptions
      slotLength = cardanoSlotLength testnetOptions
      epochLength = cardanoEpochLength testnetOptions
      maxLovelaceLovelaceSupply = cardanoMaxSupply testnetOptions
      pVer = eraToProtocolVersion $ cardanoNodeEra testnetOptions
      protocolParams = Api.sgProtocolParams Api.shelleyGenesisDefaults
      protocolParamsWithPVer = protocolParams & ppProtocolVersionL' .~ pVer
  in Api.shelleyGenesisDefaults
        { Api.sgNetworkMagic = fromIntegral testnetMagic
        , Api.sgSlotLength = secondsToNominalDiffTimeMicro $ realToFrac slotLength
        , Api.sgEpochLength = EpochSize $ fromIntegral epochLength
        , Api.sgMaxLovelaceSupply = maxLovelaceLovelaceSupply
        , Api.sgSystemStart = startTime
        , Api.sgProtocolParams = protocolParamsWithPVer
        }


eraToProtocolVersion :: AnyCardanoEra -> ProtVer
eraToProtocolVersion (AnyCardanoEra era) =
  case era of
    ByronEra -> error "eraToProtocolVersion: Byron not supported"
    ShelleyEra -> mkProtVer (2, 0)
    AllegraEra -> mkProtVer (3, 0)
    MaryEra -> mkProtVer (4, 0)
    -- Alonzo had an intra-era hardfork
    AlonzoEra -> mkProtVer (6, 0)
    -- Babbage had an intra-era hardfork
    BabbageEra -> mkProtVer (8, 0)
    ConwayEra -> mkProtVer (9, 0)

-- TODO: Expose from cardano-api
mkProtVer :: (Natural, Natural) -> ProtVer
mkProtVer (majorProtVer, minorProtVer) =
  case (`ProtVer` minorProtVer) <$> Ledger.mkVersion majorProtVer of
    Just pVer -> pVer
    Nothing -> error "mkProtVer: invalid protocol version"

ppProtocolVersionL' ::  Lens' (PParams (Ledger.ShelleyEra StandardCrypto)) ProtVer
ppProtocolVersionL' = Ledger.ppLens . Ledger.hkdProtocolVersionL @(Ledger.ShelleyEra StandardCrypto) @Identity

defaultMainnetTopology :: NetworkTopology
defaultMainnetTopology =
  let single = RemoteAddress
         { raAddress  = "relays-new.cardano-mainnet.iohk.io"
         , raPort     = 3001
         , raValency  = 2
         }
  in RealNodeTopology [single]

-- TODO: These filepaths should really be decoded from the output of
-- create-testnet-data. We are still deciding the best way to pass this information
-- to cardano-testnet.

defaultShelleyGenesisFp :: FilePath
defaultShelleyGenesisFp = "shelley/genesis.shelley.json"


-- TODO: We should not hardcode a script like this. We need to move
-- plutus-example from plutus apps to cardano-node-testnet. This will
-- let us directly compile the plutus validators and avoid bit rotting of
-- hardcoded plutus scripts.
-- | Default plutus script that succeeds regardless of redeemer
-- NB: This cannot be used as a spending script
plutusV3NonSpendingScript :: Text
plutusV3NonSpendingScript =
  Text.unlines ["{"
               , "\"type\": \"PlutusScriptV3\""
               , ",\"description\": \"\""
               , ",\"cborHex\": \"4746010100228001\""
               , "}"
               ]

-- | Default plutus spending script that succeeds regardless of redeemer
plutusV3SpendingScript :: Text
plutusV3SpendingScript =
  Text.unlines ["{"
               , "\"type\": \"PlutusScriptV3\""
               , ",\"description\": \"\""
               , ",\"cborHex\": \"484701010022280001\""
               , "}"
               ]
