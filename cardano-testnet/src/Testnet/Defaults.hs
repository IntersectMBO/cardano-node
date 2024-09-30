{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
  , defaultCommitteeKeyPair
  , defaultCommitteeVkeyFp
  , defaultCommitteeSkeyFp
  , defaultDRepSkeyFp
  , defaultDRepKeyPair
  , defaultDelegatorStakeKeyPair
  , defaultSpoColdKeyPair
  , defaultSPOColdVKeyFp
  , defaultSPOColdSKeyFp
  , defaultSpoKeys
  , defaultShelleyGenesis
  , defaultGenesisFilepath
  , defaultYamlHardforkViaConfig
  , defaultMainnetTopology
  , plutusV3Script
  ) where

import           Cardano.Api (CardanoEra (..), File (..), pshow, ShelleyBasedEra (..),
                   toCardanoEra, unsafeBoundedRational, AnyShelleyBasedEra (..))
import qualified Cardano.Api.Shelley as Api

import           Cardano.Ledger.Alonzo.Core (PParams (..))
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import qualified Cardano.Ledger.Alonzo.Genesis as Ledger
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Binary.Version ()
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Conway.Genesis
import           Cardano.Ledger.Conway.PParams
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Plutus as Ledger
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
import qualified Data.Default.Class as DefaultClass
import           Data.Proxy
import           Data.Ratio
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import qualified Data.Vector as Vector
import           Data.Word (Word64)
import           Lens.Micro
import           Numeric.Natural
import           System.FilePath ((</>))

import           Test.Cardano.Ledger.Core.Rational
import           Test.Cardano.Ledger.Plutus (testingCostModelV3)
import           Testnet.Start.Types
import           Testnet.Types

{- HLINT ignore "Use underscore" -}

instance Api.Error AlonzoGenesisError where
  prettyError (AlonzoGenErrTooMuchPrecision r) =
    "Too much precision for bounded rational in Alonzo genesis: " <> pshow r

newtype AlonzoGenesisError
  = AlonzoGenErrTooMuchPrecision Rational
  deriving Show

defaultAlonzoGenesis :: ShelleyBasedEra era -> Either AlonzoGenesisError AlonzoGenesis
defaultAlonzoGenesis sbe = do
  let genesis = Api.alonzoGenesisDefaults (toCardanoEra sbe)
      prices = Ledger.agPrices genesis

  -- double check that prices have correct values - they're set using unsafeBoundedRational in cardano-api
  _priceExecSteps <- checkBoundedRational $ Ledger.prSteps prices
  _priceMemSteps <- checkBoundedRational $ Ledger.prMem prices

  pure genesis
  where
    checkBoundedRational :: BoundedRational a => a -> Either AlonzoGenesisError a
    checkBoundedRational v = do
      let r = unboundRational v
      case boundRational r of
        Nothing -> Left $ AlonzoGenErrTooMuchPrecision r
        Just s -> return s

defaultConwayGenesis :: ConwayGenesis StandardCrypto
defaultConwayGenesis =
  let upPParams :: UpgradeConwayPParams Identity
      upPParams = UpgradeConwayPParams
                    { ucppPoolVotingThresholds = poolVotingThresholds
                    , ucppDRepVotingThresholds = drepVotingThresholds
                    , ucppCommitteeMinSize = 0
                    , ucppCommitteeMaxTermLength = EpochInterval 200
                    , ucppGovActionLifetime = EpochInterval 1 -- One Epoch
                    , ucppGovActionDeposit = Coin 1_000_000
                    , ucppDRepDeposit = Coin 1_000_000
                    , ucppDRepActivity = EpochInterval 100
                    , ucppMinFeeRefScriptCostPerByte = 0 %! 1 -- FIXME GARBAGE VALUE
                    , ucppPlutusV3CostModel = testingCostModelV3
                    }
      drepVotingThresholds = DRepVotingThresholds
        { dvtMotionNoConfidence = 0 %! 1
        , dvtCommitteeNormal = 1 %! 2
        , dvtCommitteeNoConfidence = 0 %! 1
        , dvtUpdateToConstitution = 0 %! 2 -- TODO: Requires a constitutional committee when non-zero
        , dvtHardForkInitiation = 1 %! 2
        , dvtPPNetworkGroup = 1 %! 2
        , dvtPPEconomicGroup = 1 %! 2
        , dvtPPTechnicalGroup = 1 %! 2
        , dvtPPGovGroup = 1 %! 2
        , dvtTreasuryWithdrawal = 1 %! 2
        }
      poolVotingThresholds = PoolVotingThresholds
         { pvtMotionNoConfidence = 1 %! 2
         , pvtCommitteeNormal = 1 %! 2
         , pvtCommitteeNoConfidence = 1 %! 2
         , pvtHardForkInitiation = 1 %! 2
         , pvtPPSecurityGroup = 1 %! 2
         }
  in ConwayGenesis
      { cgUpgradePParams = upPParams
      , cgConstitution = DefaultClass.def
      , cgCommittee = DefaultClass.def
      , cgDelegs = mempty
      , cgInitialDReps = mempty
      }



-- | Configuration value that allows you to hardfork to any Cardano era
-- at epoch 0.
defaultYamlHardforkViaConfig :: ShelleyBasedEra era -> KeyMapAeson.KeyMap Aeson.Value
defaultYamlHardforkViaConfig sbe =
  mconcat $ concat
    [ defaultYamlConfig
    , tracers
    , protocolVersions sbe
    , hardforkViaConfig sbe
    ]

 where
  -- The protocol version number gets used by block producing nodes as part
  -- of the system for agreeing on and synchronising protocol updates.
  -- NB: We follow the mainnet protocol versions and assume the latest
  -- protocol version for a given era that has had an intraera hardfork.
  protocolVersions :: ShelleyBasedEra era -> [KeyMapAeson.KeyMap Aeson.Value]
  protocolVersions sbe' =
    case sbe' of
      ShelleyBasedEraShelley ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 2)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyBasedEraAllegra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 3)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyBasedEraMary ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 4)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyBasedEraAlonzo ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 5)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyBasedEraBabbage ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 8)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]
      ShelleyBasedEraConway ->
        map (uncurry KeyMapAeson.singleton)
          [ ("LastKnownBlockVersion-Major", Aeson.Number 9)
          , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
          , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
          ]

  -- Allows a direct hardfork to an era of your choice via the configuration.
  -- This removes the usual requirement for submitting an update proposal,
  -- waiting for the protocol to change and then restarting the nodes.
  hardforkViaConfig :: ShelleyBasedEra era -> [KeyMapAeson.KeyMap Aeson.Value]
  hardforkViaConfig sbe' =
    case sbe' of
      ShelleyBasedEraShelley ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          ]
      ShelleyBasedEraAllegra ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          ]
      ShelleyBasedEraMary ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          ]
      ShelleyBasedEraAlonzo ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          ]
      ShelleyBasedEraBabbage ->
        map (uncurry KeyMapAeson.singleton)
          [ ("ExperimentalProtocolsEnabled", Aeson.Bool True)
          , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
          , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
          , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
          , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
          , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
          ]
      ShelleyBasedEraConway ->
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
    , ("ShelleyGenesisFile", genesisPath ShelleyEra)
    , ("AlonzoGenesisFile",  genesisPath AlonzoEra)
    , ("ConwayGenesisFile",  genesisPath ConwayEra)

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
  where
    genesisPath era = Aeson.String $ Text.pack $ defaultGenesisFilepath era

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
  :: AnyShelleyBasedEra
  -> UTCTime
  -> Word64
  -> ShelleyTestnetOptions
  -> Api.ShelleyGenesis StandardCrypto
defaultShelleyGenesis asbe startTime maxSupply options = do
  let ShelleyTestnetOptions
        { shelleyTestnetMagic = magic
        , shelleySlotLength = slotLength
        , shelleyEpochLength = epochLength
        , shelleyActiveSlotsCoeff
        } = options
      -- f
      activeSlotsCoeff = round (shelleyActiveSlotsCoeff * 100) % 100
      -- make security param k satisfy: epochLength = 10 * k / f
      -- TODO: find out why this actually degrates network stability - turned off for now
      -- securityParam = ceiling $ fromIntegral epochLength * cardanoActiveSlotsCoeff / 10
      pVer = eraToProtocolVersion asbe
      protocolParams = Api.sgProtocolParams Api.shelleyGenesisDefaults
      protocolParamsWithPVer = protocolParams & ppProtocolVersionL' .~ pVer
  Api.shelleyGenesisDefaults
        { Api.sgActiveSlotsCoeff = unsafeBoundedRational activeSlotsCoeff
        , Api.sgEpochLength = EpochSize $ fromIntegral epochLength
        , Api.sgMaxLovelaceSupply = maxSupply
        , Api.sgNetworkMagic = fromIntegral magic
        , Api.sgProtocolParams = protocolParamsWithPVer
        -- using default from shelley genesis k = 2160
        -- , Api.sgSecurityParam = securityParam
        , Api.sgSlotLength = secondsToNominalDiffTimeMicro $ realToFrac slotLength
        , Api.sgSystemStart = startTime
        }


eraToProtocolVersion :: AnyShelleyBasedEra -> ProtVer
eraToProtocolVersion =
  \case
    AnyShelleyBasedEra ShelleyBasedEraShelley -> mkProtVer (2, 0)
    AnyShelleyBasedEra ShelleyBasedEraAllegra -> mkProtVer (3, 0)
    AnyShelleyBasedEra ShelleyBasedEraMary -> mkProtVer (4, 0)
    -- Alonzo had an intra-era hardfork
    AnyShelleyBasedEra ShelleyBasedEraAlonzo -> mkProtVer (6, 0)
    -- Babbage had an intra-era hardfork
    AnyShelleyBasedEra ShelleyBasedEraBabbage -> mkProtVer (8, 0)
    -- By default start after bootstrap (which is PV9)
    AnyShelleyBasedEra ShelleyBasedEraConway -> mkProtVer (10, 0)

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

defaultGenesisFilepath :: CardanoEra a -> FilePath
defaultGenesisFilepath era =
  -- This path is actually generated by create-testnet-data. Don't change it.
  eraToString era <> "-genesis.json"

defaultCommitteeVkeyFp
  :: Int -- ^ The Committee's index (starts at 1)
  -> FilePath
defaultCommitteeVkeyFp n = "committee-keys" </> "committee" <> show n <> ".vkey"

defaultCommitteeSkeyFp
  :: Int -- ^ The Committee's index (starts at 1)
  -> FilePath
defaultCommitteeSkeyFp n = "committee-keys" </> "committee" <> show n <> ".skey"

-- | The relative path to DRep keys in directories created by cardano-testnet
defaultDRepKeyPair
  :: Int -- ^ The DRep's index (starts at 1)
  -> KeyPair PaymentKey
defaultDRepKeyPair n =
  KeyPair
    { verificationKey = File $ "drep-keys" </> ("drep" <> show n) </> "drep.vkey"
    , signingKey = File $ "drep-keys" </> ("drep" <> show n) </> "drep.skey"
    }

-- | The relative path to DRep secret keys in directories created by cardano-testnet
defaultDRepSkeyFp
  :: Int -- ^ The DRep's index (starts at 1)
  -> FilePath
defaultDRepSkeyFp n = "drep-keys" </> ("drep" <> show n) </> "drep.skey"

defaultCommitteeKeyPair :: Int -> KeyPair PaymentKey
defaultCommitteeKeyPair n =
  KeyPair
    { verificationKey = File $ defaultCommitteeVkeyFp n
    , signingKey = File $ defaultCommitteeSkeyFp n
    }

-- | The relative path to SPO cold verification key in directories created by cardano-testnet
defaultSPOColdVKeyFp :: Int -> FilePath
defaultSPOColdVKeyFp n = "pools-keys" </> "pool" <> show n </> "cold.vkey"

-- | The relative path to SPO cold secret key in directories created by cardano-testnet
defaultSPOColdSKeyFp :: Int -> FilePath
defaultSPOColdSKeyFp n = "pools-keys" </> "pool" <> show n </> "cold.skey"

-- | The relative path to SPO keys in directories created by cardano-testnet
defaultSpoColdKeyPair
  :: Int
  -> KeyPair SpoColdKey
defaultSpoColdKeyPair n =
  KeyPair
    { verificationKey = File $ "pools-keys" </> "pool" <> show n </> "cold.vkey"
    , signingKey = File $ "pools-keys" </> "pool" <> show n </> "cold.skey"
    }

-- | The relative path to SPO key pairs in directories created by cardano-testnet
defaultSpoKeys :: Int -> PoolNodeKeys
defaultSpoKeys n =
  PoolNodeKeys
    { poolNodeKeysCold = defaultSpoColdKeyPair n
    , poolNodeKeysVrf =
      KeyPair
        { verificationKey = File $ "pools-keys" </> "pool" ++ show n </> "vrf.vkey"
        , signingKey = File $ "pools-keys" </> "pool" ++ show n </> "vrf.skey"
        }
    , poolNodeKeysStaking =
      KeyPair
        { verificationKey = File $ "pools-keys" </> "pool" ++ show n </> "staking-reward.vkey"
        , signingKey = File $ "pools-keys" </> "pool" ++ show n </> "staking-reward.skey"
        }
    }

-- | The relative path to stake delegator key pairs in directories created by cardano-testnet
defaultDelegatorStakeKeyPair :: Int -> KeyPair StakingKey
defaultDelegatorStakeKeyPair n =
  KeyPair
    { verificationKey = File $ "stake-delegators" </> ("delegator" <> show n) </> "staking.vkey"
    , signingKey = File $ "stake-delegators" </> ("delegator" <> show n) </> "staking.skey"
    }

-- | Default plutus script that always succeeds
plutusV3Script :: Text
plutusV3Script =
  "{ \"type\": \"PlutusScriptV3\", \"description\": \"\", \"cborHex\": \"46450101002499\" }"

