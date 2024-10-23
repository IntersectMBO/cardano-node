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
  , defaultNodeName
  , defaultNodeDataDir
  , defaultSpoColdKeyPair
  , defaultSpoColdVKeyFp
  , defaultSpoColdSKeyFp
  , defaultSpoKeys
  , defaultSpoKeysDir
  , defaultSpoName
  , defaultShelleyGenesis
  , defaultGenesisFilepath
  , defaultYamlHardforkViaConfig
  , defaultMainnetTopology
  , defaultUtxoKeys
  , plutusV3Script
  , plutusV3SupplementalDatumScript
  ) where

import           Cardano.Api (AnyShelleyBasedEra (..), CardanoEra (..), File (..),
                   ShelleyBasedEra (..), pshow, toCardanoEra, unsafeBoundedRational)
import qualified Cardano.Api.Shelley as Api

import           Cardano.Ledger.Alonzo.Core (PParams (..))
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import qualified Cardano.Ledger.Alonzo.Genesis as Ledger
import qualified Cardano.Ledger.Api as L
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
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.Bifunctor (bimap)
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
defaultYamlHardforkViaConfig :: ShelleyBasedEra era -> Aeson.KeyMap Aeson.Value
defaultYamlHardforkViaConfig sbe =
  defaultYamlConfig
    <> tracers
    <> protocolVersions sbe
    <> hardforkViaConfig sbe
 where
  -- The protocol version number gets used by block producing nodes as part
  -- of the system for agreeing on and synchronising protocol updates.
  -- NB: We follow the mainnet protocol versions and assume the latest
  -- protocol version for a given era that has had an intraera hardfork.
  protocolVersions :: ShelleyBasedEra era -> Aeson.KeyMap Aeson.Value
  protocolVersions sbe' =
    Aeson.fromList
      [case sbe' of
        ShelleyBasedEraShelley -> ("LastKnownBlockVersion-Major", Aeson.Number 2)
        ShelleyBasedEraAllegra -> ("LastKnownBlockVersion-Major", Aeson.Number 3)
        ShelleyBasedEraMary -> ("LastKnownBlockVersion-Major", Aeson.Number 4)
        ShelleyBasedEraAlonzo -> ("LastKnownBlockVersion-Major", Aeson.Number 5)
        ShelleyBasedEraBabbage -> ("LastKnownBlockVersion-Major", Aeson.Number 8)
        ShelleyBasedEraConway -> ("LastKnownBlockVersion-Major", Aeson.Number 9)
      , ("LastKnownBlockVersion-Minor", Aeson.Number 0)
      , ("LastKnownBlockVersion-Alt", Aeson.Number 0)
      ]

  -- Allows a direct hardfork to an era of your choice via the configuration.
  -- This removes the usual requirement for submitting an update proposal,
  -- waiting for the protocol to change and then restarting the nodes.
  hardforkViaConfig :: ShelleyBasedEra era -> Aeson.KeyMap Aeson.Value
  hardforkViaConfig sbe' =
    Aeson.fromList $
      [ ("ExperimentalHardForksEnabled", Aeson.Bool True)
      , ("ExperimentalProtocolsEnabled", Aeson.Bool True) ]
      ++ (case sbe' of
            ShelleyBasedEraShelley ->
                [ ("TestShelleyHardForkAtEpoch", Aeson.Number 0) ]
            ShelleyBasedEraAllegra ->
                [ ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
                , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
                ]
            ShelleyBasedEraMary ->
                [ ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
                , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
                , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
                ]
            ShelleyBasedEraAlonzo ->
                [ ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
                , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
                , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
                , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
                ]
            ShelleyBasedEraBabbage ->
                [ ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
                , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
                , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
                , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
                , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
                ]
            ShelleyBasedEraConway ->
                [ ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
                , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
                , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
                , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
                , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
                , ("TestConwayHardForkAtEpoch", Aeson.Number 0)
                ])
  -- | Various tracers we can turn on or off
  tracers :: Aeson.KeyMap Aeson.Value
  tracers = Aeson.fromList $ map (bimap Aeson.fromText Aeson.Bool)
    [ (proxyName (Proxy @TraceBlockFetchClient), False)
    , (proxyName (Proxy @TraceBlockFetchDecisions), False)
    , (proxyName (Proxy @TraceBlockFetchProtocol), False)
    , (proxyName (Proxy @TraceBlockFetchProtocolSerialised), False)
    , (proxyName (Proxy @TraceBlockFetchServer), False)
    , (proxyName (Proxy @TraceBlockchainTime), True)
    , (proxyName (Proxy @TraceChainDB), True)
    , (proxyName (Proxy @TraceChainSyncClient), False)
    , (proxyName (Proxy @TraceChainSyncBlockServer), False)
    , (proxyName (Proxy @TraceChainSyncHeaderServer), False)
    , (proxyName (Proxy @TraceChainSyncProtocol), False)
    , (proxyName (Proxy @TraceDnsResolver), True)
    , (proxyName (Proxy @TraceDnsSubscription), True)
    , (proxyName (Proxy @TraceErrorPolicy), True)
    , (proxyName (Proxy @TraceLocalErrorPolicy), True)
    , (proxyName (Proxy @TraceForge), True)
    , (proxyName (Proxy @TraceHandshake), False)
    , (proxyName (Proxy @TraceIpSubscription), True)
    , (proxyName (Proxy @TraceLocalRootPeers), True)
    , (proxyName (Proxy @TracePublicRootPeers), True)
    , (proxyName (Proxy @TracePeerSelection), True)
    , (proxyName (Proxy @TracePeerSelectionActions), True)
    , (proxyName (Proxy @TraceConnectionManager), True)
    , (proxyName (Proxy @TraceServer), True)
    , (proxyName (Proxy @TraceLocalConnectionManager), False)
    , (proxyName (Proxy @TraceLocalServer), False)
    , (proxyName (Proxy @TraceLocalChainSyncProtocol), False)
    , (proxyName (Proxy @TraceLocalHandshake), False)
    , (proxyName (Proxy @TraceLocalTxSubmissionProtocol), False)
    , (proxyName (Proxy @TraceLocalTxSubmissionServer), False)
    , (proxyName (Proxy @TraceMempool), True)
    , (proxyName (Proxy @TraceMux), False)
    , (proxyName (Proxy @TraceTxInbound), False)
    , (proxyName (Proxy @TraceTxOutbound), False)
    , (proxyName (Proxy @TraceTxSubmissionProtocol), False)
    ]

defaultYamlConfig :: Aeson.KeyMap Aeson.Value
defaultYamlConfig =
  Aeson.fromList
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
    defaultScribes :: Aeson.Value
    defaultScribes =
      Aeson.Array $ Vector.fromList
        [ Aeson.Array $ Vector.fromList ["FileSK","logs/mainnet.log"]
        , Aeson.Array $ Vector.fromList ["StdoutSK","stdout"]
        ]
    rotationObject :: Aeson.Value
    rotationObject =
      Aeson.Object $
        mconcat $ map (uncurry Aeson.singleton)
          [ ("rpLogLimitBytes", Aeson.Number 5_000_000)
          , ("rpKeepFilesNum", Aeson.Number 3)
          , ("rpMaxAgeHours", Aeson.Number 24)
          ]
    setupScribes :: Aeson.Value
    setupScribes =
      Aeson.Array $ Vector.fromList
        [ Aeson.Object $ mconcat $ map (uncurry Aeson.singleton)
            [ ("scKind", "FileSK")
            , ("scName", "logs/node.log")
            , ("scFormat", "ScJson")
            ]
        , Aeson.Object $ mconcat $ map (uncurry Aeson.singleton)
            [ ("scKind", "StdoutSK")
            , ("scName", "stdout")
            , ("scFormat", "ScJson")
            ]
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
  :: AnyShelleyBasedEra
  -> UTCTime
  -> Word64
  -> GenesisOptions
  -> Api.ShelleyGenesis StandardCrypto
defaultShelleyGenesis asbe startTime maxSupply options = do
  let GenesisOptions
        { genesisTestnetMagic = magic
        , genesisSlotLength = slotLength
        , genesisEpochLength = epochLength
        , genesisActiveSlotsCoeff
        } = options
      -- f
      activeSlotsCoeff = round (genesisActiveSlotsCoeff * 100) % 100
      -- make security param k satisfy: epochLength = 10 * k / f
      -- TODO: find out why this actually degrates network stability - turned off for now
      -- securityParam = ceiling $ fromIntegral epochLength * cardanoActiveSlotsCoeff / 10
      pVer = eraToProtocolVersion asbe
      -- TODO: Remove after merging https://github.com/IntersectMBO/cardano-node/pull/6017
      protocolParams = Api.sgProtocolParams Api.shelleyGenesisDefaults & L.ppKeyDepositL .~ 0
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
         , raPort     = 3_001
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
defaultSpoColdVKeyFp :: Int -> FilePath
defaultSpoColdVKeyFp n = defaultSpoKeysDir n </> "cold.vkey"

-- | The relative path to SPO cold secret key in directories created by cardano-testnet
defaultSpoColdSKeyFp :: Int -> FilePath
defaultSpoColdSKeyFp n = defaultSpoKeysDir n </> "cold.skey"

-- | The name of a SPO, used in file system operations
defaultSpoName :: Int -> String
defaultSpoName n = "pool" <> show n

-- | The name of a node (which doesn't have to be a SPO)
defaultNodeName :: Int -> String
defaultNodeName n = "node" <> show n

-- | The relative path of the node data dir, where the database is stored
defaultNodeDataDir :: Int -> String
defaultNodeDataDir n = "node-data" </> defaultNodeName n

-- | The relative path where the SPO keys for the node are stored
defaultSpoKeysDir :: Int -> String
defaultSpoKeysDir n = "pools-keys" </> defaultSpoName n

-- | The relative path to SPO keys in directories created by cardano-testnet
defaultSpoColdKeyPair
  :: Int
  -> KeyPair SpoColdKey
defaultSpoColdKeyPair n =
  KeyPair
    { verificationKey = File $ defaultSpoKeysDir n </> "cold.vkey"
    , signingKey = File $ defaultSpoKeysDir n </> "cold.skey"
    }

-- | The relative path to SPO key pairs in directories created by cardano-testnet
defaultSpoKeys :: Int -> SpoNodeKeys
defaultSpoKeys n =
  SpoNodeKeys
    { poolNodeKeysCold = defaultSpoColdKeyPair n
    , poolNodeKeysVrf =
      KeyPair
        { verificationKey = File $ defaultSpoKeysDir n </> "vrf.vkey"
        , signingKey = File $ defaultSpoKeysDir n </> "vrf.skey"
        }
    , poolNodeKeysStaking =
      KeyPair
        { verificationKey = File $ defaultSpoKeysDir n </> "staking-reward.vkey"
        , signingKey = File $ defaultSpoKeysDir n </> "staking-reward.skey"
        }
    }

-- | The relative path to stake delegator key pairs in directories created by cardano-testnet
defaultDelegatorStakeKeyPair :: Int -> KeyPair StakingKey
defaultDelegatorStakeKeyPair n =
  KeyPair
    { verificationKey = File $ "stake-delegators" </> ("delegator" <> show n) </> "staking.vkey"
    , signingKey = File $ "stake-delegators" </> ("delegator" <> show n) </> "staking.skey"
    }

-- | The relative path to UTXO keys
defaultUtxoKeys :: Int -> KeyPair PaymentKey
defaultUtxoKeys n =
  KeyPair
    { verificationKey = File $ "utxo-keys" </> "utxo" <> show n </> "utxo.vkey"
    , signingKey = File $ "utxo-keys" </> "utxo" <> show n </> "utxo.skey"
    }

-- | Default plutus script that always succeeds
plutusV3Script :: Text
plutusV3Script =
  "{ \"type\": \"PlutusScriptV3\", \"description\": \"\", \"cborHex\": \"46450101002499\" }"

plutusV3SupplementalDatumScript :: Text 
plutusV3SupplementalDatumScript = 
  "{ \"type\": \"PlutusScriptV3\", \"description\": \"\", \"cborHex\": \"590e41590e3e0100003232332232323232332232323232323232323225335333573466e1d2004301035353232325335333573466e1d200000200f00e13232323232333222123330010040030023232325335333573466e1d20000020170161323232323232323232323232323232323333333333332333233233222222222222222212333333333333333300101101000f00e00d00c00b00a00900800700600500400300230013574202860026ae8404cc0948c8c8c94cd4ccd5cd19b87480000080b40b04cc8848cc00400c008c074d5d080098029aba1357440022605c9201035054310035573c0046aae74004dd5000998128009aba101123232325335333573466e1d200000202c02b13232333322221233330010050040030023232325335333573466e1d2000002031030133221233001003002302e357420026605e4646464a66a666ae68cdc3a400000406a068264244600400660646ae8400454cd4ccd5cd19b87480080080d40d04c8ccc888488ccc00401401000cdd69aba1002375a6ae84004dd69aba1357440026ae880044c0d92401035054310035573c0046aae74004dd50009aba135744002260649201035054310035573c0046aae74004dd51aba1003300735742004646464a66a666ae68cdc3a4000004062060224440062a66a666ae68cdc3a4004004062060264244460020086eb8d5d08008a99a999ab9a3370e9002001018818099091118010021aba1001130324901035054310035573c0046aae74004dd51aba10013302c75c6ae84d5d10009aba2001357440022605a9201035054310035573c0046aae74004dd50009bad3574201e60026ae84038c008c009d69981180a9aba100c33302902475a6ae8402cc8c8c94cd4ccd5cd19b87480000080a80a44cc8848cc00400c008c8c8c94cd4ccd5cd19b87480000080b40b04cc8848cc00400c008cc09dd69aba10013026357426ae880044c0b9241035054310035573c0046aae74004dd51aba10013232325335333573466e1d200000202d02c1332212330010030023302775a6ae84004c098d5d09aba20011302e491035054310035573c0046aae74004dd51aba13574400226056921035054310035573c0046aae74004dd51aba100a3302375c6ae84024ccc0a48c8c8c94cd4ccd5cd19b87480000080ac0a84c84888888c01401cdd71aba100115335333573466e1d200200202b02a13212222223002007301b357420022a66a666ae68cdc3a40080040560542642444444600600e60506ae8400454cd4ccd5cd19b87480180080ac0a84cc884888888cc01802001cdd69aba10013019357426ae8800454cd4ccd5cd19b87480200080ac0a84c84888888c00401cc068d5d08008a99a999ab9a3370e9005001015815099910911111198020040039bad3574200260306ae84d5d1000898162481035054310035573c0046aae74004dd500080f9aba10083300201f3574200e6eb8d5d080319981480b198148111191919299a999ab9a3370e9000001015815089110010a99a999ab9a3370e9001001015815089110008a99a999ab9a3370e900200101581508911001898162481035054310035573c0046aae74004dd50009aba1005330230143574200860026ae8400cc004d5d09aba2003302475a604eeb8d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba200113018491035054310035573c0046aae74004dd51aba10063574200a646464a66a666ae68cdc3a400000402e02c2642444444600a00e6eb8d5d08008a99a999ab9a3370e900100100b80b0999109111111980100400398039aba10013301500f357426ae8800454cd4ccd5cd19b874801000805c0584c84888888c00c01cc050d5d08008a99a999ab9a3370e900300100b80b099910911111198030040039bad35742002600a6ae84d5d10008a99a999ab9a3370e900400100b80b0990911111180080398031aba100115335333573466e1d200a00201701613322122222233004008007375a6ae84004c010d5d09aba2001130184901035054310035573c0046aae74004dd51aba13574400a4646464a66a666ae68cdc3a400000402e02c264666444246660020080060046eb4d5d0801180a9aba10013232325335333573466e1d200000201b01a1323332221222222233300300a0090083301a017357420046ae84004cc069d71aba1357440026ae8800454cd4ccd5cd19b874800800806c0684cc8848888888cc01c024020cc064058d5d0800991919299a999ab9a3370e900000100f00e8999109198008018011bad357420026eb4d5d09aba20011301f491035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a400800403603426644244444446600401201066603a030eb4d5d08009980cbae357426ae8800454cd4ccd5cd19b874801800806c0684c848888888c010020cc064058d5d08008a99a999ab9a3370e900400100d80d09919199991110911111119998008058050048041980d80c1aba10033301901a3574200466603e034eb4d5d08009a991919299a999ab9a3370e900000101000f8998139bad357420026eb4d5d09aba2001130214901035054310035573c0046aae74004dd51aba135744002446602a0040026ae88004d5d10008a99a999ab9a3370e900500100d80d0999109111111198028048041980c80b1aba10013232325335333573466e1d200000201e01d13301c75c6ae840044c07d241035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a401800403603422444444400c26038921035054310035573c0046aae74004dd51aba1357440026ae880044c061241035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100b00a899910911111111111980280680618099aba10013301475a6ae84d5d10008a99a999ab9a3370e900100100b00a899910911111111111980100680618099aba10013301475a6ae84d5d10008a9919a999ab9a3370e900200180b80b0999109111111111119805006806180a1aba10023001357426ae8800854cd4ccd5cd19b874801800c05c0584c8ccc888488888888888ccc018038034030c054d5d080198011aba1001375a6ae84d5d10009aba200215335333573466e1d2008003017016133221222222222223300700d00c3014357420046eb4d5d09aba200215335333573466e1d200a003017016132122222222222300100c3014357420042a66a666ae68cdc3a401800602e02c266442444444444446600601a01860286ae84008dd69aba1357440042a66a666ae68cdc3a401c00602e02c266442444444444446601201a0186eb8d5d08011bae357426ae8800854cd4ccd5cd19b874804000c05c0584cc88488888888888cc020034030dd71aba1002375a6ae84d5d10010a99a999ab9a3370e900900180b80b0999109111111111119805806806180a1aba10023014357426ae8800854cd4ccd5cd19b874805000c05c0584c8488888888888c010030c050d5d08010980c2481035054310023232325335333573466e1d200000201a01913212223003004375c6ae8400454c8cd4ccd5cd19b874800800c06c0684c84888c004010c004d5d08010a99a999ab9a3370e900200180d80d099910911198010028021bae3574200460026ae84d5d10010980e2481035054310023232325335333573466e1d200000201e01d13212223003004301b357420022a66a666ae68cdc3a400400403c03a224440042a66a666ae68cdc3a400800403c03a224440022603e921035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d00137540024646464a66a666ae68cdc3a400000402a0282642444600600860246ae8400454cd4ccd5cd19b87480080080540504c84888c008010c048d5d08008a99a999ab9a3370e900200100a80a099091118008021bae357420022602c921035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100a0098999109198008018011bae357420026eb4d5d09aba200113015491035054310035573c0046aae74004dd50009aba200113010491035054310035573c0046aae74004dd500091100191111111111111110030060058a4c2601a9201035054350030122225335333573466e1d200000100d00c1300e491035054330015335333573466e20005200000d00c13300333702900000119b81480000044c8cc8848cc00400c008cdc200180099b840020013300400200130112225335333573466e1d200000100c00b10021330030013370c00400240024646464a66a666ae68cdc3a400000401601420142a66a666ae68cdc3a40040040160142016260189201035054310035573c0046aae74004dd500091191919299a999ab9a3370e9000001005805089110010a99a999ab9a3370e90010010058050990911180180218029aba100115335333573466e1d200400200b00a112220011300c4901035054310035573c0046aae74004dd50009191919299a999ab9a3370e90000010048040999109198008018011bae357420026eb4d5d09aba20011300a491035054310035573c0046aae74004dd5000919118011bac001300d2233335573e002401c466a01a60086ae84008c00cd5d10010051191919299a999ab9a3370e900000100380309909118010019bae357420022a66a666ae68cdc3a400400400e00c26424460020066eb8d5d0800898042481035054310035573c0046aae74004dd500091191919299a999ab9a3370e900100100380308910008a99a999ab9a3370e9000001003803099091180100198029aba1001130084901035054310035573c0046aae74004dd5000891001091000919319ab9c00100322322300237560026010446666aae7c00480248c8cd4024cc02cc018d55ce80098029aab9e0013004357440066ae8400801448004c010894cd400452000221350022233700900118030018910010910911980080200191091980080180111918008009180111980100100081\" }"


