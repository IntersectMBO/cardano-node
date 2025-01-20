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
  , plutusV2StakeScript
  ) where

import           Cardano.Api (AnyShelleyBasedEra (..), CardanoEra (..), File (..),
                   ShelleyBasedEra (..), pshow, toCardanoEra, unsafeBoundedRational)
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
import           GHC.Exts (IsList (..))
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
    <> fromList [("TraceOptions", Aeson.Object mempty)]
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
    , ("ByronGenesisFile", genesisPath ByronEra)
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
  -> KeyPair StakePoolKey
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
defaultDelegatorStakeKeyPair :: Int -> KeyPair StakeKey
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

-- | Created via: cabal run plutus-scripts-bench -- print SupplementalDatum -o supplemental-datum.plutus
plutusV3SupplementalDatumScript :: Text
plutusV3SupplementalDatumScript =
  "{ \"type\": \"PlutusScriptV3\", \"description\": \"\", \"cborHex\": \"590e72590e6f01000032323322332233223232323232323232323232323225335533535353232325335333573466e1d200000201301213232323232333222123330010040030023232325335333573466e1d200000201b01a1323232323232323232323232323232323333333333332333233233222222222222222212333333333333333300101101000f00e00d00c00b00a00900800700600500400300230013574202860026ae8404cc0948c8c8c94cd4ccd5cd19b87480000080c40c04cc8848cc00400c008c074d5d080098029aba135744002260589201035054310035573c0046aae74004dd5000998128009aba101123232325335333573466e1d200000203002f13232333322221233330010050040030023232325335333573466e1d2000002035034133221233001003002302e357420026605e4646464a66a666ae68cdc3a4000004072070264244600400660646ae8400454cd4ccd5cd19b87480080080e40e04c8ccc888488ccc00401401000cdd69aba1002375a6ae84004dd69aba1357440026ae880044c0d12401035054310035573c0046aae74004dd50009aba135744002260609201035054310035573c0046aae74004dd51aba1003300735742004646464a66a666ae68cdc3a400000406a068224440062a66a666ae68cdc3a400400406a068264244460020086eb8d5d08008a99a999ab9a3370e900200101a81a099091118010021aba1001130304901035054310035573c0046aae74004dd51aba10013302c75c6ae84d5d10009aba200135744002260569201035054310035573c0046aae74004dd50009bad3574201e60026ae84038c008c009d69981180a9aba100c33302702475a6ae8402cc8c8c94cd4ccd5cd19b87480000080b80b44cc8848cc00400c008c8c8c94cd4ccd5cd19b87480000080c40c04cc8848cc00400c008cc09dd69aba10013026357426ae880044c0b1241035054310035573c0046aae74004dd51aba10013232325335333573466e1d20000020310301332212330010030023302775a6ae84004c098d5d09aba20011302c491035054310035573c0046aae74004dd51aba13574400226052921035054310035573c0046aae74004dd51aba100a3302375c6ae84024ccc09c8c8c8c94cd4ccd5cd19b87480000080bc0b84c84888888c01401cdd71aba100115335333573466e1d200200202f02e13212222223002007301b357420022a66a666ae68cdc3a400800405e05c2642444444600600e60506ae8400454cd4ccd5cd19b87480180080bc0b84cc884888888cc01802001cdd69aba10013019357426ae8800454cd4ccd5cd19b87480200080bc0b84c84888888c00401cc068d5d08008a99a999ab9a3370e9005001017817099910911111198020040039bad3574200260306ae84d5d1000898152481035054310035573c0046aae74004dd500080f9aba10083300201f3574200e6eb8d5d080319981380b198138111191919299a999ab9a3370e9000001017817089110010a99a999ab9a3370e9001001017817089110008a99a999ab9a3370e900200101781708911001898152481035054310035573c0046aae74004dd50009aba1005330230143574200860026ae8400cc004d5d09aba2003302475a604aeb8d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba200113016491035054310035573c0046aae74004dd51aba10063574200a646464a66a666ae68cdc3a40000040360342642444444600a00e6eb8d5d08008a99a999ab9a3370e900100100d80d0999109111111980100400398039aba10013301500f357426ae8800454cd4ccd5cd19b874801000806c0684c84888888c00c01cc050d5d08008a99a999ab9a3370e900300100d80d099910911111198030040039bad35742002600a6ae84d5d10008a99a999ab9a3370e900400100d80d0990911111180080398031aba100115335333573466e1d200a00201b01a13322122222233004008007375a6ae84004c010d5d09aba2001130164901035054310035573c0046aae74004dd51aba13574400a4646464a66a666ae68cdc3a4000004036034264666444246660020080060046eb4d5d0801180a9aba10013232325335333573466e1d200000201f01e1323332221222222233300300a0090083301a017357420046ae84004cc069d71aba1357440026ae8800454cd4ccd5cd19b874800800807c0784cc8848888888cc01c024020cc064058d5d0800991919299a999ab9a3370e90000010110108999109198008018011bad357420026eb4d5d09aba20011301d491035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a400800403e03c266442444444466004012010666036030eb4d5d08009980cbae357426ae8800454cd4ccd5cd19b874801800807c0784c848888888c010020cc064058d5d08008a99a999ab9a3370e900400100f80f09919199991110911111119998008058050048041980d80c1aba10033301901a3574200466603a034eb4d5d08009a991919299a999ab9a3370e90000010120118998149bad357420026eb4d5d09aba20011301f4901035054310035573c0046aae74004dd51aba135744002446602a0040026ae88004d5d10008a99a999ab9a3370e900500100f80f0999109111111198028048041980c80b1aba10013232325335333573466e1d200000202202113301c75c6ae840044c075241035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a401800403e03c22444444400c26034921035054310035573c0046aae74004dd51aba1357440026ae880044c059241035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100d00c899910911111111111980280680618099aba10013301475a6ae84d5d10008a99a999ab9a3370e900100100d00c899910911111111111980100680618099aba10013301475a6ae84d5d10008a9919a999ab9a3370e900200180d80d0999109111111111119805006806180a1aba10023001357426ae8800854cd4ccd5cd19b874801800c06c0684c8ccc888488888888888ccc018038034030c054d5d080198011aba1001375a6ae84d5d10009aba200215335333573466e1d200800301b01a133221222222222223300700d00c3014357420046eb4d5d09aba200215335333573466e1d200a00301b01a132122222222222300100c3014357420042a66a666ae68cdc3a4018006036034266442444444444446600601a01860286ae84008dd69aba1357440042a66a666ae68cdc3a401c006036034266442444444444446601201a0186eb8d5d08011bae357426ae8800854cd4ccd5cd19b874804000c06c0684cc88488888888888cc020034030dd71aba1002375a6ae84d5d10010a99a999ab9a3370e900900180d80d0999109111111111119805806806180a1aba10023014357426ae8800854cd4ccd5cd19b874805000c06c0684c8488888888888c010030c050d5d08010980b2481035054310023232325335333573466e1d200000201e01d13212223003004375c6ae8400454c8cd4ccd5cd19b874800800c07c0784c84888c004010c004d5d08010a99a999ab9a3370e900200180f80f099910911198010028021bae3574200460026ae84d5d10010980d2481035054310023232325335333573466e1d200000202202113212223003004301b357420022a66a666ae68cdc3a4004004044042224440042a66a666ae68cdc3a4008004044042224440022603a921035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d00137540024646464a66a666ae68cdc3a40000040320302642444600600860246ae8400454cd4ccd5cd19b87480080080640604c84888c008010c048d5d08008a99a999ab9a3370e900200100c80c099091118008021bae3574200226028921035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100c00b8999109198008018011bae357420026eb4d5d09aba200113013491035054310035573c0046aae74004dd50009aba20011300e491035054310035573c0046aae74004dd50009110019111111111111111180f0031080888078a4c26016921035054350030142225335333573466e1d20000010110101300c491035054330015335333573466e20005200001101013300333702900000119b81480000044c8cc8848cc00400c008cdc200180099b840020013300400200130132225335333573466e1d200000101000f10021330030013370c00400240024646464a66a666ae68cdc3a400000401e01c201c2a66a666ae68cdc3a400400401e01c201e260149201035054310035573c0046aae74004dd500091191919299a999ab9a3370e9000001007807089110010a99a999ab9a3370e90010010078070990911180180218029aba100115335333573466e1d200400200f00e112220011300a4901035054310035573c0046aae74004dd50009191919299a999ab9a3370e90000010068060999109198008018011bae357420026eb4d5d09aba200113008491035054310035573c0046aae74004dd5000919118011bac001300f2233335573e002401c466a01a60086ae84008c00cd5d10010041191919299a999ab9a3370e900000100580509909118010019bae357420022a66a666ae68cdc3a400400401601426424460020066eb8d5d0800898032481035054310035573c0046aae74004dd500091191919299a999ab9a3370e90010010058050a8070a99a999ab9a3370e90000010058050980798029aba1001130064901035054310035573c0046aae74004dd5000919319ab9c00100322322300237560026018446666aae7c004802c8c8cd402ccc03cc018d55ce80098029aab9e0013004357440066ae8400801448004c020894cd40045401c884d4008894cd4ccd5cd19b8f488120ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25000020080071300c001130060031220021220011220021221223300100400321223002003112200122123300100300223230010012300223300200200101\" }"


plutusV2StakeScript :: Text
plutusV2StakeScript =
    "{ \"type\": \"PlutusScriptV2\", \"description\": \"\", \"cborHex\": \"5907655907620100003232323232323232323232323232332232323232322232325335320193333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4050054d5d0a80619a80a00a9aba1500b33501401635742a014666aa030eb9405cd5d0a804999aa80c3ae501735742a01066a02803e6ae85401cccd54060081d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40a9d69aba15002302b357426ae8940088c98c80b4cd5ce01701681589aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8153ad35742a00460566ae84d5d1280111931901699ab9c02e02d02b135573ca00226ea8004d5d09aba2500223263202933573805405204e26aae7940044dd50009aba1500533501475c6ae854010ccd540600708004d5d0a801999aa80c3ae200135742a004603c6ae84d5d1280111931901299ab9c026025023135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a004601c6ae84d5d1280111931900b99ab9c018017015101613263201633573892010350543500016135573ca00226ea800448c88c008dd6000990009aa80a911999aab9f0012500a233500930043574200460066ae880080508c8c8cccd5cd19b8735573aa004900011991091980080180118061aba150023005357426ae8940088c98c8050cd5ce00a80a00909aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500f014357426ae8940088c98c8064cd5ce00d00c80b89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403666ae7007006c06406005c4d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201533573802c02a02626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355012223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301213574200222440042442446600200800624464646666ae68cdc3a800a40004642446004006600a6ae84d55cf280191999ab9a3370ea0049001109100091931900819ab9c01101000e00d135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01101000e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00d00c00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00580500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700340300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002c02802001c0184d55cea80089baa0012323333573466e1d40052002200723333573466e1d40092000212200123263200633573800e00c00800626aae74dd5000a4c2400292010350543100122002112323001001223300330020020011\" }"

