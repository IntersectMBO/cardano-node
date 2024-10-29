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
  "{ \"type\": \"PlutusScriptV3\", \"description\": \"\", \"cborHex\": \"590e59590e560100003233223322323232323232323232323232323232253353010300e35353232325335333573466e1d200000201501413232323232333222123330010040030023232325335333573466e1d200000201d01c1323232323232323232323232323232323333333333332333233233222222222222222212333333333333333300101101000f00e00d00c00b00a00900800700600500400300230013574202860026ae8404cc0948c8c8c94cd4ccd5cd19b87480000080cc0c84cc8848cc00400c008c074d5d080098029aba135744002260589201035054310035573c0046aae74004dd5000998128009aba101123232325335333573466e1d200000203203113232333322221233330010050040030023232325335333573466e1d2000002037036133221233001003002302e357420026605e4646464a66a666ae68cdc3a4000004076074264244600400660646ae8400454cd4ccd5cd19b87480080080ec0e84c8ccc888488ccc00401401000cdd69aba1002375a6ae84004dd69aba1357440026ae880044c0d12401035054310035573c0046aae74004dd50009aba135744002260609201035054310035573c0046aae74004dd51aba1003300735742004646464a66a666ae68cdc3a400000406e06c224440062a66a666ae68cdc3a400400406e06c264244460020086eb8d5d08008a99a999ab9a3370e900200101b81b099091118010021aba1001130304901035054310035573c0046aae74004dd51aba10013302c75c6ae84d5d10009aba200135744002260569201035054310035573c0046aae74004dd50009bad3574201e60026ae84038c008c009d69981180a9aba100c33302702475a6ae8402cc8c8c94cd4ccd5cd19b87480000080c00bc4cc8848cc00400c008c8c8c94cd4ccd5cd19b87480000080cc0c84cc8848cc00400c008cc09dd69aba10013026357426ae880044c0b1241035054310035573c0046aae74004dd51aba10013232325335333573466e1d20000020330321332212330010030023302775a6ae84004c098d5d09aba20011302c491035054310035573c0046aae74004dd51aba13574400226052921035054310035573c0046aae74004dd51aba100a3302375c6ae84024ccc09c8c8c8c94cd4ccd5cd19b87480000080c40c04c84888888c01401cdd71aba100115335333573466e1d200200203103013212222223002007301b357420022a66a666ae68cdc3a40080040620602642444444600600e60506ae8400454cd4ccd5cd19b87480180080c40c04cc884888888cc01802001cdd69aba10013019357426ae8800454cd4ccd5cd19b87480200080c40c04c84888888c00401cc068d5d08008a99a999ab9a3370e9005001018818099910911111198020040039bad3574200260306ae84d5d1000898152481035054310035573c0046aae74004dd500080f9aba10083300201f3574200e6eb8d5d080319981380b198138111191919299a999ab9a3370e9000001018818089110010a99a999ab9a3370e9001001018818089110008a99a999ab9a3370e900200101881808911001898152481035054310035573c0046aae74004dd50009aba1005330230143574200860026ae8400cc004d5d09aba2003302475a604aeb8d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba200113016491035054310035573c0046aae74004dd51aba10063574200a646464a66a666ae68cdc3a400000403a0382642444444600a00e6eb8d5d08008a99a999ab9a3370e900100100e80e0999109111111980100400398039aba10013301500f357426ae8800454cd4ccd5cd19b87480100080740704c84888888c00c01cc050d5d08008a99a999ab9a3370e900300100e80e099910911111198030040039bad35742002600a6ae84d5d10008a99a999ab9a3370e900400100e80e0990911111180080398031aba100115335333573466e1d200a00201d01c13322122222233004008007375a6ae84004c010d5d09aba2001130164901035054310035573c0046aae74004dd51aba13574400a4646464a66a666ae68cdc3a400000403a038264666444246660020080060046eb4d5d0801180a9aba10013232325335333573466e1d20000020210201323332221222222233300300a0090083301a017357420046ae84004cc069d71aba1357440026ae8800454cd4ccd5cd19b87480080080840804cc8848888888cc01c024020cc064058d5d0800991919299a999ab9a3370e90000010120118999109198008018011bad357420026eb4d5d09aba20011301d491035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a4008004042040266442444444466004012010666036030eb4d5d08009980cbae357426ae8800454cd4ccd5cd19b87480180080840804c848888888c010020cc064058d5d08008a99a999ab9a3370e900400101081009919199991110911111119998008058050048041980d80c1aba10033301901a3574200466603a034eb4d5d08009a991919299a999ab9a3370e90000010130128998119bad357420026eb4d5d09aba20011301f4901035054310035573c0046aae74004dd51aba135744002446602a0040026ae88004d5d10008a99a999ab9a3370e90050010108100999109111111198028048041980c80b1aba10013232325335333573466e1d200000202402313301c75c6ae840044c075241035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a401800404204022444444400c26034921035054310035573c0046aae74004dd51aba1357440026ae880044c059241035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100e00d899910911111111111980280680618099aba10013301475a6ae84d5d10008a99a999ab9a3370e900100100e00d899910911111111111980100680618099aba10013301475a6ae84d5d10008a9919a999ab9a3370e900200180e80e0999109111111111119805006806180a1aba10023001357426ae8800854cd4ccd5cd19b874801800c0740704c8ccc888488888888888ccc018038034030c054d5d080198011aba1001375a6ae84d5d10009aba200215335333573466e1d200800301d01c133221222222222223300700d00c3014357420046eb4d5d09aba200215335333573466e1d200a00301d01c132122222222222300100c3014357420042a66a666ae68cdc3a401800603a038266442444444444446600601a01860286ae84008dd69aba1357440042a66a666ae68cdc3a401c00603a038266442444444444446601201a0186eb8d5d08011bae357426ae8800854cd4ccd5cd19b874804000c0740704cc88488888888888cc020034030dd71aba1002375a6ae84d5d10010a99a999ab9a3370e900900180e80e0999109111111111119805806806180a1aba10023014357426ae8800854cd4ccd5cd19b874805000c0740704c8488888888888c010030c050d5d08010980b2481035054310023232325335333573466e1d200000202001f13212223003004375c6ae8400454c8cd4ccd5cd19b874800800c0840804c84888c004010c004d5d08010a99a999ab9a3370e9002001810810099910911198010028021bae3574200460026ae84d5d10010980d2481035054310023232325335333573466e1d200000202402313212223003004301b357420022a66a666ae68cdc3a4004004048046224440042a66a666ae68cdc3a4008004048046224440022603a921035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d00137540024646464a66a666ae68cdc3a40000040360342642444600600860246ae8400454cd4ccd5cd19b874800800806c0684c84888c008010c048d5d08008a99a999ab9a3370e900200100d80d099091118008021bae3574200226028921035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100d00c8999109198008018011bae357420026eb4d5d09aba200113013491035054310035573c0046aae74004dd50009aba20011300e491035054310035573c0046aae74004dd500091100191111111111111110030a4c260169201035054350030132225335333573466e1d20000010130121300c491035054330015335333573466e20005200001301213300333702900000119b81480000044c8cc8848cc00400c008cdc200180099b840020013300400200130122225335333573466e1d200000101201110021330030013370c00400240024646464a66a666ae68cdc3a400000402202020202a66a666ae68cdc3a40040040220202022260149201035054310035573c0046aae74004dd500091191919299a999ab9a3370e9000001008808089110010a99a999ab9a3370e90010010088080990911180180218029aba100115335333573466e1d2004002011010112220011300a4901035054310035573c0046aae74004dd50009191919299a999ab9a3370e90000010078070999109198008018011bae357420026eb4d5d09aba200113008491035054310035573c0046aae74004dd5000919118011bac001300e2233335573e0024020466a01e60086ae84008c00cd5d10010041191919299a999ab9a3370e900000100680609909118010019bae357420022a66a666ae68cdc3a400400401a01826424460020066eb8d5d0800898032481035054310035573c0046aae74004dd500091191919299a999ab9a3370e900100100680608910008a99a999ab9a3370e9000001006806099091180100198029aba1001130064901035054310035573c0046aae74004dd5000919319ab9c00100322322300237560026016446666aae7c00480348c8cd4034cc024c018d55ce80098029aab9e0013004357440066ae8400801448004c01c894cd40044020884cd4024d400888004c0100048848cc00400c008c014894cd4004400c8854cd4ccd5cd19baf4c01010100002006005100613004001122002122001122002122122330010040032323001001230022330020020011\" }"


