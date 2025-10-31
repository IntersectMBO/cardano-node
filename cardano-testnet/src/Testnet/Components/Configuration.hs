{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Configuration
  ( createConfigJson
  , createConfigJsonNoHash
  , createSPOGenesisAndFiles
  , mkTopologyConfig
  , numSeededUTxOKeys

  , getByronGenesisHash
  , getShelleyGenesisHash
  , getDefaultAlonzoGenesis
  , getDefaultShelleyGenesis
  , startTimeOffsetSeconds

  , anyEraToString
  , eraToString
  ) where

import           Cardano.Api hiding (Value, cardanoEra)
import           Cardano.Api.Ledger (AlonzoGenesis, ConwayGenesis)

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.BaseTypes (unsafeNonZero)
import           Cardano.Network.PeerSelection.Bootstrap
import           Cardano.Network.PeerSelection.PeerTrustable
import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Node.Protocol.Byron
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers

import           Control.Exception
import           Control.Monad
import           Control.Monad.Extra
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Aeson.Key hiding (fromString)
import           Data.Aeson.KeyMap hiding (map)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import qualified Network.HTTP.Simple as HTTP
import           RIO ( MonadThrow, throwM)
import qualified System.Directory as System
import           System.FilePath.Posix (takeDirectory, (</>))


import           Testnet.Blockfrost (blockfrostToGenesis)
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Process.RunIO (execCli_, liftIOAnnotated)
import           Testnet.Start.Types

import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.Time as DTC

-- | Returns JSON encoded hashes of the era, as well as the hard fork configuration toggle.
createConfigJson :: ()
  => HasCallStack
  => MonadIO m
  => MonadThrow m
  => TmpAbsolutePath
  -> ShelleyBasedEra era -- ^ The era used for generating the hard fork configuration toggle
  -> m (KeyMap Aeson.Value)
createConfigJson (TmpAbsolutePath tempAbsPath) sbe = GHC.withFrozenCallStack $ do
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath </> "byron-genesis.json"
  shelleyGenesisHash <- getHash ShelleyEra "ShelleyGenesisHash"
  alonzoGenesisHash  <- getHash AlonzoEra  "AlonzoGenesisHash"
  conwayGenesisHash  <- getHash ConwayEra  "ConwayGenesisHash"

  pure $ mconcat
    [ byronGenesisHash
    , shelleyGenesisHash
    , alonzoGenesisHash
    , conwayGenesisHash
    , Defaults.defaultYamlHardforkViaConfig sbe
    ]
   where
    getHash ::  MonadIO m => CardanoEra a -> Text.Text -> m (KeyMap Value)
    getHash e = getShelleyGenesisHash (tempAbsPath </> Defaults.defaultGenesisFilepath e)

createConfigJsonNoHash :: ()
  => ShelleyBasedEra era -- ^ The era used for generating the hard fork configuration toggle
  -> KeyMap Aeson.Value
createConfigJsonNoHash = Defaults.defaultYamlHardforkViaConfig

-- Generate hashes for genesis.json files

getByronGenesisHash
  :: MonadIO m
  => MonadThrow m
  => FilePath
  -> m (KeyMap Aeson.Value)
getByronGenesisHash path = do
  e <- runExceptT $ readGenesisData path
  case e of 
    Left err -> throwM $ GenesisReadError path err
    Right (_, genesisHash) -> do
      let genesisHash' = unGenesisHash genesisHash
      pure . singleton "ByronGenesisHash" $ toJSON genesisHash'

getShelleyGenesisHash
  :: MonadIO m
  => FilePath
  -> Text
  -> m (KeyMap Aeson.Value)
getShelleyGenesisHash path key = do
  content <- liftIOAnnotated $ BS.readFile path
  let genesisHash = Crypto.hashWith id content :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString
  pure . singleton (fromText key) $ toJSON genesisHash

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

-- | A start time and 'ShelleyGenesis' value that are fit to pass to 'cardanoTestnet'
getDefaultShelleyGenesis :: ()
  => MonadIO m
  => AnyShelleyBasedEra
  -> Word64 -- ^ The max supply
  -> GenesisOptions
  -> m ShelleyGenesis
getDefaultShelleyGenesis asbe maxSupply opts = do
  currentTime <- liftIOAnnotated DTC.getCurrentTime
  let startTime = DTC.addUTCTime startTimeOffsetSeconds currentTime
  return $ Defaults.defaultShelleyGenesis asbe startTime maxSupply opts

-- | An 'AlonzoGenesis' value that is fit to pass to 'cardanoTestnet'
getDefaultAlonzoGenesis :: ()
  => HasCallStack
  => MonadThrow m
  => ShelleyBasedEra era
  -> m AlonzoGenesis
getDefaultAlonzoGenesis sbe =
  case Defaults.defaultAlonzoGenesis sbe of 
    Right genesis -> return genesis
    Left err -> throwM err


numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

createSPOGenesisAndFiles
  :: MonadIO m
  => HasCallStack
  => MonadThrow m
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> TestnetOnChainParams
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles
  testnetOptions genesisOptions@GenesisOptions{genesisTestnetMagic}
  onChainParams
  (TmpAbsolutePath tempAbsPath) =  do
  AnyShelleyBasedEra sbe <- pure cardanoNodeEra

  let genesisShelleyDirAbs = takeDirectory inputGenesisShelleyFp

  genesisShelleyDir <- liftIOAnnotated $ System.createDirectoryIfMissing True genesisShelleyDirAbs >> pure genesisShelleyDirAbs
  let -- At least there should be a delegator per DRep
      -- otherwise some won't be representing anybody
      numStakeDelegators = max 3 (fromIntegral cardanoNumDReps) :: Int

  shelleyGenesis'' <- getDefaultShelleyGenesis cardanoNodeEra cardanoMaxSupply genesisOptions
  -- TODO: Remove this rewrite.
  -- 50 second epochs
  -- Epoch length should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
  let shelleyGenesis' = shelleyGenesis''
        { sgSecurityParam = unsafeNonZero 5
        , sgUpdateQuorum = 2
        }
  alonzoGenesis' <- getDefaultAlonzoGenesis sbe
  let conwayGenesis' = Defaults.defaultConwayGenesis

  (alonzoGenesis, conwayGenesis, shelleyGenesis)   
     <- resolveOnChainParams onChainParams
     (alonzoGenesis', conwayGenesis', shelleyGenesis')

  -- Write Genesis files to disk, so they can be picked up by create-testnet-data
  liftIOAnnotated $ do
    LBS.writeFile inputGenesisAlonzoFp $ A.encodePretty alonzoGenesis
    LBS.writeFile inputGenesisConwayFp $ A.encodePretty conwayGenesis
    LBS.writeFile inputGenesisShelleyFp $ A.encodePretty shelleyGenesis

  let era = toCardanoEra sbe

  currentTime <- liftIOAnnotated DTC.getCurrentTime
  let startTime = DTC.addUTCTime startTimeOffsetSeconds currentTime

  execCli_ $
    [ eraToString sbe, "genesis", "create-testnet-data" ]
    ++ createTestnetDataFlag ShelleyEra
    ++ createTestnetDataFlag AlonzoEra
    ++ createTestnetDataFlag ConwayEra
    ++
    [ "--testnet-magic", show genesisTestnetMagic
    , "--pools", show nPoolNodes
    , "--total-supply",     show cardanoMaxSupply -- Half of this will be delegated, see https://github.com/IntersectMBO/cardano-cli/pull/874
    , "--stake-delegators", show numStakeDelegators
    , "--utxo-keys", show numSeededUTxOKeys]
    <> monoidForEraInEon @ConwayEraOnwards era (const ["--drep-keys", show cardanoNumDReps])
    <> [ "--start-time", DTC.formatIso8601 startTime
    , "--out-dir", tempAbsPath
    ]

  -- Remove the input files. We don't need them anymore, since create-testnet-data wrote new versions.
  forM_
    [  inputGenesisShelleyFp, inputGenesisAlonzoFp, inputGenesisConwayFp
     , tempAbsPath </> "byron.genesis.spec.json" -- Created by create-testnet-data
    ]
    (\fp -> liftIO $ whenM (System.doesFileExist fp) (System.removeFile fp))

  return genesisShelleyDir
  where
    inputGenesisShelleyFp = genesisInputFilepath ShelleyEra
    inputGenesisAlonzoFp  = genesisInputFilepath AlonzoEra
    inputGenesisConwayFp  = genesisInputFilepath ConwayEra
    nPoolNodes = cardanoNumPools testnetOptions
    CardanoTestnetOptions{cardanoNodeEra, cardanoMaxSupply, cardanoNumDReps} = testnetOptions
    genesisInputFilepath :: Pretty (eon era) => eon era -> FilePath
    genesisInputFilepath e = tempAbsPath </> ("genesis-input." <> eraToString e <> ".json")
    createTestnetDataFlag :: Pretty (eon era) => eon era -> [String]
    createTestnetDataFlag sbe =
        ["--spec-" ++ eraToString sbe, genesisInputFilepath sbe]

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- TODO: Reconcile all other mkTopologyConfig functions. NB: We only intend
-- to support current era on mainnet and the upcoming era.
mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> LBS.ByteString
mkTopologyConfig numNodes allPorts port False = A.encodePretty topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology NonP2P.RemoteAddress
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numNodes - 1)
        | peerPort <- allPorts List.\\ [port]
        ]
mkTopologyConfig numNodes allPorts port True = A.encodePretty topologyP2P
  where
    rootConfig :: P2P.RootConfig RelayAccessPoint
    rootConfig =
      P2P.RootConfig
        [ RelayAccessAddress (fromString ifaceAddress)
                             (fromIntegral peerPort)
        | peerPort <- allPorts List.\\ [port]
        ]
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups RelayAccessPoint
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (HotValency (numNodes - 1))
                                  (WarmValency (numNodes - 1))
                                  IsNotTrustable
                                  InitiatorAndResponderDiffusionMode
        ]

    topologyP2P :: P2P.NetworkTopology RelayAccessPoint
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        DontUseLedgerPeers
        DontUseBootstrapPeers
        Nothing


data BlockfrostParamsError = BlockfrostParamsDecodeError FilePath String
  deriving Show

instance Exception BlockfrostParamsError where 
  displayException (BlockfrostParamsDecodeError fp err) =
    "Failed to decode Blockfrost on-chain parameters from file "
      <> fp
      <> ": "
      <> err

-- | Resolves different kinds of user-provided on-chain parameters
-- into a unified, consistent set of Genesis files
resolveOnChainParams :: ()
 => HasCallStack
 => MonadIO m
 => MonadThrow m
 => TestnetOnChainParams
 -> (AlonzoGenesis, ConwayGenesis, ShelleyGenesis)
 -> m (AlonzoGenesis, ConwayGenesis, ShelleyGenesis)
resolveOnChainParams onChainParams geneses = case onChainParams of

  DefaultParams -> do 
    pure geneses

  OnChainParamsFile file -> do
    eParams <- eitherDecode <$> liftIOAnnotated (LBS.readFile file)
    case eParams of 
      Right params -> pure $ blockfrostToGenesis geneses params
      Left err -> throwM $ BlockfrostParamsDecodeError file err

  OnChainParamsMainnet -> do
    mainnetParams <- liftIOAnnotated $ HTTP.getResponseBody <$> HTTP.httpJSON mainnetParamsRequest
    pure $ blockfrostToGenesis geneses mainnetParams
