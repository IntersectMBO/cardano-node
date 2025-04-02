{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Configuration
  ( createConfigJson
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

import           Cardano.Api.Ledger (AlonzoGenesis, ConwayGenesis)
import           Cardano.Api.Shelley hiding (Value, cardanoEra)

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Network.PeerSelection.Bootstrap
import           Cardano.Network.PeerSelection.PeerTrustable
import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.Extra
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Aeson.Key hiding (fromString)
import           Data.Aeson.KeyMap hiding (map)
import qualified Data.Aeson.Lens as L
import           Data.Bifunctor (first)
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
import           Lens.Micro
import qualified System.Directory as System
import           System.FilePath.Posix (takeDirectory, (</>))

import           Testnet.Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli_)
import           Testnet.Start.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.Time as DTC
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | Returns JSON encoded hashes of the era, as well as the hard fork configuration toggle.
createConfigJson :: ()
  => (MonadTest m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> ShelleyBasedEra era -- ^ The era used for generating the hard fork configuration toggle
  -> m LBS.ByteString
createConfigJson (TmpAbsolutePath tempAbsPath) sbe = GHC.withFrozenCallStack $ do
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath </> "byron-genesis.json"
  shelleyGenesisHash <- getHash ShelleyEra "ShelleyGenesisHash"
  alonzoGenesisHash  <- getHash AlonzoEra  "AlonzoGenesisHash"
  conwayGenesisHash  <- getHash ConwayEra  "ConwayGenesisHash"

  pure . A.encodePretty . Object
    $ mconcat [ byronGenesisHash
              , shelleyGenesisHash
              , alonzoGenesisHash
              , conwayGenesisHash
              , defaultYamlHardforkViaConfig sbe
              ]
   where
    getHash :: (MonadTest m, MonadIO m) => CardanoEra a -> Text.Text -> m (KeyMap Value)
    getHash e = getShelleyGenesisHash (tempAbsPath </> defaultGenesisFilepath e)


-- Generate hashes for genesis.json files

getByronGenesisHash
  :: (H.MonadTest m, MonadIO m)
  => FilePath
  -> m (KeyMap Aeson.Value)
getByronGenesisHash path = do
  e <- runExceptT $ readGenesisData path
  (_, genesisHash) <- H.leftFail e
  let genesisHash' = unGenesisHash genesisHash
  pure . singleton "ByronGenesisHash" $ toJSON genesisHash'

getShelleyGenesisHash
  :: (H.MonadTest m, MonadIO m)
  => FilePath
  -> Text
  -> m (KeyMap Aeson.Value)
getShelleyGenesisHash path key = do
  content <- H.evalIO  $ BS.readFile path
  let genesisHash = Crypto.hashWith id content :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString
  pure . singleton (fromText key) $ toJSON genesisHash

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

-- | A start time and 'ShelleyGenesis' value that are fit to pass to 'cardanoTestnet'
getDefaultShelleyGenesis :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => AnyShelleyBasedEra
  -> Word64 -- ^ The max supply
  -> GenesisOptions
  -> m ShelleyGenesis
getDefaultShelleyGenesis asbe maxSupply opts = do
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime
  return $ defaultShelleyGenesis asbe startTime maxSupply opts

-- | An 'AlonzoGenesis' value that is fit to pass to 'cardanoTestnet'
getDefaultAlonzoGenesis :: ()
  => HasCallStack
  => MonadTest m
  => ShelleyBasedEra era
  -> m AlonzoGenesis
getDefaultAlonzoGenesis sbe =
  H.evalEither $ first prettyError (defaultAlonzoGenesis sbe)

numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

createSPOGenesisAndFiles
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> UserProvidedData ShelleyGenesis
  -> UserProvidedData AlonzoGenesis
  -> UserProvidedData ConwayGenesis
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles
  testnetOptions genesisOptions@GenesisOptions{genesisTestnetMagic}
  mShelleyGenesis mAlonzoGenesis mConwayGenesis
  (TmpAbsolutePath tempAbsPath) = GHC.withFrozenCallStack $ do
  AnyShelleyBasedEra sbe <- pure cardanoNodeEra

  let genesisShelleyDirAbs = takeDirectory inputGenesisShelleyFp
  genesisShelleyDir <- H.createDirectoryIfMissing genesisShelleyDirAbs
  let -- At least there should be a delegator per DRep
      -- otherwise some won't be representing anybody
      numStakeDelegators = max 3 (fromIntegral cardanoNumDReps) :: Int

  -- When the user did not specify a genesis file, we write a default one to disk,
  -- and then pass it to create-testnet-data (which will amend it).
  -- When the user specifies a genesis file, we overwrite the default genesis file
  -- generated by create-testnet-data with the user's file
  -- (we still need to call create-testnet-data to generate keys)
  generateGenesisIfAbsent ShelleyEra mShelleyGenesis =<< getDefaultShelleyGenesis cardanoNodeEra cardanoMaxSupply genesisOptions
  generateGenesisIfAbsent AlonzoEra mAlonzoGenesis =<< getDefaultAlonzoGenesis sbe
  generateGenesisIfAbsent ConwayEra mConwayGenesis defaultConwayGenesis

  case mShelleyGenesis of
    UserProvidedData _ -> pure () -- Don't touch the user's file
    NoUserProvidedData -> do
      -- TODO: Remove this rewrite.
      -- 50 second epochs
      -- Epoch length should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
      H.rewriteJsonFile @Value inputGenesisShelleyFp $ \o -> o
        & L.key "securityParam" . L._Integer .~ 5
        & L.key "updateQuorum" . L._Integer .~ 2

  -- TODO: create-testnet-data should have arguments for
  -- Alonzo and Conway genesis that are optional and if not
  -- supplised the users get a default
  H.note_ $ "Number of pools: " <> show nPoolNodes
  H.note_ $ "Number of stake delegators: " <> show nPoolNodes
  H.note_ $ "Number of seeded UTxO keys: " <> show numSeededUTxOKeys

  let era = toCardanoEra sbe
  startTime <-
    case mShelleyGenesis of
      UserProvidedData shelleyGenesis ->
        pure $ sgSystemStart shelleyGenesis
      NoUserProvidedData -> do
        currentTime <- H.noteShowIO DTC.getCurrentTime
        H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  execCli_ $
    [ eraToString sbe, "genesis", "create-testnet-data" ]
    ++ userGenesisToCreateTestnetDataFlag ShelleyEra mShelleyGenesis
    ++ userGenesisToCreateTestnetDataFlag AlonzoEra mAlonzoGenesis
    ++ userGenesisToCreateTestnetDataFlag ConwayEra mConwayGenesis
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

  -- Overwrite the genesis files created by create-testnet-data with the files
  -- specified by the user (if any)
  liftIO $ do
    overwriteCreateTestnetDataGenesis ShelleyEra mShelleyGenesis
    overwriteCreateTestnetDataGenesis AlonzoEra mAlonzoGenesis
    overwriteCreateTestnetDataGenesis ConwayEra mConwayGenesis

  -- Remove the input files. We don't need them anymore, since create-testnet-data wrote new versions.
  forM_
    [  inputGenesisShelleyFp, inputGenesisAlonzoFp, inputGenesisConwayFp
     , tempAbsPath </> "byron.genesis.spec.json" -- Created by create-testnet-data
    ]
    (\fp -> liftIO $ whenM (System.doesFileExist fp) (System.removeFile fp))

  files <- H.listDirectory tempAbsPath
  forM_ files H.note

  return genesisShelleyDir
  where
    inputGenesisShelleyFp = genesisInputFilepath ShelleyEra
    inputGenesisAlonzoFp  = genesisInputFilepath AlonzoEra
    inputGenesisConwayFp  = genesisInputFilepath ConwayEra
    nPoolNodes = cardanoNumPools testnetOptions
    CardanoTestnetOptions{cardanoNodeEra, cardanoMaxSupply, cardanoNumDReps} = testnetOptions
    genesisInputFilepath :: Pretty (eon era) => eon era -> FilePath
    genesisInputFilepath e = tempAbsPath </> ("genesis-input." <> eraToString e <> ".json")
    generateGenesisIfAbsent ::
      Pretty (eon era) => MonadTest m => MonadIO m => ToJSON b =>
      eon era -> UserProvidedData a -> b -> m ()
    generateGenesisIfAbsent sbe userData genesis =
      case userData of
        UserProvidedData _ ->
          -- No need to give the genesis file to create-testnet-data. We use the user's one
          pure ()
        NoUserProvidedData -> do
          -- Let's generate a genesis file. It is used as a template by create-testnet-data
          liftIO $ LBS.writeFile (genesisInputFilepath sbe) $ A.encodePretty genesis
    userGenesisToCreateTestnetDataFlag ::
      Pretty (eon era) =>
      eon era -> UserProvidedData a -> [String]
    userGenesisToCreateTestnetDataFlag sbe = \case
      UserProvidedData _ ->
        -- Genesis file is provided by the user, so we don't need to pass a template one
        -- to create-testnet-data (we anyway overwrite the genesis file
        -- created by create-testnet-data in this case, see below)
        []
      NoUserProvidedData ->
        -- We're in control, so we pass our template to create-testnet-data,
        -- which will amend it.
        ["--spec-" ++ eraToString sbe, genesisInputFilepath sbe]
    -- Overwrites the genesis file created by create-testnet-data with the one provided by the user
    overwriteCreateTestnetDataGenesis ::
      Pretty (eon era) => ToJSON a =>
      eon era -> UserProvidedData a -> IO ()
    overwriteCreateTestnetDataGenesis e = \case
      UserProvidedData a ->
        LBS.writeFile (tempAbsPath </> eraToString e <> "-genesis.json") $ A.encodePretty a
      NoUserProvidedData ->
        pure ()

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- TODO: Reconcile all other mkTopologyConfig functions. NB: We only intend
-- to support current era on mainnet and the upcoming era.
mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> LBS.ByteString
mkTopologyConfig numNodes allPorts port False = A.encodePretty topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numNodes - 1)
        | peerPort <- allPorts List.\\ [port]
        ]
mkTopologyConfig numNodes allPorts port True = A.encodePretty topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        [ RelayAccessAddress (fromString ifaceAddress)
                             (fromIntegral peerPort)
        | peerPort <- allPorts List.\\ [port]
        ]
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (HotValency (numNodes - 1))
                                  (WarmValency (numNodes - 1))
                                  IsNotTrustable
                                  InitiatorAndResponderDiffusionMode
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        DontUseLedgerPeers
        DontUseBootstrapPeers
        Nothing
