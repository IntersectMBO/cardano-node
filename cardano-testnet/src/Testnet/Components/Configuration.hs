{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Configuration
  ( writeConfiguration
  , refreshConfigurationHashes
  , createSPOGenesisAndFiles
  , numSeededUTxOKeys

  , getDefaultAlonzoGenesis
  , getDefaultShelleyGenesis
  , startTimeOffsetSeconds

  , anyEraToString
  , eraToString
  ) where

import           Cardano.Api hiding (Value, cardanoEra)
import           Cardano.Api.Ledger (AlonzoGenesis, ConwayGenesis)

import           Cardano.Configuration.Edit (configurationDocumentFromValue,
                   readConfigurationDocument, refreshGenesisHashes, writeConfigurationDocument)
import           Cardano.Ledger.BaseTypes (unsafeNonZero)
import           Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis)

import           Control.Exception
import           Control.Monad
import           Control.Monad.Extra
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as DTC
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import qualified Network.HTTP.Simple as HTTP
import qualified System.Directory as System
import           System.FilePath.Posix (takeDirectory, (</>))

import           Testnet.Blockfrost (blockfrostToGenesis)
import qualified Testnet.Defaults as Defaults
import           Testnet.Filepath
import           Testnet.Process.RunIO (execCli_, liftIOAnnotated)
import           Testnet.Start.Types

import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.Time as DTC

import           RIO (MonadThrow, throwM)

-- | Write the node configuration of a testnet: the default configuration for the
-- given era, with the hashes of the genesis files in the same directory as the
-- configuration file. The file is written in the @cardano-config@ format.
writeConfiguration :: ()
  => HasCallStack
  => MonadIO m
  => FilePath -- ^ The configuration file to write
  -> ShelleyBasedEra era -- ^ The era used for generating the hard fork configuration toggle
  -> m ()
writeConfiguration configFile sbe = GHC.withFrozenCallStack . liftIOAnnotated $ do
  (document, _warnings) <- configurationDocumentFromValue . Object $ Defaults.defaultYamlHardforkViaConfig sbe
  refreshGenesisHashes (takeDirectory configFile) document >>= writeConfigurationDocument configFile

-- | Set the genesis hashes in a node configuration file to the hashes of the
-- genesis files it names, adding the missing ones and replacing the ones that
-- are out of date.
refreshConfigurationHashes :: ()
  => HasCallStack
  => MonadIO m
  => FilePath -- ^ The configuration file
  -> m ()
refreshConfigurationHashes configFile = GHC.withFrozenCallStack . liftIOAnnotated $ do
  (document, _warnings) <- readConfigurationDocument configFile
  refreshGenesisHashes (takeDirectory configFile) document >>= writeConfigurationDocument configFile

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: Int
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
  let startTime = DTC.addUTCTime (fromIntegral startTimeOffsetSeconds) currentTime
  return $ Defaults.defaultShelleyGenesis asbe startTime maxSupply opts

-- | An 'AlonzoGenesis' value that is fit to pass to 'cardanoTestnet'
getDefaultAlonzoGenesis :: ()
  => HasCallStack
  => MonadThrow m
  => m AlonzoGenesis
getDefaultAlonzoGenesis =
  case Defaults.defaultAlonzoGenesis of
    Right genesis -> return genesis
    Left err -> throwM err


numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

createSPOGenesisAndFiles
  :: MonadIO m
  => HasCallStack
  => MonadThrow m
  => TestnetCreationOptions
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles
  creationOptions@TestnetCreationOptions
    { creationGenesisOptions=genesisOptions@GenesisOptions{genesisTestnetMagic}
    , creationOnChainParams=onChainParams
    }
  (TmpAbsolutePath tempAbsPath) =  do
  AnyShelleyBasedEra sbe <- pure creationEra


  let genesisShelleyDir = takeDirectory inputGenesisShelleyFp

  liftIOAnnotated $ System.createDirectoryIfMissing True genesisShelleyDir

  let -- At least there should be a delegator per DRep
      -- otherwise some won't be representing anybody
      numStakeDelegators = max 3 (fromIntegral creationNumDReps) :: Int

  shelleyGenesis'' <- getDefaultShelleyGenesis creationEra creationMaxSupply genesisOptions
  -- TODO: Remove this rewrite.
  -- 50 second epochs
  -- Epoch length should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
  let shelleyGenesis' = shelleyGenesis''
        { sgSecurityParam = unsafeNonZero 5
        , sgUpdateQuorum = 2
        }
  alonzoGenesis' <- getDefaultAlonzoGenesis
  let conwayGenesis' = Defaults.defaultConwayGenesis
      dijkstraGenesis' = Defaults.defaultDijkstraGenesis

  (shelleyGenesis, alonzoGenesis, conwayGenesis, dijkstraGenesis)
     <- resolveOnChainParams onChainParams
     (shelleyGenesis', alonzoGenesis', conwayGenesis', dijkstraGenesis')

  -- Write Genesis files to disk, so they can be picked up by create-testnet-data
  liftIOAnnotated $ do
    LBS.writeFile inputGenesisAlonzoFp $ A.encodePretty alonzoGenesis
    LBS.writeFile inputGenesisConwayFp $ A.encodePretty conwayGenesis
    LBS.writeFile inputGenesisShelleyFp $ A.encodePretty shelleyGenesis
    LBS.writeFile inputGenesisDijkstraFp $ A.encodePretty dijkstraGenesis
  let era = toCardanoEra sbe

  currentTime <- liftIOAnnotated DTC.getCurrentTime
  let startTime = DTC.addUTCTime (fromIntegral startTimeOffsetSeconds) currentTime

  execCli_ $
    [ eraToString sbe, "genesis", "create-testnet-data" ]
    ++ createTestnetDataFlag ShelleyEra
    ++ createTestnetDataFlag AlonzoEra
    ++ createTestnetDataFlag ConwayEra
    ++ createTestnetDataFlag DijkstraEra
    ++
    [ "--testnet-magic", show genesisTestnetMagic
    , "--pools", show nPoolNodes
    , "--total-supply",     show creationMaxSupply -- Half of this will be delegated, see https://github.com/IntersectMBO/cardano-cli/pull/874
    , "--stake-delegators", show numStakeDelegators
    , "--utxo-keys", show numSeededUTxOKeys]
    <> monoidForEraInEon @ConwayEraOnwards era (const ["--drep-keys", show creationNumDReps])
    <> [ "--start-time", DTC.formatIso8601 startTime
    , "--out-dir", tempAbsPath
    ]

  -- Remove the input files. We don't need them anymore, since create-testnet-data wrote new versions.
  forM_
    [  inputGenesisShelleyFp, inputGenesisAlonzoFp, inputGenesisConwayFp, inputGenesisDijkstraFp
     , tempAbsPath </> "byron.genesis.spec.json" -- Created by create-testnet-data
    ]
    (\fp -> liftIOAnnotated $ whenM (System.doesFileExist fp) (System.removeFile fp))

  return genesisShelleyDir
  where
    inputGenesisShelleyFp = genesisInputFilepath ShelleyEra
    inputGenesisAlonzoFp  = genesisInputFilepath AlonzoEra
    inputGenesisConwayFp  = genesisInputFilepath ConwayEra
    inputGenesisDijkstraFp  = genesisInputFilepath DijkstraEra
    nPoolNodes = creationNumPools creationOptions
    TestnetCreationOptions{creationEra, creationMaxSupply, creationNumDReps} = creationOptions
    genesisInputFilepath :: Pretty (eon era) => eon era -> FilePath
    genesisInputFilepath e = tempAbsPath </> ("genesis-input." <> eraToString e <> ".json")
    createTestnetDataFlag :: Pretty (eon era) => eon era -> [String]
    createTestnetDataFlag sbe =
        ["--spec-" ++ eraToString sbe, genesisInputFilepath sbe]



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
 -> (ShelleyGenesis, AlonzoGenesis, ConwayGenesis, DijkstraGenesis)
 -> m (ShelleyGenesis, AlonzoGenesis, ConwayGenesis, DijkstraGenesis)
resolveOnChainParams onChainParams geneses = case onChainParams of

  DefaultParams -> do
    pure geneses

  OnChainParamsFile file -> do
    eParams <- eitherDecode <$> liftIOAnnotated (LBS.readFile file)
    case eParams of
      Right params -> blockfrostToGenesis geneses params
      Left err -> throwM $ BlockfrostParamsDecodeError file err

  OnChainParamsMainnet -> do
    mainnetParams <- liftIOAnnotated $ HTTP.getResponseBody <$> HTTP.httpJSON mainnetParamsRequest
    blockfrostToGenesis geneses mainnetParams
