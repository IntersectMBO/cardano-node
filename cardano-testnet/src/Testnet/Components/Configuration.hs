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
import           Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis)

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Control.Monad.Extra
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Aeson.Key hiding (fromString)
import           Data.Aeson.KeyMap hiding (map)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
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
  -> m (KeyMap Aeson.Value)
createConfigJson (TmpAbsolutePath tempAbsPath) sbe = GHC.withFrozenCallStack $ do
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath </> "byron-genesis.json"
  shelleyGenesisHash <- getHash ShelleyEra "ShelleyGenesisHash"
  alonzoGenesisHash  <- getHash AlonzoEra  "AlonzoGenesisHash"
  conwayGenesisHash  <- getHash ConwayEra  "ConwayGenesisHash"
  dijkstraGenesisHash  <- getHash DijkstraEra  "DijkstraGenesisHash"

  pure $ mconcat
    [ byronGenesisHash
    , shelleyGenesisHash
    , alonzoGenesisHash
    , conwayGenesisHash
    , dijkstraGenesisHash
    , Defaults.defaultYamlHardforkViaConfig sbe
    ]
   where
    getHash :: (MonadTest m, MonadIO m) => CardanoEra a -> Text.Text -> m (KeyMap Value)
    getHash e = getShelleyGenesisHash (tempAbsPath </> Defaults.defaultGenesisFilepath e)

createConfigJsonNoHash :: ()
  => ShelleyBasedEra era -- ^ The era used for generating the hard fork configuration toggle
  -> KeyMap Aeson.Value
createConfigJsonNoHash = Defaults.defaultYamlHardforkViaConfig

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
  return $ Defaults.defaultShelleyGenesis asbe startTime maxSupply opts

-- | An 'AlonzoGenesis' value that is fit to pass to 'cardanoTestnet'
getDefaultAlonzoGenesis :: ()
  => HasCallStack
  => MonadTest m
  => m AlonzoGenesis
getDefaultAlonzoGenesis =
  H.evalEither $ first prettyError Defaults.defaultAlonzoGenesis

numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

createSPOGenesisAndFiles
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> TestnetOnChainParams
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles
  testnetOptions genesisOptions@GenesisOptions{genesisTestnetMagic}
  onChainParams
  (TmpAbsolutePath tempAbsPath) = GHC.withFrozenCallStack $ do
  AnyShelleyBasedEra sbe <- pure cardanoNodeEra

  let genesisShelleyDirAbs = takeDirectory inputGenesisShelleyFp
  genesisShelleyDir <- H.createDirectoryIfMissing genesisShelleyDirAbs
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
  alonzoGenesis' <- getDefaultAlonzoGenesis
  let conwayGenesis' = Defaults.defaultConwayGenesis
      dijkstraGenesis' = dijkstraGenesisDefaults

  (shelleyGenesis, alonzoGenesis, conwayGenesis, dijkstraGenesis) <- resolveOnChainParams onChainParams
    (shelleyGenesis', alonzoGenesis', conwayGenesis', dijkstraGenesis')

  -- Write Genesis files to disk, so they can be picked up by create-testnet-data
  H.lbsWriteFile inputGenesisAlonzoFp $ A.encodePretty alonzoGenesis
  H.lbsWriteFile inputGenesisConwayFp $ A.encodePretty conwayGenesis
  H.lbsWriteFile inputGenesisShelleyFp $ A.encodePretty shelleyGenesis
  H.lbsWriteFile inputGenesisDijkstraFp $ A.encodePretty dijkstraGenesis

  H.note_ $ "Number of pools: " <> show nPoolNodes
  H.note_ $ "Number of stake delegators: " <> show numStakeDelegators
  H.note_ $ "Number of seeded UTxO keys: " <> show numSeededUTxOKeys

  let era = toCardanoEra sbe

  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

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

  files <- H.listDirectory tempAbsPath
  forM_ files H.note

  return genesisShelleyDir
  where
    inputGenesisShelleyFp = genesisInputFilepath ShelleyEra
    inputGenesisAlonzoFp  = genesisInputFilepath AlonzoEra
    inputGenesisConwayFp  = genesisInputFilepath ConwayEra
    inputGenesisDijkstraFp  = genesisInputFilepath DijkstraEra
    nPoolNodes = cardanoNumPools testnetOptions
    CardanoTestnetOptions{cardanoNodeEra, cardanoMaxSupply, cardanoNumDReps} = testnetOptions
    genesisInputFilepath :: Pretty (eon era) => eon era -> FilePath
    genesisInputFilepath e = tempAbsPath </> ("genesis-input." <> eraToString e <> ".json")
    createTestnetDataFlag :: Pretty (eon era) => eon era -> [String]
    createTestnetDataFlag sbe =
        ["--spec-" ++ eraToString sbe, genesisInputFilepath sbe]

-- | Resolves different kinds of user-provided on-chain parameters
-- into a unified, consistent set of Genesis files
resolveOnChainParams :: ()
 => (MonadTest m, MonadIO m)
 => HasCallStack
 => TestnetOnChainParams
 -> (ShelleyGenesis, AlonzoGenesis, ConwayGenesis, DijkstraGenesis)
 -> m (ShelleyGenesis, AlonzoGenesis, ConwayGenesis, DijkstraGenesis)
resolveOnChainParams onChainParams geneses = case onChainParams of

  DefaultParams -> pure geneses

  OnChainParamsFile file -> do
    eParams <- H.readJsonFile file
    params <- H.leftFail eParams
    pure $ blockfrostToGenesis geneses params

  OnChainParamsMainnet -> do
    mainnetParams <- H.evalIO $ HTTP.getResponseBody <$> HTTP.httpJSON mainnetParamsRequest
    pure $ blockfrostToGenesis geneses mainnetParams
