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
import           Cardano.Node.Protocol.Byron

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
import           Testnet.Process.RunIO (execCli_, getCliHelpText, liftIOAnnotated)
import           Testnet.Start.Byron (ByronGenesisOptions (..), byronDefaultGenesisOptions,
                   createByronGenesis)
import           Testnet.Start.Types

import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Stock.Time as DTC

import           Data.List (isInfixOf)
import           Data.Maybe (fromMaybe)
import           Data.Monoid.Extra (mwhen)
import           RIO (MonadThrow, throwM)


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
  alonzoGenesisHash <- getHash AlonzoEra "AlonzoGenesisHash"
  conwayGenesisHash <- getHash ConwayEra "ConwayGenesisHash"
  dijkstraGenesisHash <- getHash DijkstraEra "DijkstraGenesisHash"

  pure $ mconcat
    [ byronGenesisHash
    , shelleyGenesisHash
    , alonzoGenesisHash
    , conwayGenesisHash
    , dijkstraGenesisHash
    , Defaults.defaultYamlHardforkViaConfig sbe
    ]
   where
    getHash :: MonadIO m => CardanoEra a -> Text.Text -> m (KeyMap Value)
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
-- MacOS. We need to allow a lot more time to set up a testnet.
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
  => CardanoTestnetOptions -- ^ The options to use
  -> GenesisOptions
  -> TestnetOnChainParams
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles
  testnetOptions genesisOptions@GenesisOptions{genesisTestnetMagic}
  onChainParams
  (TmpAbsolutePath tempAbsPath) = do
  AnyShelleyBasedEra sbe <- pure cardanoNodeEra


  let genesisShelleyDir = takeDirectory inputGenesisShelleyFp

  liftIOAnnotated $ System.createDirectoryIfMissing True genesisShelleyDir

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

  -- Probe which --spec-* flags the installed cardano-cli supports so that
  -- we only pass flags it understands. To add a new era, just append a
  -- line below — the rest adapts automatically.
  let cliHelp = fromMaybe "" <$>
        getCliHelpText [eraToString sbe, "genesis", "create-testnet-data"]
  cliHas <- flip isInfixOf <$> cliHelp

  execCli_ $
    [ eraToString sbe, "genesis", "create-testnet-data" ]
    ++ specFlag cliHas ShelleyEra
    ++ specFlag cliHas AlonzoEra
    ++ specFlag cliHas ConwayEra
    ++ specFlag cliHas DijkstraEra
    ++
    [ "--testnet-magic", show genesisTestnetMagic
    , "--pools", show nPoolNodes
    , "--total-supply", show cardanoMaxSupply -- Half of this will be delegated, see https://github.com/IntersectMBO/cardano-cli/pull/874
    , "--stake-delegators", show numStakeDelegators
    , "--utxo-keys", show numSeededUTxOKeys]
    <> monoidForEraInEon @ConwayEraOnwards era (const ["--drep-keys", show cardanoNumDReps])
    <> [ "--start-time", DTC.formatIso8601 startTime
    , "--out-dir", tempAbsPath
    ]

  -- Older cardano-cli versions do not generate byron-genesis.json from
  -- create-testnet-data.  Create it via the legacy byron command when missing.
  let byronGenesisPath = tempAbsPath </> Defaults.defaultGenesisFilepath ByronEra
  unlessFileExists byronGenesisPath $ do
    let byronGenDir = tempAbsPath </> "byron-gen-command"
        byronParamsFp = tempAbsPath </> "byron-params.json"
        byronOpts = byronDefaultGenesisOptions
          { byronNumBftNodes = fromIntegral nPoolNodes }
    liftIOAnnotated $ LBS.writeFile byronParamsFp $ Aeson.encode Defaults.defaultByronProtocolParamsJsonValue
    createByronGenesis genesisTestnetMagic startTime byronOpts byronParamsFp byronGenDir
    liftIOAnnotated $ do
      System.renameFile (byronGenDir </> "genesis.json") byronGenesisPath
      forM_ [1..nPoolNodes] $ \i -> do
        let ii = fromIntegral i :: Int
            poolKeysDir = tempAbsPath </> Defaults.defaultSpoKeysDir ii
            padIdx = let s = show (ii - 1) in replicate (3 - length s) '0' ++ s
        System.copyFile (byronGenDir </> ("delegate-keys." ++ padIdx ++ ".key")) (poolKeysDir </> "byron-delegate.key")
        System.copyFile (byronGenDir </> ("delegation-cert." ++ padIdx ++ ".json")) (poolKeysDir </> "byron-delegation.cert")
      System.removeFile byronParamsFp

  -- For eras whose --spec-* flag was not supported, write a default genesis
  -- file so that the node configuration and hash computation stay uniform.
  let dijkstraGenesisPath = tempAbsPath </> Defaults.defaultGenesisFilepath DijkstraEra
  unlessFileExists dijkstraGenesisPath $
    liftIOAnnotated $ LBS.writeFile dijkstraGenesisPath $ A.encodePretty dijkstraGenesis

  -- Remove the input files. We don't need them anymore, since create-testnet-data wrote new versions.
  forM_
    [  inputGenesisShelleyFp, inputGenesisAlonzoFp, inputGenesisConwayFp
     , inputGenesisDijkstraFp
     , tempAbsPath </> "byron.genesis.spec.json" -- Created by create-testnet-data
    ]
    (\fp -> liftIOAnnotated $ whenM (System.doesFileExist fp) (System.removeFile fp))

  return genesisShelleyDir
  where
    inputGenesisShelleyFp = genesisInputFilepath ShelleyEra
    inputGenesisAlonzoFp = genesisInputFilepath AlonzoEra
    inputGenesisConwayFp = genesisInputFilepath ConwayEra
    inputGenesisDijkstraFp = genesisInputFilepath DijkstraEra

    nPoolNodes = cardanoNumPools testnetOptions

    CardanoTestnetOptions{cardanoNodeEra, cardanoMaxSupply, cardanoNumDReps} = testnetOptions

    genesisInputFilepath :: Pretty (eon era) => eon era -> FilePath
    genesisInputFilepath e = tempAbsPath </> ("genesis-input." <> eraToString e <> ".json")

    specFlag :: Pretty (eon era) => (String -> Bool) -> eon era -> [String]
    specFlag supported e =
        let flag = "--spec-" ++ eraToString e
        in mwhen (supported flag) [flag, genesisInputFilepath e]

    unlessFileExists fp act = do
      exists <- liftIOAnnotated $ System.doesFileExist fp
      unless exists act



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
