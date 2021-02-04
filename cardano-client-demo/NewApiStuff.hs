{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module NewApiStuff
  ( LedgerStateVar(..)
  , initialLedgerState
  -- , TODO apply block
  )
  where

import           Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml

import qualified Cardano.Api.Block
import qualified Cardano.BM.Configuration.Model as BM
import qualified Cardano.BM.Data.Configuration as BM
import qualified Cardano.Chain.Genesis
import qualified Cardano.Chain.Genesis as Cardano.Chain.Genesis.Config
import qualified Cardano.Chain.UTxO
import qualified Cardano.Chain.Update
import qualified Cardano.Crypto
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson.Types as Data.Aeson.Types.Internal
import           Data.Word
import           GHC.Conc
import qualified Ouroboros.Consensus.Byron.Ledger.Block
import qualified Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as C
import qualified Ouroboros.Consensus.Cardano.Block
import qualified Ouroboros.Consensus.Cardano.Block as C
import qualified Ouroboros.Consensus.Cardano.CanHardFork
import qualified Ouroboros.Consensus.Cardano.Node
import qualified Ouroboros.Consensus.Config as C
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.Ledger.Extended as C
import qualified Ouroboros.Consensus.Node.ProtocolInfo
import qualified Ouroboros.Consensus.Shelley.Eras
import qualified Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Shelley.Spec.Ledger.BaseTypes
import qualified Shelley.Spec.Ledger.Genesis
import qualified Shelley.Spec.Ledger.PParams
import           System.FilePath

-- Bring it all together and make the initial ledger state
initialLedgerState
  :: FilePath -- Path to the db-sync config file
  -> IO LedgerStateVar
initialLedgerState dbSyncConfFilePath = do
  dbSyncConf <- readDbSyncNodeConfig (ConfigFile dbSyncConfFilePath)
  genConf <- fmap (either (error . Text.unpack . renderDbSyncNodeError) id) $ runExceptT (readCardanoGenesisConfig dbSyncConf)
  initLedgerStateVar genConf

--------------------------------------------------------------------------------
-- Everything below this is just coppied from db-sync                         --
--------------------------------------------------------------------------------

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

readDbSyncNodeConfig :: ConfigFile -> IO DbSyncNodeConfig
readDbSyncNodeConfig (ConfigFile fp) = do
    pcfg <- adjustNodeFilePath . parseDbSyncPreConfig <$> readByteString fp "DbSync"
    ncfg <- parseNodeConfig <$> readByteString (pcNodeConfigFilePath pcfg) "node"
    coalesceConfig pcfg ncfg (mkAdjustPath pcfg)
  where
    parseDbSyncPreConfig :: ByteString -> DbSyncPreConfig
    parseDbSyncPreConfig bs =
      case Yaml.decodeEither' bs of
      Left err -> error . Text.unpack $ "readDbSyncNodeConfig: Error parsing config: " <> textShow err
      Right res -> res

    adjustNodeFilePath :: DbSyncPreConfig -> DbSyncPreConfig
    adjustNodeFilePath cfg =
      cfg { pcNodeConfigFile = adjustNodeConfigFilePath (takeDirectory fp </>) (pcNodeConfigFile cfg) }


adjustNodeConfigFilePath :: (FilePath -> FilePath) -> NodeConfigFile -> NodeConfigFile
adjustNodeConfigFilePath f (NodeConfigFile p) = NodeConfigFile (f p)

pcNodeConfigFilePath :: DbSyncPreConfig -> FilePath
pcNodeConfigFilePath = unNodeConfigFile . pcNodeConfigFile

data NodeConfig = NodeConfig
  { ncProtocol :: !DbSyncProtocol
  , ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  , ncByronSotfwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , ncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  -- Shelley hardfok parameters
  , ncShelleyHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , ncByronToShelley :: !ByronToShelley

  -- Allegra hardfok parameters
  , ncAllegraHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , ncShelleyToAllegra :: !ShelleyToAllegra

  -- Mary hardfok parameters
  , ncMaryHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , ncAllegraToMary :: !AllegraToMary
  }


instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Data.Aeson.Types.Internal.Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .: "Protocol"
          <*> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o

          <*> parseShelleyHardForkEpoch o
          <*> (Ouroboros.Consensus.Cardano.Node.ProtocolParamsTransition <$> parseShelleyHardForkEpoch o)

          <*> parseAllegraHardForkEpoch o
          <*> (Ouroboros.Consensus.Cardano.Node.ProtocolParamsTransition <$> parseAllegraHardForkEpoch o)

          <*> parseMaryHardForkEpoch o
          <*> (Ouroboros.Consensus.Cardano.Node.ProtocolParamsTransition <$> parseMaryHardForkEpoch o)

      parseByronProtocolVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.ProtocolVersion
      parseByronProtocolVersion o =
        Cardano.Chain.Update.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseByronSoftwareVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.SoftwareVersion
      parseByronSoftwareVersion o =
        Cardano.Chain.Update.SoftwareVersion
          <$> fmap Cardano.Chain.Update.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseShelleyHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
      parseShelleyHardForkEpoch o =
        asum
          [ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseMaryHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
      parseMaryHardForkEpoch o =
        asum
          [ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure $ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

parseNodeConfig :: ByteString -> NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> error . Text.unpack $ "Error parsing node config: " <> textShow err
    Right nc -> nc

coalesceConfig
    :: DbSyncPreConfig -> NodeConfig -> (FilePath -> FilePath)
    -> IO DbSyncNodeConfig
coalesceConfig pcfg ncfg adjustGenesisPath = do
  lc <- BM.setupFromRepresentation $ pcLoggingConfig pcfg
  pure $ DbSyncNodeConfig
          { dncNetworkName = pcNetworkName pcfg
          , dncLoggingConfig = lc
          , dncNodeConfigFile = pcNodeConfigFile pcfg
          , dncProtocol = ncProtocol ncfg
          , dncRequiresNetworkMagic = ncRequiresNetworkMagic ncfg
          , dncEnableLogging = pcEnableLogging pcfg
          , dncEnableMetrics = pcEnableMetrics pcfg
          , dncPBftSignatureThreshold = ncPBftSignatureThreshold ncfg
          , dncByronGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncByronGenesisFile ncfg)
          , dncByronGenesisHash = ncByronGenesisHash ncfg
          , dncShelleyGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncShelleyGenesisFile ncfg)
          , dncShelleyGenesisHash = ncShelleyGenesisHash ncfg
          , dncByronSoftwareVersion = ncByronSotfwareVersion ncfg
          , dncByronProtocolVersion = ncByronProtocolVersion ncfg

          , dncShelleyHardFork = ncShelleyHardFork ncfg
          , dncAllegraHardFork = ncAllegraHardFork ncfg
          , dncMaryHardFork = ncMaryHardFork ncfg

          , dncByronToShelley = ncByronToShelley ncfg
          , dncShelleyToAllegra = ncShelleyToAllegra ncfg
          , dncAllegraToMary = ncAllegraToMary ncfg
          }

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

mkAdjustPath :: DbSyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp

readByteString :: FilePath -> Text -> IO ByteString
readByteString fp cfgType =
  catch (BS.readFile fp) $ \(_ :: IOException) ->
    error . Text.unpack $ mconcat [ "Cannot find the ", cfgType, " configuration file at : ", Text.pack fp ]


initLedgerStateVar :: GenesisConfig -> IO LedgerStateVar
initLedgerStateVar genesisConfig =
  fmap LedgerStateVar . newTVarIO $
    CardanoLedgerState
      { clsState = Ouroboros.Consensus.Node.ProtocolInfo.pInfoInitLedger protocolInfo
      , clsConfig = Ouroboros.Consensus.Node.ProtocolInfo.pInfoConfig protocolInfo
      }
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(C.ExtLedgerState (C.CardanoBlock C.StandardCrypto))
  , clsConfig :: !(C.TopLevelConfig (C.CardanoBlock C.StandardCrypto))
  }

newtype LedgerStateVar = LedgerStateVar
  { unLedgerStateVar :: TVar CardanoLedgerState
  }

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !DbSyncNodeConfig !Cardano.Chain.Genesis.Config !ShelleyConfig

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Shelley.Spec.Ledger.Genesis.ShelleyGenesis Ouroboros.Consensus.Shelley.Eras.StandardShelley)
  , scGenesisHash :: !GenesisHashShelley
  }


data DbSyncNodeConfig = DbSyncNodeConfig
  { dncNetworkName :: !NetworkName
  , dncLoggingConfig :: !BM.Configuration
  , dncNodeConfigFile :: !NodeConfigFile
  , dncProtocol :: !DbSyncProtocol
  , dncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  , dncEnableLogging :: !Bool
  , dncEnableMetrics :: !Bool
  , dncPBftSignatureThreshold :: !(Maybe Double)
  , dncByronGenesisFile :: !GenesisFile
  , dncByronGenesisHash :: !GenesisHashByron
  , dncShelleyGenesisFile :: !GenesisFile
  , dncShelleyGenesisHash :: !GenesisHashShelley
  , dncByronSoftwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , dncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  , dncShelleyHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , dncAllegraHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , dncMaryHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork

  , dncByronToShelley :: !ByronToShelley
  , dncShelleyToAllegra :: !ShelleyToAllegra
  , dncAllegraToMary :: !AllegraToMary
  }

-- May have other constructors when we are preparing for a HFC event.
data DbSyncProtocol
  = DbSyncProtocolCardano
  deriving Show

instance FromJSON DbSyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure DbSyncProtocolCardano
      x -> Data.Aeson.Types.Internal.typeMismatch "Protocol" x

type ByronToShelley =
  C.ProtocolParamsTransition Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)

type ShelleyToAllegra =
  C.ProtocolParamsTransition
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)

type AllegraToMary =
  C.ProtocolParamsTransition
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary)

data DbSyncPreConfig = DbSyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !BM.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  }

instance FromJSON DbSyncPreConfig where
  parseJSON o =
    Aeson.withObject "top-level" parseGenDbSyncNodeConfig o


parseGenDbSyncNodeConfig :: Object -> Data.Aeson.Types.Internal.Parser DbSyncPreConfig
parseGenDbSyncNodeConfig o =
  DbSyncPreConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> fmap NodeConfigFile (o .: "NodeConfigFile")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype LogFileDir
  = LogFileDir FilePath

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NodeConfigFile = NodeConfigFile
  { unNodeConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

mkProtocolInfoCardano :: GenesisConfig -> Ouroboros.Consensus.Node.ProtocolInfo.ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano = Ouroboros.Consensus.Cardano.protocolInfo . mkProtocolCardano

type CardanoBlock =
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
            (Ouroboros.Consensus.Cardano.Block.CardanoEras C.StandardCrypto)

mkProtocolCardano :: GenesisConfig -> C.Protocol m CardanoBlock CardanoProtocol
mkProtocolCardano ge =
  case ge of
    GenesisCardano dnc byronGenesis shelleyGenesis ->
        C.ProtocolCardano
          C.ProtocolParamsByron
            { C.byronGenesis = byronGenesis
            , C.byronPbftSignatureThreshold = C.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , C.byronProtocolVersion = dncByronProtocolVersion dnc
            , C.byronSoftwareVersion = dncByronSoftwareVersion dnc
            , C.byronLeaderCredentials = Nothing
            }
          C.ProtocolParamsShelleyBased
            { C.shelleyBasedGenesis = scConfig shelleyGenesis
            , C.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , C.shelleyBasedLeaderCredentials = []
            }
          C.ProtocolParamsShelley
            { C.shelleyProtVer = shelleyProtVer dnc
            }
          C.ProtocolParamsAllegra
            { C.allegraProtVer = shelleyProtVer dnc
            }
          C.ProtocolParamsMary
            { C.maryProtVer = shelleyProtVer dnc
            }
          (dncByronToShelley dnc)
          (dncShelleyToAllegra dnc)
          (dncAllegraToMary dnc)

shelleyPraosNonce :: ShelleyConfig -> Shelley.Spec.Ledger.BaseTypes.Nonce
shelleyPraosNonce sCfg = Shelley.Spec.Ledger.BaseTypes.Nonce (Cardano.Crypto.Hash.Class.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: DbSyncNodeConfig -> Shelley.Spec.Ledger.PParams.ProtVer
shelleyProtVer dnc =
  let bver = dncByronProtocolVersion dnc in
  Shelley.Spec.Ledger.PParams.ProtVer
    (fromIntegral $ Cardano.Chain.Update.pvMajor bver)
    (fromIntegral $ Cardano.Chain.Update.pvMinor bver)

type CardanoProtocol =
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkProtocol
            '[ Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary
            ]

readCardanoGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  case dncProtocol enc of
    DbSyncProtocolCardano ->
      GenesisCardano enc <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc

data DbSyncNodeError
  = NELookup !Text !LookupFail
  | NEError !Text
  | NEInvariant !Text !DbSyncInvariant
  | NEBlockMismatch !Word64 !ByteString !ByteString
  | NEByronConfig !FilePath !Cardano.Chain.Genesis.Config.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NECardanoConfig !Text

renderDbSyncNodeError :: DbSyncNodeError -> Text
renderDbSyncNodeError ne =
  case ne of
    NELookup loc lf -> mconcat [ "DB lookup fail in ", loc, ": ", renderLookupFail lf ]
    NEError t -> "Error: " <> t
    NEInvariant loc i -> mconcat [ loc, ": " <> renderDbSyncInvariant i ]
    NEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number ", textShow blkNo, ", db has "
        , bsBase16Encode hashDb, " but chain provided ", bsBase16Encode hashBlk
        ]
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file ", textShow fp, ": ", textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file ", textShow fp, ": ", txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   ", err
        ]

unTxHash :: Cardano.Crypto.Hashing.Hash Cardano.Chain.UTxO.Tx -> ByteString
unTxHash =  Cardano.Crypto.Hashing.abstractHashToBytes

renderDbSyncInvariant :: DbSyncInvariant -> Text
renderDbSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat [ "input value ", textShow inval, " < output value ", textShow outval ]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx ", bsBase16Encode (unTxHash $ Cardano.Crypto.Hashing.serializeCborHash tx)
        , " : input value ", textShow inval, " < output value ", textShow outval
        , "\n", textShow tx
        ]

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt

renderLookupFail :: LookupFail -> Text
renderLookupFail lf =
  case lf of
    DbLookupBlockHash h -> "block hash " <> base16encode h
    DbLookupBlockId blkid -> "block id " <> textShow blkid
    DbLookupMessage txt -> txt
    DbLookupTxHash h -> "tx hash " <> base16encode h
    DbLookupTxOutPair h i ->
        Text.concat [ "tx out pair (", base16encode h, ", ", textShow i, ")" ]
    DbLookupEpochNo e ->
        Text.concat [ "epoch number ", textShow e ]
    DbLookupSlotNo s ->
        Text.concat [ "slot number ", textShow s ]
    DbMetaEmpty -> "Meta table is empty"
    DbMetaMultipleRows -> "Multiple rows in Meta table which should only contain one"

base16encode :: ByteString -> Text
base16encode = Text.decodeUtf8 . Base16.encode

data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  deriving (Eq, Show)

data DbSyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Cardano.Chain.UTxO.Tx !Word64 !Word64

readByronGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO Cardano.Chain.Genesis.Config.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ dncByronGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ Cardano.Crypto.Hashing.decodeAbstractHash (unGenesisHashByron $ dncByronGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Cardano.Chain.Genesis.Config.mkConfigFromFile (dncRequiresNetworkMagic enc) file genHash


readShelleyGenesisConfig
    :: DbSyncNodeConfig
    -> ExceptT DbSyncNodeError IO ShelleyConfig
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ dncShelleyGenesisFile enc
  firstExceptT (NEShelleyConfig file . renderShelleyGenesisError)
    $ readGenesis (GenesisFile file) Nothing

textShow :: Show a => a -> Text
textShow = Text.pack . show

readGenesis
    :: GenesisFile -> Maybe GenesisHashShelley
    -> ExceptT ShelleyGenesisError IO ShelleyConfig
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashShelley (Cardano.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> left (GenesisHashMismatch actual expected)
        _ -> pure ()

data ShelleyGenesisError
     = GenesisReadError !FilePath !Text
     | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
     | GenesisDecodeError !FilePath !Text
     deriving Show

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash actual
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash expected, "."
          ]

      GenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]
  where
    renderHash :: GenesisHashShelley -> Text
    renderHash (GenesisHashShelley h) = Text.decodeUtf8 $ Base16.encode (Cardano.Crypto.Hash.Class.hashToBytes h)

