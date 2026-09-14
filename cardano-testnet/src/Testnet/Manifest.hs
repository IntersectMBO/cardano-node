{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Manifest
  ( -- * Manifest types
    Manifest(..)
  , ManifestNetwork(..)
  , ManifestPaths(..)
  , ManifestGenesisFiles(..)
  , ManifestNode(..)
  , ManifestGrpc(..)
  , ManifestWallet(..)
    -- * Constants
  , manifestFileName
  , cardanoTestnetVersionString
    -- * File operations
  , writeManifest
  , removeStaleManifest
    -- * Manifest construction
  , buildManifest
  ) where

import           Cardano.Api (CardanoEra (..), File (..))

import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultGenesisFilepath)

import           Prelude

import           Control.Exception (onException, try)
import           Control.Monad (when)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NEL
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Version (showVersion)
import           System.Directory (doesFileExist, removeFile, renameFile)
import           System.FilePath (makeRelative, normalise, (</>))
import           System.IO (hClose)
import qualified System.IO as IO
import qualified System.Process as Process

import           Hedgehog.Extras.Stock (sprocketSystemName)

import           Testnet.Types (NodeRpcEndpoint (..), PaymentKeyInfo (..), TestnetNode (..),
                   TestnetRuntime (..), isTestnetNodeSpo, showIpv4Address, signingKeyFp,
                   verificationKeyFp)

import           Paths_cardano_testnet (version)


-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | The manifest file name, always written into the output directory root.
manifestFileName :: FilePath
manifestFileName = "manifest.json"

-- | The cardano-testnet package version as a string (e.g. @"11.1.1"@),
-- matching the output of @cardano-testnet version@.
cardanoTestnetVersionString :: String
cardanoTestnetVersionString = showVersion version


-- ---------------------------------------------------------------------------
-- Manifest types
-- ---------------------------------------------------------------------------

data Manifest = Manifest
  { manifestSchemaVersion        :: !Int
  , manifestCreatedAt            :: !UTCTime
  , manifestCardanoTestnetVersion :: !String
  , manifestNetwork              :: !ManifestNetwork
  , manifestPaths                :: !ManifestPaths
  , manifestNodes                :: ![ManifestNode]
  , manifestWallets              :: ![ManifestWallet]
  } deriving (Eq, Show)

data ManifestNetwork = ManifestNetwork
  { mnMagic       :: !Int
  , mnEra         :: !String
  , mnSystemStart :: !UTCTime
  } deriving (Eq, Show)

data ManifestPaths = ManifestPaths
  { mpNodeConfigFile :: !FilePath
  , mpGenesisFiles   :: !ManifestGenesisFiles
  } deriving (Eq, Show)

data ManifestGenesisFiles = ManifestGenesisFiles
  { mgfByron    :: !FilePath
  , mgfShelley  :: !FilePath
  , mgfAlonzo   :: !FilePath
  , mgfConway   :: !FilePath
  , mgfDijkstra :: !FilePath
  } deriving (Eq, Show)

data ManifestNode = ManifestNode
  { mnodeName         :: !String
  , mnodeRole         :: !String
  , mnodeHost         :: !String
  , mnodePort         :: !Int
  , mnodeSocketPath   :: !FilePath
  , mnodeGrpc         :: !(Maybe ManifestGrpc)
  , mnodePid          :: !(Maybe Int)
  , mnodePidFile      :: !FilePath
  , mnodeTopologyFile :: !FilePath
  , mnodeStdoutFile   :: !FilePath
  , mnodeStderrFile   :: !FilePath
  } deriving (Eq, Show)

-- | gRPC endpoint descriptor: HTTP or unix-socket transport.
data ManifestGrpc
  = ManifestGrpcHttp !String !Int       -- ^ host, port
  | ManifestGrpcUnixSocket !FilePath    -- ^ socket path (relative)
  deriving (Eq, Show)

data ManifestWallet = ManifestWallet
  { mwalletName                :: !String
  , mwalletAddress             :: !Text
  , mwalletSigningKeyFile      :: !FilePath
  , mwalletVerificationKeyFile :: !FilePath
  } deriving (Eq, Show)


-- ---------------------------------------------------------------------------
-- ToJSON instances (hand-written — field names are the contract)
-- ---------------------------------------------------------------------------

instance ToJSON Manifest where
  toJSON m = object
    [ "schemaVersion"          .= manifestSchemaVersion m
    , "createdAt"              .= manifestCreatedAt m
    , "cardanoTestnetVersion"  .= manifestCardanoTestnetVersion m
    , "network"                .= manifestNetwork m
    , "paths"                  .= manifestPaths m
    , "nodes"                  .= manifestNodes m
    , "wallets"                .= manifestWallets m
    ]

instance ToJSON ManifestNetwork where
  toJSON n = object
    [ "magic"       .= mnMagic n
    , "era"         .= mnEra n
    , "systemStart" .= mnSystemStart n
    ]

instance ToJSON ManifestPaths where
  toJSON p = object
    [ "nodeConfigFile" .= mpNodeConfigFile p
    , "genesisFiles"   .= mpGenesisFiles p
    ]

instance ToJSON ManifestGenesisFiles where
  toJSON g = object
    [ "byron"    .= mgfByron g
    , "shelley"  .= mgfShelley g
    , "alonzo"   .= mgfAlonzo g
    , "conway"   .= mgfConway g
    , "dijkstra" .= mgfDijkstra g
    ]

instance ToJSON ManifestNode where
  toJSON n = object
    [ "name"         .= mnodeName n
    , "role"         .= mnodeRole n
    , "host"         .= mnodeHost n
    , "port"         .= mnodePort n
    , "socketPath"   .= mnodeSocketPath n
    , "grpc"         .= mnodeGrpc n     -- Nothing encodes as JSON null
    , "pid"          .= mnodePid n      -- Nothing encodes as JSON null
    , "pidFile"      .= mnodePidFile n
    , "topologyFile" .= mnodeTopologyFile n
    , "stdoutFile"   .= mnodeStdoutFile n
    , "stderrFile"   .= mnodeStderrFile n
    ]

instance ToJSON ManifestGrpc where
  toJSON (ManifestGrpcHttp host port) = object
    [ "transport"  .= ("http" :: String)
    , "host"       .= host
    , "port"       .= port
    ]
  toJSON (ManifestGrpcUnixSocket path) = object
    [ "transport"  .= ("unix-socket" :: String)
    , "socketPath" .= path
    ]

instance ToJSON ManifestWallet where
  toJSON w = object
    [ "name"                 .= mwalletName w
    , "address"              .= mwalletAddress w
    , "signingKeyFile"       .= mwalletSigningKeyFile w
    , "verificationKeyFile"  .= mwalletVerificationKeyFile w
    ]


-- ---------------------------------------------------------------------------
-- FromJSON instances
-- ---------------------------------------------------------------------------

instance FromJSON Manifest where
  parseJSON = withObject "Manifest" $ \o -> Manifest
    <$> o .: "schemaVersion"
    <*> o .: "createdAt"
    <*> o .: "cardanoTestnetVersion"
    <*> o .: "network"
    <*> o .: "paths"
    <*> o .: "nodes"
    <*> o .: "wallets"

instance FromJSON ManifestNetwork where
  parseJSON = withObject "ManifestNetwork" $ \o -> ManifestNetwork
    <$> o .: "magic"
    <*> o .: "era"
    <*> o .: "systemStart"

instance FromJSON ManifestPaths where
  parseJSON = withObject "ManifestPaths" $ \o -> ManifestPaths
    <$> o .: "nodeConfigFile"
    <*> o .: "genesisFiles"

instance FromJSON ManifestGenesisFiles where
  parseJSON = withObject "ManifestGenesisFiles" $ \o -> ManifestGenesisFiles
    <$> o .: "byron"
    <*> o .: "shelley"
    <*> o .: "alonzo"
    <*> o .: "conway"
    <*> o .: "dijkstra"

instance FromJSON ManifestNode where
  parseJSON = withObject "ManifestNode" $ \o -> ManifestNode
    <$> o .: "name"
    <*> o .: "role"
    <*> o .: "host"
    <*> o .: "port"
    <*> o .: "socketPath"
    <*> o .: "grpc"
    <*> o .: "pid"
    <*> o .: "pidFile"
    <*> o .: "topologyFile"
    <*> o .: "stdoutFile"
    <*> o .: "stderrFile"

instance FromJSON ManifestGrpc where
  parseJSON = withObject "ManifestGrpc" $ \o -> do
    transport <- o .: "transport"
    case (transport :: String) of
      "http"        -> ManifestGrpcHttp <$> o .: "host" <*> o .: "port"
      "unix-socket" -> ManifestGrpcUnixSocket <$> o .: "socketPath"
      _             -> fail $ "Unknown gRPC transport: " <> transport

instance FromJSON ManifestWallet where
  parseJSON = withObject "ManifestWallet" $ \o -> ManifestWallet
    <$> o .: "name"
    <*> o .: "address"
    <*> o .: "signingKeyFile"
    <*> o .: "verificationKeyFile"


-- ---------------------------------------------------------------------------
-- File operations
-- ---------------------------------------------------------------------------

-- | Write the manifest atomically: write to a temp file in the same
-- directory, then rename.  A reader never sees a half-written file.
-- If writing fails, the temp file is removed rather than left behind.
writeManifest :: FilePath -> Manifest -> IO ()
writeManifest outputDir manifest = do
  let manifestPath = outputDir </> manifestFileName
  -- Default permissions (not openTempFile's owner-only 0600): the manifest
  -- must be as readable as the rest of the output directory.
  (tmpFile, tmpHandle) <- IO.openTempFileWithDefaultPermissions outputDir "manifest.json.tmp"
  let cleanup = do
        _ <- try (hClose tmpHandle) :: IO (Either IOError ())
        _ <- try (removeFile tmpFile) :: IO (Either IOError ())
        pure ()
  (do LBS.hPut tmpHandle (A.encodePretty manifest)
      hClose tmpHandle
      renameFile tmpFile manifestPath)
    `onException` cleanup

-- | Delete any pre-existing manifest in the output directory (fresh-run rule).
removeStaleManifest :: FilePath -> IO ()
removeStaleManifest outputDir = do
  let manifestPath = outputDir </> manifestFileName
  exists <- doesFileExist manifestPath
  when exists $ removeFile manifestPath


-- ---------------------------------------------------------------------------
-- Manifest construction
-- ---------------------------------------------------------------------------

-- | Build a manifest from the data available after the testnet is ready.
--
-- The era is passed as a string because @cardanoTestnet@ does not receive
-- the era as a parameter — both CLI paths (creation and node-env) go
-- through it.  For now the only supported era is Conway ('defaultEra'),
-- so this is always @\"conway\"@.
buildManifest
  :: FilePath          -- ^ Output directory (the @tmpAbsPath@ inside @cardanoTestnet@)
  -> TestnetRuntime    -- ^ The runtime returned after readiness checks pass
  -> String            -- ^ Era name, e.g. @\"conway\"@
  -> UTCTime           -- ^ System start time (from the shelley genesis, potentially updated)
  -> IO Manifest
buildManifest outputDir TestnetRuntime{testnetMagic, testnetNodes, wallets} era systemStart = do
  now <- getCurrentTime
  nodes <- mapM (buildNode outputDir) (NEL.toList testnetNodes)
  let ws = buildWallets outputDir wallets
  pure Manifest
    { manifestSchemaVersion        = 1
    , manifestCreatedAt            = now
    , manifestCardanoTestnetVersion = cardanoTestnetVersionString
    , manifestNetwork              = ManifestNetwork
        { mnMagic       = testnetMagic
        , mnEra         = era
        , mnSystemStart = systemStart
        }
    , manifestPaths                = ManifestPaths
        { mpNodeConfigFile = defaultConfigFile
        , mpGenesisFiles   = ManifestGenesisFiles
            { mgfByron    = defaultGenesisFilepath ByronEra
            , mgfShelley  = defaultGenesisFilepath ShelleyEra
            , mgfAlonzo   = defaultGenesisFilepath AlonzoEra
            , mgfConway   = defaultGenesisFilepath ConwayEra
            , mgfDijkstra = defaultGenesisFilepath DijkstraEra
            }
        }
    , manifestNodes                = nodes
    , manifestWallets              = ws
    }

buildNode :: FilePath -> TestnetNode -> IO ManifestNode
buildNode outputDir node = do
  mPid <- Process.getPid (nodeProcessHandle node)
  pure ManifestNode
    { mnodeName         = nodeName node
    , mnodeRole         = if isTestnetNodeSpo node then "spo" else "relay"
    , mnodeHost         = showIpv4Address (nodeIpv4 node)
    , mnodePort         = fromIntegral (nodePort node)
    , mnodeSocketPath   = relPath (sprocketSystemName (nodeSprocket node))
    , mnodeGrpc         = buildGrpc (nodeRpcEndpoint node)
    , mnodePid          = fmap fromIntegral mPid
    , mnodePidFile      = "logs" </> nodeName node </> "node.pid"
    , mnodeTopologyFile = "node-data" </> nodeName node </> "topology.json"
    , mnodeStdoutFile   = relPath (nodeStdout node)
    , mnodeStderrFile   = relPath (nodeStderr node)
    }
  where
    relPath = makeManifestRelPath outputDir

    buildGrpc Nothing = Nothing
    buildGrpc (Just (NodeRpcUnixSocket socketPath)) =
      Just $ ManifestGrpcUnixSocket (relPath (unFile socketPath))
    buildGrpc (Just (NodeRpcHttp ip port)) =
      Just $ ManifestGrpcHttp (show ip) (fromIntegral port)

buildWallets :: FilePath -> [PaymentKeyInfo] -> [ManifestWallet]
buildWallets outputDir = zipWith build [(1::Int)..]
  where
    build i PaymentKeyInfo{paymentKeyInfoPair, paymentKeyInfoAddr} =
      ManifestWallet
        { mwalletName                = "utxo" <> show i
        , mwalletAddress             = paymentKeyInfoAddr
        , mwalletSigningKeyFile      = relPath (signingKeyFp paymentKeyInfoPair)
        , mwalletVerificationKeyFile = relPath (verificationKeyFp paymentKeyInfoPair)
        }
    relPath = makeManifestRelPath outputDir

-- | Make a path relative to the output directory, normalised (no leading @./@).
-- Windows named pipes (starting with @\\.\pipe\@) are kept as-is.
makeManifestRelPath :: FilePath -> FilePath -> FilePath
makeManifestRelPath outputDir path
  | "\\\\.\\pipe\\" `isPrefixOf` path = path
  | otherwise                          = normalise (makeRelative outputDir path)
