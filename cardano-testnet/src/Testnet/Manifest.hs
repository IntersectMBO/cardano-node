{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Manifest
  ( -- * Manifest types
    Manifest(..)
  , ManifestNetwork(..)
  , ManifestPaths(..)
  , ManifestGenesisFiles(..)
  , ManifestNode(..)
  , ManifestNodeRole(..)
  , ManifestGrpc(..)
  , ManifestWallet(..)
    -- * File operations
  , writeManifest
  , removeStaleManifest
    -- * Manifest construction
  , buildManifest
  ) where

import           Cardano.Api (AddressAny, AnyCardanoEra (..), AsType (AsAddressAny),
                   CardanoEra (..), File (..), deserialiseAddress, serialiseAddress)

import           Cardano.Node.Testnet.Paths (defaultGenesisFilepath, defaultManifestFile,
                   defaultNodePidFile, defaultNodeTopologyFile)

import           Prelude

import           Control.Exception.Safe (onException, throwString, try)
import           Control.Monad (when, zipWithM)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import           Data.IP (IP, IPv4, toHostAddress)
import           Data.List (find)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Version (Version, parseVersion, showVersion)
import           Data.Word (Word32)
import           Network.Socket (HostAddress, PortNumber)
import           System.Directory (doesFileExist, removeFile, renameFile)
import           System.FilePath (makeRelative, normalise, (</>))
import           System.IO (hClose)
import qualified System.IO as IO
import qualified System.Process as Process
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read (readMaybe)

import           Hedgehog.Extras.Stock (sprocketSystemName)

import           Testnet.Start.Types (anyEraToString)
import           Testnet.Types (NodeRpcEndpoint (..), PaymentKeyInfo (..), TestnetNode (..),
                   TestnetRuntime (..), isTestnetNodeSpo, showIpv4Address, signingKeyFp,
                   verificationKeyFp)

import           Paths_cardano_testnet (version)


-- ---------------------------------------------------------------------------
-- Manifest types
-- ---------------------------------------------------------------------------

data Manifest = Manifest
  { manifestSchemaVersion        :: !Int
  , manifestCreatedAt            :: !UTCTime
  , manifestCardanoTestnetVersion :: !Version
  , manifestNetwork              :: !ManifestNetwork
  , manifestPaths                :: !ManifestPaths
  , manifestNodes                :: !(NonEmpty ManifestNode)
  , manifestWallets              :: ![ManifestWallet]
  } deriving (Eq, Show)

data ManifestNetwork = ManifestNetwork
  { mnMagic       :: !Word32
  , mnEra         :: !AnyCardanoEra
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

-- | Node entry.  Host, ports and pid keep their domain types
-- ('HostAddress', 'PortNumber', 'IP', 'Process.Pid'); the string/number
-- forms exist only in the JSON instances, so the record cannot drift
-- from the runtime values.
data ManifestNode = ManifestNode
  { mnodeName         :: !String
  , mnodeRole         :: !ManifestNodeRole
  , mnodeHost         :: !HostAddress
  , mnodePort         :: !PortNumber
  , mnodeSocketPath   :: !FilePath
  , mnodeGrpc         :: !(Maybe ManifestGrpc)
  , mnodePid          :: !(Maybe Process.Pid)
  , mnodePidFile      :: !FilePath
  , mnodeTopologyFile :: !FilePath
  , mnodeStdoutFile   :: !FilePath
  , mnodeStderrFile   :: !FilePath
  } deriving (Eq, Show)

-- | gRPC endpoint descriptor: HTTP or unix-socket transport.
data ManifestGrpc
  = ManifestGrpcHttp !IP !PortNumber
  | ManifestGrpcUnixSocket !FilePath    -- ^ socket path (relative)
  deriving (Eq, Show)

-- | Node role.  The schema pins the JSON values (@"spo"@/@"relay"@);
-- adding a role bumps the schema version.
data ManifestNodeRole = RoleSpo | RoleRelay
  deriving (Eq, Show)

data ManifestWallet = ManifestWallet
  { mwalletName                :: !String
  , mwalletAddress             :: !AddressAny
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
    , "cardanoTestnetVersion"  .= showVersion (manifestCardanoTestnetVersion m)
    , "network"                .= manifestNetwork m
    , "paths"                  .= manifestPaths m
    , "nodes"                  .= manifestNodes m
    , "wallets"                .= manifestWallets m
    ]

instance ToJSON ManifestNetwork where
  toJSON n = object
    [ "magic"       .= mnMagic n
    , "era"         .= anyEraToString (mnEra n)
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
    , "host"         .= (showIpv4Address (mnodeHost n) :: String)
    , "port"         .= (fromIntegral (mnodePort n) :: Int)
    , "socketPath"   .= mnodeSocketPath n
    , "grpc"         .= mnodeGrpc n     -- Nothing encodes as JSON null
    , "pid"          .= (fromIntegral <$> mnodePid n :: Maybe Int)  -- Nothing encodes as JSON null
    , "pidFile"      .= mnodePidFile n
    , "topologyFile" .= mnodeTopologyFile n
    , "stdoutFile"   .= mnodeStdoutFile n
    , "stderrFile"   .= mnodeStderrFile n
    ]

instance ToJSON ManifestGrpc where
  toJSON (ManifestGrpcHttp host port) = object
    [ "transport"  .= ("http" :: String)
    , "host"       .= show host
    , "port"       .= (fromIntegral port :: Int)
    ]
  toJSON (ManifestGrpcUnixSocket path) = object
    [ "transport"  .= ("unix-socket" :: String)
    , "socketPath" .= path
    ]

instance ToJSON ManifestNodeRole where
  toJSON RoleSpo   = "spo"
  toJSON RoleRelay = "relay"

instance ToJSON ManifestWallet where
  toJSON w = object
    [ "name"                 .= mwalletName w
    , "address"              .= serialiseAddress (mwalletAddress w)
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
    <*> (parseVersionString =<< o .: "cardanoTestnetVersion")
    <*> o .: "network"
    <*> o .: "paths"
    <*> o .: "nodes"
    <*> o .: "wallets"

instance FromJSON ManifestNetwork where
  parseJSON = withObject "ManifestNetwork" $ \o -> ManifestNetwork
    <$> o .: "magic"
    <*> (parseEra =<< o .: "era")
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
    <*> (parseHostAddress =<< o .: "host")
    <*> (parsePort =<< o .: "port")
    <*> o .: "socketPath"
    <*> o .: "grpc"
    <*> (fmap fromIntegral <$> (o .: "pid" :: Parser (Maybe Int)))
    <*> o .: "pidFile"
    <*> o .: "topologyFile"
    <*> o .: "stdoutFile"
    <*> o .: "stderrFile"

instance FromJSON ManifestGrpc where
  parseJSON = withObject "ManifestGrpc" $ \o -> do
    transport <- o .: "transport"
    case (transport :: String) of
      "http"        -> ManifestGrpcHttp <$> (parseIP =<< o .: "host") <*> (parsePort =<< o .: "port")
      "unix-socket" -> ManifestGrpcUnixSocket <$> o .: "socketPath"
      _             -> fail $ "Unknown gRPC transport: " <> transport

instance FromJSON ManifestNodeRole where
  parseJSON = withText "ManifestNodeRole" parseRole
    where
      parseRole "spo"   = pure RoleSpo
      parseRole "relay" = pure RoleRelay
      parseRole t       = fail $ "Unknown node role: " <> Text.unpack t

-- | Parse a dotted-quad IPv4 address, e.g. @"127.0.0.1"@.
parseHostAddress :: String -> Parser HostAddress
parseHostAddress s =
  maybe (fail $ "Invalid IPv4 address: " <> s) (pure . toHostAddress) (readMaybe s :: Maybe IPv4)

-- | Parse an IPv4 or IPv6 address.
parseIP :: String -> Parser IP
parseIP s = maybe (fail $ "Invalid IP address: " <> s) pure (readMaybe s)

-- | Parse a port number, checking the 0-65535 range.
parsePort :: Int -> Parser PortNumber
parsePort p
  | p >= 0 && p <= 65535 = pure (fromIntegral p)
  | otherwise            = fail $ "Port out of range: " <> show p

-- | Parse a lowercase era name, e.g. @"conway"@.
parseEra :: String -> Parser AnyCardanoEra
parseEra s =
  maybe (fail $ "Unknown era name: " <> s) pure $
    find (\e -> anyEraToString e == s) [minBound .. maxBound]

-- | Parse a bech32 (or base58 byron) address.
parseAddress :: Text -> Parser AddressAny
parseAddress t =
  maybe (fail $ "Invalid address: " <> Text.unpack t) pure $
    deserialiseAddress AsAddressAny t

-- | Parse a version string, e.g. @"11.1.1"@.
parseVersionString :: String -> Parser Version
parseVersionString s =
  case [v | (v, "") <- readP_to_S parseVersion s] of
    [v] -> pure v
    _   -> fail $ "Invalid version string: " <> s

instance FromJSON ManifestWallet where
  parseJSON = withObject "ManifestWallet" $ \o -> ManifestWallet
    <$> o .: "name"
    <*> (parseAddress =<< o .: "address")
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
  let manifestPath = outputDir </> defaultManifestFile
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
  let manifestPath = outputDir </> defaultManifestFile
  exists <- doesFileExist manifestPath
  when exists $ removeFile manifestPath


-- ---------------------------------------------------------------------------
-- Manifest construction
-- ---------------------------------------------------------------------------

-- | Build a manifest from the data available after the testnet is ready.
--
-- The era is passed in because @cardanoTestnet@ does not receive it as a
-- parameter — both CLI paths (creation and node-env) go through it.  For
-- now the only supported era is Conway ('defaultEra').
buildManifest
  :: FilePath          -- ^ Output directory (the @tmpAbsPath@ inside @cardanoTestnet@)
  -> TestnetRuntime    -- ^ The runtime returned after readiness checks pass
  -> AnyCardanoEra     -- ^ The era the testnet runs in
  -> UTCTime           -- ^ System start time (from the shelley genesis, potentially updated)
  -> IO Manifest
buildManifest outputDir TestnetRuntime{testnetMagic, testnetNodes, wallets, configurationFile, shelleyGenesisFile} era systemStart = do
  now <- getCurrentTime
  nodes <- mapM (buildNode outputDir) testnetNodes
  ws <- buildWallets outputDir wallets
  pure Manifest
    { manifestSchemaVersion        = 1
    , manifestCreatedAt            = now
    , manifestCardanoTestnetVersion = version
    , manifestNetwork              = ManifestNetwork
        { -- TestnetRuntime carries the magic as Int; the value comes from
          -- the genesis Word32, so narrowing it back cannot lose anything.
          mnMagic       = fromIntegral testnetMagic
        , mnEra         = era
        , mnSystemStart = systemStart
        }
    , manifestPaths                = ManifestPaths
        -- The config and shelley genesis paths come from the runtime — the
        -- values the nodes were actually started with — so the manifest
        -- cannot drift from them.  The other genesis files have no runtime
        -- field, so the default constants are used.
        { mpNodeConfigFile = relPath (unFile configurationFile)
        , mpGenesisFiles   = ManifestGenesisFiles
            { mgfByron    = defaultGenesisFilepath ByronEra
            , mgfShelley  = relPath shelleyGenesisFile
            , mgfAlonzo   = defaultGenesisFilepath AlonzoEra
            , mgfConway   = defaultGenesisFilepath ConwayEra
            , mgfDijkstra = defaultGenesisFilepath DijkstraEra
            }
        }
    , manifestNodes                = nodes
    , manifestWallets              = ws
    }
  where
    relPath = makeManifestRelPath outputDir

buildNode :: FilePath -> TestnetNode -> IO ManifestNode
buildNode outputDir node = do
  mPid <- Process.getPid (nodeProcessHandle node)
  pure ManifestNode
    { mnodeName         = nodeName node
    , mnodeRole         = if isTestnetNodeSpo node then RoleSpo else RoleRelay
    , mnodeHost         = nodeIpv4 node
    , mnodePort         = nodePort node
    , mnodeSocketPath   = relPath (sprocketSystemName (nodeSprocket node))
    , mnodeGrpc         = buildGrpc <$> nodeRpcEndpoint node
    , mnodePid          = mPid
    , mnodePidFile      = defaultNodePidFile (nodeName node)
    , mnodeTopologyFile = defaultNodeTopologyFile (nodeName node)
    , mnodeStdoutFile   = relPath (nodeStdout node)
    , mnodeStderrFile   = relPath (nodeStderr node)
    }
  where
    relPath = makeManifestRelPath outputDir

    buildGrpc (NodeRpcUnixSocket socketPath) =
      ManifestGrpcUnixSocket (relPath (unFile socketPath))
    buildGrpc (NodeRpcHttp ip port) = ManifestGrpcHttp ip port

buildWallets :: FilePath -> [PaymentKeyInfo] -> IO [ManifestWallet]
buildWallets outputDir = zipWithM build [(1::Int)..]
  where
    build i PaymentKeyInfo{paymentKeyInfoPair, paymentKeyInfoAddr} = do
      -- The runtime carries the address as the raw file contents; parsing it
      -- here means the manifest can only ever hold a valid address.
      addr <- case deserialiseAddress AsAddressAny paymentKeyInfoAddr of
        Just a  -> pure a
        Nothing -> throwString $ "buildManifest: invalid wallet address: " <> Text.unpack paymentKeyInfoAddr
      pure ManifestWallet
        { mwalletName                = "utxo" <> show i
        , mwalletAddress             = addr
        , mwalletSigningKeyFile      = relPath (signingKeyFp paymentKeyInfoPair)
        , mwalletVerificationKeyFile = relPath (verificationKeyFp paymentKeyInfoPair)
        }
    relPath = makeManifestRelPath outputDir

-- | Make a path relative to the output directory, normalised (no leading @./@).
--
-- Windows named pipes (@\\.\pipe\...@) come through unchanged without a
-- special case: 'System.FilePath.splitDrive' treats @\\.\@ as a drive, so
-- 'makeRelative' returns pipe paths as-is and 'normalise' keeps the prefix
-- (checked on filepath 1.4.301.0, both the Windows and Posix flavours).
makeManifestRelPath :: FilePath -> FilePath -> FilePath
makeManifestRelPath outputDir = normalise . makeRelative outputDir
