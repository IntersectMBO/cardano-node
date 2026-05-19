{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.TxGenerator.Setup.TestnetDiscovery
  ( TestnetConfig (..)
  , discoverTestnetConfig
  ) where

import           Cardano.Node.Configuration.NodeAddress (NodeAddress' (..), NodeHostIPv4Address (..),
                   NodeIPv4Address)
import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultNodeDataDir, defaultNodeName,
                   defaultPortFile, defaultSocketPath, defaultUtxoSKeyPath)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions, NodeDescription (..))

import           Cardano.Prelude ( unless, sort )
import           Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Char (isDigit)
import           Data.List (isPrefixOf)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe (mapMaybe)
import           Network.Socket (PortNumber)
import           System.Directory (doesDirectoryExist, doesPathExist, listDirectory)
import           System.Exit (die)
import           System.FilePath ((</>), takeDirectory)
import           Text.Read (readMaybe)

-- | Location of a @cardano-testnet@ output directory.
newtype TestnetConfig = TestnetConfig
  { tcDir :: FilePath
  } deriving (Show, Eq)


-- | Discover testnet connection settings from a @cardano-testnet@ output
-- directory and merge them with user-provided JSON config.
--
-- The 4 connection settings (@localNodeSocketPath@, @sigKey@,
-- @nodeConfigFile@, @targetNodes@) are always populated from the testnet
-- directory and override any values in the user config.  All other fields
-- must be supplied by the user.
discoverTestnetConfig :: TestnetConfig -> Aeson.Value -> IO NixServiceOptions
discoverTestnetConfig TestnetConfig{tcDir} userConfig = do
  dirExists <- doesDirectoryExist tcDir
  unless dirExists $ die $ "discoverTestnetConfig: testnet directory does not exist: " ++ tcDir

  targetNodes <- discoverNodes tcDir
  let socketPath = tcDir </> defaultSocketPath 1
      sigKeyPath = tcDir </> defaultUtxoSKeyPath 1
      configPath = tcDir </> defaultConfigFile

  validateFileExists socketPath "socket"
  validateFileExists sigKeyPath "signing key"
  validateFileExists configPath "configuration"

  let connectionSettings = object
        [ "localNodeSocketPath" .= socketPath
        , "sigKey"              .= sigKeyPath
        , "nodeConfigFile"      .= configPath
        , "targetNodes"         .= targetNodes
        ]

  let merged = mergeValues userConfig connectionSettings

  case Aeson.fromJSON merged of
    Aeson.Success opts -> pure opts
    Aeson.Error err    -> die $ "discoverTestnetConfig: failed to parse merged config: " ++ err


-- | Discover nodes by scanning for port files in the testnet directory.
-- cardano-testnet always starts nodes on localhost (see testnetDefaultIpv4Address
-- in Testnet.Types). If remote/container support is added in the future,
-- cardano-testnet should write node addresses to a metadata file.
discoverNodes :: FilePath -> IO (NonEmpty NodeDescription)
discoverNodes dir = do
  let nodeDataDir = dir </> takeDirectory (defaultNodeDataDir 1)
  exists <- doesDirectoryExist nodeDataDir
  if not exists
    then die $ "discoverNodes: node data directory does not exist: " ++ nodeDataDir
    else do
      entries <- listDirectory nodeDataDir
      let nodeIndices = sort $ mapMaybe parseNodeIndex entries
      nodes <- mapM (readNodeDescription dir) nodeIndices
      case nodes of
        []     -> die $ "discoverNodes: no nodes found in: " ++ nodeDataDir
        (n:ns) -> pure (n :| ns)


-- | Parse a node index from a directory name like "node1", "node2", etc.
parseNodeIndex :: String -> Maybe Int
parseNodeIndex name
  | "node" `isPrefixOf` name = readMaybe (dropWhile (not . isDigit) name)
  | otherwise = Nothing


-- | Read a node description from its port file.
readNodeDescription :: FilePath -> Int -> IO NodeDescription
readNodeDescription dir idx = do
  let portPath = dir </> defaultPortFile idx
  validateFileExists portPath ("port file for " ++ defaultNodeName idx)
  portStr <- readFile portPath
  case readMaybe portStr :: Maybe PortNumber of
    Nothing -> die $ "readNodeDescription: invalid port number in: " ++ portPath
    Just port -> pure NodeDescription
      { ndAddr = mkLocalhostAddr port
      , ndName = defaultNodeName idx
      }

-- | Create a localhost NodeIPv4Address at the given port.
mkLocalhostAddr :: PortNumber -> NodeIPv4Address
mkLocalhostAddr port = NodeAddress
  { naHostAddress = NodeHostIPv4Address
      { unNodeHostIPv4Address = read "127.0.0.1" }
  , naPort = port
  }


-- | Deep merge two JSON values. Objects are merged recursively;
-- for all other types the override wins.
mergeValues :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeValues (Aeson.Object base) (Aeson.Object override) =
  Aeson.Object (KeyMap.unionWith mergeValues base override)
mergeValues _ override = override


-- | Validate that a path exists (file, socket, etc.), dying with a clear message if not.
validateFileExists :: FilePath -> String -> IO ()
validateFileExists path description = do
  exists <- doesPathExist path
  unless exists $ die $ "validateFileExists: required " ++ description ++ " file not found: " ++ path
