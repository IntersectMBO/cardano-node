{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.TxGenerator.Setup.TestnetDiscovery
  ( FillDefaults (..)
  , ForceInfra (..)
  , TestnetMergeFlags (..)
  , TestnetConfig (..)
  , discoverTestnetConfig
  ) where

import           Cardano.Api (AnyCardanoEra (..), AnyShelleyBasedEra (..), CardanoEra (..),
                   File (..), toCardanoEra)

import qualified Cardano.Ledger.Coin as L
import           Cardano.Node.Configuration.NodeAddress (NodeAddress' (..), NodeHostIPv4Address (..),
                   NodeIPv4Address)
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration (..), parseNodeConfigurationFP)
import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultNodeDataDir, defaultNodeName,
                   defaultPortFile, defaultSocketPath, defaultUtxoSKeyPath)
import           Cardano.Node.Types (ConfigYamlFilePath (..), NodeProtocolConfiguration (..),
                   npcTestStartingEra)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..), NodeDescription (..))

import           Cardano.Prelude (unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Char (isDigit)
import           Data.List (isPrefixOf, sortOn)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Last (..))
import           Network.Socket (PortNumber)
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.Exit (die)
import           System.FilePath ((</>), takeDirectory)
import           Text.Read (readMaybe)

-- | Whether to fill in default values for all 'NixServiceOptions' fields
-- before merging the user-provided configuration.
data FillDefaults
  = FillDefaults
    -- ^ Start from a complete 'defaultNixServiceOptions' (with discovered
    -- infrastructure already applied), so any field the user omits gets a
    -- sensible default.
  | NoFillDefaults
    -- ^ Start from only the discovered infrastructure keys; the user
    -- configuration must supply every other required field.
  deriving (Show, Eq, Bounded, Enum)

-- | Whether discovered infrastructure values (socket path, signing key,
-- node config file, target nodes) should override the user-provided
-- configuration.
data ForceInfra
  = ForceInfra
    -- ^ After merging the user configuration, force the discovered
    -- infrastructure keys back on top, so they always reflect the actual
    -- testnet directory regardless of what the user supplied.
  | NoForceInfra
    -- ^ Let the user configuration win for infrastructure keys as well;
    -- discovered values are used only when the user does not provide them.
  deriving (Show, Eq, Bounded, Enum)

-- | Flags that control how the discovered testnet infrastructure is merged
-- with user-provided JSON configuration in 'discoverTestnetConfig'.
data TestnetMergeFlags = TestnetMergeFlags
  { fillDefaults :: FillDefaults
  , forceInfra   :: ForceInfra
  } deriving (Show, Eq)

-- | Everything 'discoverTestnetConfig' needs to locate a @cardano-testnet@
-- output directory and decide how to merge its contents with user configuration.
data TestnetConfig = TestnetConfig
  { tcDir        :: FilePath
  , tcMergeFlags :: TestnetMergeFlags
  } deriving (Show, Eq)


-- | Infrastructure fields discovered from a testnet directory.
testnetSpecificKeys :: [Aeson.Key]
testnetSpecificKeys =
  [ "localNodeSocketPath"
  , "sigKey"
  , "nodeConfigFile"
  , "targetNodes"
  ]


-- | Complete default values for all NixServiceOptions fields.
-- Uses explicit field construction (no wildcards) so that adding a new required
-- field to NixServiceOptions will cause a compile error here.
defaultNixServiceOptions :: NixServiceOptions
defaultNixServiceOptions = NixServiceOptions
  { _nix_debugMode            = False
  , _nix_tx_count             = 100
  , _nix_tps                  = 10
  , _nix_inputs_per_tx        = 2
  , _nix_outputs_per_tx       = 2
  , _nix_tx_fee               = L.Coin 212345
  , _nix_min_utxo_value       = L.Coin 1000000
  , _nix_add_tx_size          = 39
  , _nix_init_cooldown        = 50
  , _nix_era                  = AnyCardanoEra ConwayEra  -- placeholder, will be overridden
  , _nix_plutus               = Nothing
  , _nix_keepalive            = Just 30
  , _nix_nodeConfigFile       = Nothing
  , _nix_cardanoTracerSocket  = Nothing
  , _nix_sigKey               = File ""    -- placeholder, will be overridden
  , _nix_localNodeSocketPath  = ""         -- placeholder, will be overridden
  , _nix_targetNodes          = NodeDescription
      { ndAddr = mkLocalhostAddr 0
      , ndName = ""
      } :| []                              -- placeholder, will be overridden
  }


-- | Discover testnet config from a cardano-testnet output directory and merge
-- it with optional user-provided JSON config according to the merge flags.
discoverTestnetConfig :: TestnetConfig -> Aeson.Value -> IO NixServiceOptions
discoverTestnetConfig TestnetConfig{tcDir, tcMergeFlags} userConfig = do
  -- Validate that the directory exists
  dirExists <- doesDirectoryExist tcDir
  unless dirExists $ die $ "Testnet directory does not exist: " ++ tcDir

  -- Discover infrastructure
  era <- detectEra tcDir
  targetNodes <- discoverNodes tcDir
  let socketPath = tcDir </> defaultSocketPath 1
      sigKeyPath = tcDir </> defaultUtxoSKeyPath 1
      configPath = tcDir </> defaultConfigFile

  -- Validate critical files
  validateFileExists socketPath "socket"
  validateFileExists sigKeyPath "signing key"
  validateFileExists configPath "configuration"

  -- Build the infra-populated defaults
  let withInfra = defaultNixServiceOptions
        { _nix_era                 = era
        , _nix_localNodeSocketPath = socketPath
        , _nix_sigKey              = File sigKeyPath
        , _nix_nodeConfigFile      = Just configPath
        , _nix_targetNodes         = targetNodes
        }

  -- Build JSON values for merge
  let baseConfig       = Aeson.toJSON withInfra
      testnetInfraOnly = extractInfraKeys baseConfig

  -- Merge logic
  let TestnetMergeFlags{fillDefaults = fd, forceInfra = fi} = tcMergeFlags

  -- Step 1: Start with full defaults (if FillDefaults) or infra-only
  let start = case fd of
        FillDefaults   -> baseConfig
        NoFillDefaults -> testnetInfraOnly

  -- Step 2: Merge user config on top (user wins)
  let merged = mergeValues start userConfig

  -- Step 3: If ForceInfra, force infra keys back
  let final = case fi of
        ForceInfra   -> mergeValues merged testnetInfraOnly
        NoForceInfra -> merged

  -- Step 4: Deserialise back to NixServiceOptions
  case Aeson.fromJSON final of
    Aeson.Success opts -> pure opts
    Aeson.Error err    -> die $ "Failed to parse merged config: " ++ err


-- | Detect the era from the testnet's configuration.yaml using npcTestStartingEra.
detectEra :: FilePath -> IO AnyCardanoEra
detectEra dir = do
  let configPath = dir </> defaultConfigFile
  validateFileExists configPath "configuration"
  pnc <- parseNodeConfigurationFP (Just (ConfigYamlFilePath configPath))
  case getLast (pncProtocolConfig pnc) of
    Nothing -> die $ "No protocol configuration found in: " ++ configPath
    Just (NodeProtocolConfigurationCardano _ _ _ _ _ hfConfig _) ->
      case npcTestStartingEra hfConfig of
        Nothing -> die $ "Cannot detect era from config (no instant hard forks configured): " ++ configPath
        Just (AnyShelleyBasedEra sbe) -> pure $ AnyCardanoEra (toCardanoEra sbe)


-- | Discover nodes by scanning for port files in the testnet directory.
-- cardano-testnet always starts nodes on localhost (see testnetDefaultIpv4Address
-- in Testnet.Types). If remote/container support is added in the future,
-- cardano-testnet should write node addresses to a metadata file.
discoverNodes :: FilePath -> IO (NonEmpty NodeDescription)
discoverNodes dir = do
  -- Derive the base "node-data" directory from defaultNodeDataDir
  let nodeDataDir = dir </> takeDirectory (defaultNodeDataDir 1)
  exists <- doesDirectoryExist nodeDataDir
  if not exists
    then die $ "Node data directory does not exist: " ++ nodeDataDir
    else do
      entries <- listDirectory nodeDataDir
      let nodeIndices = sortOn id $ mapMaybe parseNodeIndex entries
      nodes <- mapM (readNodeDescription dir) nodeIndices
      case nodes of
        []     -> die $ "No nodes found in: " ++ nodeDataDir
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
    Nothing -> die $ "Invalid port number in: " ++ portPath
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


-- | Extract only the infrastructure keys from a JSON value.
extractInfraKeys :: Aeson.Value -> Aeson.Value
extractInfraKeys (Aeson.Object obj) =
  Aeson.Object $ KeyMap.filterWithKey (\k _ -> k `elem` testnetSpecificKeys) obj
extractInfraKeys v = v


-- | Deep merge two JSON values. Objects are merged recursively;
-- for all other types the override wins.
mergeValues :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeValues (Aeson.Object base) (Aeson.Object override) =
  Aeson.Object (KeyMap.unionWith mergeValues base override)
mergeValues _ override = override


-- | Validate that a file exists, dying with a clear message if not.
validateFileExists :: FilePath -> String -> IO ()
validateFileExists path description = do
  exists <- doesFileExist path
  unless exists $ die $ "Required " ++ description ++ " file not found: " ++ path
