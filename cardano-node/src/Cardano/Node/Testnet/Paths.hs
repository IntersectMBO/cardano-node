-- | Shared path conventions for cardano-testnet output directories.
--
-- Both @cardano-testnet@ (producer) and consumers of generated
-- testnet configurations depend on this module so that directory
-- layout changes are kept in sync at compile time.
module Cardano.Node.Testnet.Paths
  ( defaultNodeName
  , defaultNodeDataDir
  , defaultUtxoKeyDir
  , defaultUtxoSKeyPath
  , defaultUtxoVKeyPath
  , defaultUtxoAddrPath
  , defaultSocketDir
  , defaultSocketName
  , defaultSocketPath
  , defaultConfigFile
  , defaultPortFile
  ) where

import           System.FilePath ((</>))

-- | The name of a node: @"node" <> show n@
defaultNodeName :: Int -> String
defaultNodeName n = "node" <> show n

-- | Relative path to a node's data directory: @"node-data" </> defaultNodeName n@
defaultNodeDataDir :: Int -> FilePath
defaultNodeDataDir n = "node-data" </> defaultNodeName n

-- | Relative path to a UTxO key directory: @"utxo-keys" </> "utxo" <> show n@
defaultUtxoKeyDir :: Int -> FilePath
defaultUtxoKeyDir n = "utxo-keys" </> "utxo" <> show n

-- | Relative path to a UTxO signing key: @defaultUtxoKeyDir n </> "utxo.skey"@
defaultUtxoSKeyPath :: Int -> FilePath
defaultUtxoSKeyPath n = defaultUtxoKeyDir n </> "utxo.skey"

-- | Relative path to a UTxO verification key: @defaultUtxoKeyDir n </> "utxo.vkey"@
defaultUtxoVKeyPath :: Int -> FilePath
defaultUtxoVKeyPath n = defaultUtxoKeyDir n </> "utxo.vkey"

-- | Relative path to a UTxO address file: @defaultUtxoKeyDir n </> "utxo.addr"@
defaultUtxoAddrPath :: Int -> FilePath
defaultUtxoAddrPath n = defaultUtxoKeyDir n </> "utxo.addr"

-- | Socket directory name: @"socket"@
defaultSocketDir :: FilePath
defaultSocketDir = "socket"

-- | Socket file name: @"sock"@
defaultSocketName :: FilePath
defaultSocketName = "sock"

-- | Relative path to a node's socket: @defaultSocketDir </> defaultNodeName n </> defaultSocketName@
defaultSocketPath :: Int -> FilePath
defaultSocketPath n = defaultSocketDir </> defaultNodeName n </> defaultSocketName

-- | Main node configuration file name: @"configuration.yaml"@
defaultConfigFile :: FilePath
defaultConfigFile = "configuration.yaml"

-- | Relative path to a node's port file: @defaultNodeDataDir n </> "port"@
defaultPortFile :: Int -> FilePath
defaultPortFile n = defaultNodeDataDir n </> "port"
