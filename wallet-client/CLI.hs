{-# LANGUAGE GADTs #-}

module CLI
  ( Protocol (..)
  , CLI (..)
  , parseCLI
  , SomeProtocol (..)
  , fromProtocol
  ) where

import           Data.Monoid (Last (..))
import           Data.Text (Text)

import           Options.Applicative

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Node.CLI

data CLI = CLI
  { cliCoreNodeId                 :: CoreNodeId
  , cliNumCoreNodes               :: NumCoreNodes
  , cliProtocol                   :: Protocol
  , cliGenesisFile                :: Last FilePath
  , cliGenesisHash                :: Last Text
  , cliStaticKeySigningKeyFile    :: Last FilePath
  , cliStaticKeyDlgCertFile       :: Last FilePath
  }

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseCoreNodeId
    <*> parseNumCoreNodes
    <*> parseProtocol
    <*> lastStrOption
           ( long "genesis-file"
          <> metavar "FILEPATH"
          <> help "The filepath to the genesis file."
           )
    <*> lastStrOption
           ( long "genesis-hash"
          <> metavar "GENESIS-HASH"
          <> help "The genesis hash value."
           )
    <*> lastStrOption
           ( long "signing-key"
          <> metavar "FILEPATH"
          <> help "Path to the signing key."
           )
    <*> lastStrOption
           ( long "delegation-certificate"
          <> metavar "FILEPATH"
          <> help "Path to the delegation certificate."
           )
