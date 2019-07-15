{-# LANGUAGE GADTs #-}

module CLI
  ( Protocol (..)
  , CLI (..)
  , parseCLI
  , SomeProtocol (..)
  , fromProtocol
  ) where

import           Data.Monoid (Last)

import           Options.Applicative

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Node.CLI
import           Cardano.Shell.Constants.CLI
import           Cardano.Shell.Constants.PartialTypes (PartialGenesis (..))

data CLI = CLI {
    cliCoreNodeId   :: CoreNodeId,
    cliNumCoreNodes :: NumCoreNodes,
    cliProtocol     :: Protocol,
    cliGenesis      :: (Last PartialGenesis)
  }

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseCoreNodeId
    <*> parseNumCoreNodes
    <*> parseProtocol
    <*> (pure <$> configGenesisCLIParser)
