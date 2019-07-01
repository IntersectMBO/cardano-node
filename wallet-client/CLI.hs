{-# LANGUAGE GADTs #-}

module CLI
  ( Protocol (..)
  , CLI (..)
  , parseCLI
  , SomeProtocol (..)
  , fromProtocol
  ) where

import           Options.Applicative

import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Node.CLI

data CLI = CLI {
    cliCoreNodeId   :: CoreNodeId,
    cliNumCoreNodes :: NumCoreNodes,
    cliProtocol     :: Protocol
  }

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseCoreNodeId
    <*> parseNumCoreNodes
    <*> parseProtocol
