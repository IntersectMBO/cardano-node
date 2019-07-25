{-# LANGUAGE GADTs #-}

module CLI
  ( Protocol (..)
  , CLI (..)
  , parseCLI
  , SomeProtocol (..)
  , fromProtocol
  ) where

import           Data.Monoid (Last (..))

import           Options.Applicative

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Node.CLI
import           Cardano.Shell.Constants.CLI
import           Cardano.Shell.Constants.PartialTypes ( PartialGenesis (..)
                                                      , PartialStaticKeyMaterial)

data CLI = CLI {
    cliCoreNodeId   :: CoreNodeId,
    cliNumCoreNodes :: NumCoreNodes,
    cliProtocol     :: Protocol,
    cliGenesis      :: Last PartialGenesis,
    cliKeyMaterial  :: Last PartialStaticKeyMaterial
  }

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseCoreNodeId
    <*> parseNumCoreNodes
    <*> parseProtocol
    <*> (Last . Just <$> configGenesisCLIParser)
    <*> (Last . Just <$> configStaticKeyMaterialCLIParser)
