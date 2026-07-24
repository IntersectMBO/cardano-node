{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Downstream home for the db-synthesizer's configuration and credential
-- machinery.
--
-- The forging engine ('Consensus.synthesize') lives in consensus and
-- consumes a @('ProtocolInfo', block forgers)@ pair plus an 'EpochSize'.
-- Constructing those from a node configuration file and on-disk forging
-- credentials is a node concern, so it is done here using cardano-node's own
-- protocol-instantiation machinery ('Node.mkConsensusProtocol') and cardano-api's
-- 'Api.protocolInfo' bridge — the same path the running node takes at startup.
module Cardano.Node.Tools.DBSynthesizer
  ( DBSynthesizerException (..)
  , initializeProtocol
  , synthesizeFromConfig
  ) where

import Cardano.Api (BlockType (..), ProtocolInfoArgs (..))
import qualified Cardano.Api as Api (protocolInfo)

import qualified Cardano.Ledger.Api.Transition as Ledger (tcShelleyGenesisL)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis, sgEpochLength)
import Cardano.Node.Configuration.POM
  ( NodeConfiguration (..)
  , PartialNodeConfiguration (..)
  , defaultPartialNodeConfiguration
  , makeNodeConfiguration
  , parseNodeConfigurationFP
  )
import Cardano.Node.Handlers.Shutdown (ShutdownConfig (..))
import Cardano.Node.Protocol (ProtocolInstantiationError)
import qualified Cardano.Node.Protocol as Node (mkConsensusProtocol)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import Cardano.Node.Types
  ( ConfigYamlFilePath (..)
  , ProtocolFilepaths (..)
  )
import Cardano.Slotting.Slot (EpochSize)
import qualified Cardano.Tools.DBSynthesizer.Run as Consensus (synthesize)
import Cardano.Tools.DBSynthesizer.Types (DBSynthesizerOptions, ForgeResult)
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
  ( CardanoProtocolParams (..)
  )

import Control.Applicative (Const (..))
import Control.Exception (Exception (..), throwIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Monoid (Last (..))

import Ouroboros.Consensus.Block.Forging (MkBlockForging)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import Control.Tracer (Tracer)

-- | Something went wrong turning a node configuration (plus credentials) into a
-- forging-capable Cardano protocol.
data DBSynthesizerException
  = -- | The configuration file could not be parsed or assembled.
    DBSynthesizerConfigError String
  | -- | The protocol could not be instantiated from the configuration.
    DBSynthesizerProtocolError ProtocolInstantiationError
  | -- | The configuration resolved to a non-Cardano protocol, which the
    -- synthesizer does not support.
    DBSynthesizerNotCardano
  deriving Show

instance Exception DBSynthesizerException

-- | Build the ready-made 'ProtocolInfo', block forgers and 'EpochSize' that
-- 'Consensus.synthesize' needs, from a node configuration file and forging
-- credentials. This is the ejected @initialize@, now built on the node.
initializeProtocol ::
  -- | Path to the node's @config.json@.
  FilePath ->
  -- | Forging credentials (KES\/VRF\/opcert or bulk creds).
  ProtocolFilepaths ->
  IO
    ( ProtocolInfo (CardanoBlock StandardCrypto)
    , Tracer IO KESAgentClientTrace ->
      IO [MkBlockForging IO (CardanoBlock StandardCrypto)]
    , EpochSize
    )
initializeProtocol configFp protocolFiles = do
  nc <-
    either (throwIO . DBSynthesizerConfigError) pure
      =<< mkNodeConfig configFp protocolFiles
  someProto <-
    either (throwIO . DBSynthesizerProtocolError) pure
      =<< runExceptT (Node.mkConsensusProtocol (ncProtocolConfig nc) (Just (ncProtocolFiles nc)))
  case someProto of
    SomeConsensusProtocol CardanoBlockType runP -> do
      (protoInfo, mkForgers) <- Api.protocolInfo @IO runP
      pure (protoInfo, mkForgers, sgEpochLength (shelleyGenesisOf runP))
    SomeConsensusProtocol{} -> throwIO DBSynthesizerNotCardano

-- | Forge a ChainDB from a node configuration file, credentials and forge
-- options — the whole @config -> protocol -> forge@ pipeline the standalone
-- @db-synthesizer@ executable runs. No transactions are injected.
synthesizeFromConfig ::
  -- | Path to the node's @config.json@.
  FilePath ->
  -- | Forging credentials.
  ProtocolFilepaths ->
  DBSynthesizerOptions ->
  -- | Directory of the ChainDB to forge into.
  FilePath ->
  IO ForgeResult
synthesizeFromConfig configFp protocolFiles opts dbDir = do
  (protoInfo, mkForgers, epochSize) <- initializeProtocol configFp protocolFiles
  Consensus.synthesize genTxs opts epochSize dbDir (protoInfo, mkForgers)
 where
  genTxs _ _ _ _ = pure []

-- | Assemble a 'NodeConfiguration' from a config file, injecting the given
-- forging credentials (which can only otherwise be supplied on the node command
-- line) and the defaults the synthesizer needs.
mkNodeConfig :: FilePath -> ProtocolFilepaths -> IO (Either String NodeConfiguration)
mkNodeConfig configFp protocolFiles = do
  configYamlPc <- parseNodeConfigurationFP (Just configYaml)
  pure $ makeNodeConfiguration (configYamlPc <> filesPc)
 where
  configYaml = ConfigYamlFilePath configFp
  filesPc =
    defaultPartialNodeConfiguration
      { pncProtocolFiles = Last (Just protocolFiles)
      , pncValidateDB = Last (Just False)
      , pncShutdownConfig = Last (Just (ShutdownConfig Nothing Nothing))
      , pncConfigFile = Last (Just configYaml)
      }

-- | Extract the Shelley genesis from a Cardano protocol's transition config.
-- Total for the Cardano protocol; the caller has already matched
-- 'CardanoBlockType'.
shelleyGenesisOf :: ProtocolInfoArgs IO (CardanoBlock StandardCrypto) -> ShelleyGenesis
shelleyGenesisOf (ProtocolInfoArgsCardano _ Consensus.CardanoProtocolParams{Consensus.cardanoLedgerTransitionConfig = transCfg}) =
  getConst $ Ledger.tcShelleyGenesisL Const transCfg
