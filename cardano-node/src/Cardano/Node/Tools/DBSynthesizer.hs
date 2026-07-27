{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Downstream home for the db-synthesizer's configuration and credential
-- machinery.
--
-- The forging engine ('Consensus.synthesize') lives in consensus and consumes a
-- @('ProtocolInfo', block forgers)@ pair plus an 'EpochSize'. Constructing those
-- from a node configuration file and on-disk forging credentials is a node
-- concern, so it is done here.
--
-- The node configuration file is parsed and resolved by the shared
-- @cardano-config@ package and then mapped to the node's own 'NodeConfiguration'
-- by the shared adapter ('cardanoConfigToNodeConfiguration'); its
-- 'ncProtocolConfig' is handed to cardano-node's protocol-instantiation
-- machinery ('Node.mkConsensusProtocol'), with the forging credentials supplied
-- separately (from the tool's CLI), and cardano-api's 'Api.protocolInfo' bridge
-- produces the forging @(ProtocolInfo, forgers)@ pair.
module Cardano.Node.Tools.DBSynthesizer
  ( DBSynthesizerException (..)
  , initializeProtocol
  , synthesizeFromConfig
  ) where

import Cardano.Api (BlockType (..), ProtocolInfoArgs (..))
import qualified Cardano.Api as Api (protocolInfo)

import qualified Cardano.Configuration as Cfg (resolveConfigurationFromFile)
import qualified Cardano.Ledger.Api.Transition as Ledger (tcShelleyGenesisL)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis, sgEpochLength)
import Cardano.Node.Configuration.CardanoConfigAdapter (cardanoConfigToNodeConfiguration)
import Cardano.Node.Configuration.POM (NodeConfiguration (..))
import Cardano.Node.Protocol (ProtocolInstantiationError)
import qualified Cardano.Node.Protocol as Node (mkConsensusProtocol)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import Cardano.Node.Types (ProtocolFilepaths)
import Cardano.Slotting.Slot (EpochSize)
import qualified Cardano.Tools.DBSynthesizer.Run as Consensus (synthesize)
import Cardano.Tools.DBSynthesizer.Types (DBSynthesizerOptions, ForgeResult)
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
  ( CardanoProtocolParams (..)
  )

import Control.Applicative (Const (..))
import Control.Exception (Exception (..), throwIO)
import Control.Monad.Trans.Except (runExceptT)

import Control.Tracer (Tracer)
import Ouroboros.Consensus.Block.Forging (MkBlockForging)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)

-- | Something went wrong turning a node configuration (plus credentials) into a
-- forging-capable Cardano protocol.
data DBSynthesizerException
  = -- | The configuration file could not be parsed/resolved by cardano-config,
    -- or adapted to the node's 'NodeConfiguration'.
    DBSynthesizerConfigError String
  | -- | The protocol could not be instantiated from the configuration.
    DBSynthesizerProtocolError ProtocolInstantiationError
  | -- | The configuration resolved to a non-Cardano protocol, which the
    -- synthesizer does not support.
    DBSynthesizerNotCardano
  deriving Show

instance Exception DBSynthesizerException

-- | Build the ready-made 'ProtocolInfo', block forgers and 'EpochSize' that
-- 'Consensus.synthesize' needs, from a node configuration file (parsed with
-- @cardano-config@, adapted to the node's 'NodeConfiguration') and forging
-- credentials (supplied separately, e.g. from the tool's CLI).
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
  cfgNc <-
    Cfg.resolveConfigurationFromFile configFp >>= \case
      Left err -> throwIO (DBSynthesizerConfigError (show err))
      Right (nc, _warnings) -> pure nc
  nodeCfg <-
    either (throwIO . DBSynthesizerConfigError) pure
      (cardanoConfigToNodeConfiguration cfgNc)
  someProto <-
    either (throwIO . DBSynthesizerProtocolError) pure
      =<< runExceptT (Node.mkConsensusProtocol (ncProtocolConfig nodeCfg) (Just protocolFiles))
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

-- | Extract the Shelley genesis from a Cardano protocol's transition config.
-- Total for the Cardano protocol; the caller has already matched
-- 'CardanoBlockType'.
shelleyGenesisOf :: ProtocolInfoArgs IO (CardanoBlock StandardCrypto) -> ShelleyGenesis
shelleyGenesisOf (ProtocolInfoArgsCardano _ Consensus.CardanoProtocolParams{Consensus.cardanoLedgerTransitionConfig = transCfg}) =
  getConst $ Ledger.tcShelleyGenesisL Const transCfg
