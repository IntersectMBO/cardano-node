{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

-- needed for instance AdjustFilePaths CardanoNetworkTopology
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Configuration.TopologyP2P
  ( readTopologyFile
  , readPeerSnapshotFile
  , readTopologyFileOrError
  )
where

import           Cardano.Api (handleIOExceptionsLiftWith, liftEither, runExceptT, throwError)

import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot (..),
                   LedgerPeersKind(..), isLedgerPeersEnabled)
import           Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Ouroboros.Network.Diffusion.Topology (NetworkTopology(..))
import           Cardano.Network.Diffusion.Topology (CardanoNetworkTopology, isValidTrustedPeerConfiguration)
import          Ouroboros.Network.OrphanInstances ()

import           Control.Exception.Safe (Exception (..), IOException, try)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified "contra-tracer" Control.Tracer as CT
import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe (isJust, isNothing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.FilePath (takeDirectory, (</>))

instance AdjustFilePaths CardanoNetworkTopology where
  adjustFilePaths f nt@NetworkTopology{peerSnapshotPath} =
    nt{peerSnapshotPath = f <$> peerSnapshotPath}

-- | Read the `NetworkTopology` configuration from the specified file.
readTopologyFile :: ()
  => NodeConfiguration
  -> CT.Tracer IO (StartupTrace blk) -> IO (Either Text CardanoNetworkTopology)
readTopologyFile
  NodeConfiguration{ncTopologyFile=TopologyFile topologyFilePath, ncConsensusMode, ncProtocolFiles} tracer = runExceptT $ do
  bs <- handleIOExceptionsLiftWith handler $ BS.readFile topologyFilePath
  topology@NetworkTopology{useLedgerPeers, peerSnapshotPath, extraConfig} <-
    liftEither . first handlerJSON $
      eitherDecode $ LBS.fromStrict bs

  unless (isValidTrustedPeerConfiguration topology) $
    throwError handlerBootstrap

  when (isBlockProducer && isLedgerPeersEnabled useLedgerPeers) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg "Use of ledger peers is not recommended for BP operation"

  when (isJust peerSnapshotPath && not (isLedgerPeersEnabled useLedgerPeers) && isBlockProducer) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateInfo
           $ createMsg "Ledger peers and peer snapshot, although specified in the topology file, are disabled in line with recommended BP operation"

  when (inPraosMode && isJust peerSnapshotPath &&  not (isLedgerPeersEnabled useLedgerPeers)) $
    if isBlockProducer
    then liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg
           $  "Peer snapshot file specified but topology disables ledger peers - ignoring file. "
           <> "To turn off this message remove peerSnapshotFile from the topology file."
    else liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg
           $  "Peer snapshot file specified but topology disables ledger peers - ignoring file. "
           <> "To turn off this message enable the use of ledger peers or remove peerSnapshotFile from the topology file."


  when (inGenesisMode && not (isLedgerPeersEnabled useLedgerPeers) && not isBlockProducer) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg "It is recommended to use ledger peers and peer snapshot file for relay operations in Genesis mode"

  when (inGenesisMode && isNothing peerSnapshotPath && isLedgerPeersEnabled useLedgerPeers && not isBlockProducer) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg
           $  "It is recommended to specify an up-to-date ledger peer snapshot file for relay operations in Genesis mode "
           <> "when the use of ledger peers is enabled."

  -- make all relative paths in the topology file relative to the topology file location
  adjustFilePaths (takeDirectory topologyFilePath </>) <$>
    if isGenesisCompatible ncConsensusMode extraConfig
       then pure topology
       else do
         liftIO $ CT.traceWith tracer $ NetworkConfigUpdateWarning genesisIncompatible
         pure $ topology{extraConfig = DontUseBootstrapPeers}
  where
    createMsg msg =
      "Cardano.Node.Configuration.Topology.readTopologyFile: " <> msg
    handler :: IOException -> Text
    handler = Text.pack . createMsg . displayException
    handlerJSON :: String -> Text
    handlerJSON err = mconcat
      [ "Is your topology file formatted correctly? "
      , "Expecting P2P Topology file format. "
      , "The port and valency fields should be numerical. "
      , "If you specified the correct topology file "
      , "make sure that you correctly setup EnableP2P "
      , "configuration flag. "
      , Text.pack err
      ]
    genesisIncompatible
      = Text.pack . createMsg $
                     "Bootstrap peers (field 'bootstrapPeers') are not compatible "
                  <> "with Genesis syncing mode, reverting to 'DontUseBootstrapPeers'. "
                  <> "Big ledger peers will be leveraged for decentralized syncing - it "
                  <> "is recommended to provide an up-to-date big ledger peer snapshot file "
                  <> "(field 'peerSnapshotFile' in topology configuration) to facilitate "
                  <> "this process."
    handlerBootstrap :: Text
    handlerBootstrap = mconcat
      [ "You seem to have not configured any trustable peers. "
      , "This is important in order for the node to make progress "
      , "in bootstrap mode. Make sure you provide at least one bootstrap peer "
      , "source. "
      ]
    isGenesisCompatible GenesisMode UseBootstrapPeers{} = False
    isGenesisCompatible _ _ = True
    inPraosMode = ncConsensusMode == PraosMode
    inGenesisMode = ncConsensusMode == GenesisMode
    isBlockProducer = hasProtocolFile ncProtocolFiles

readTopologyFileOrError :: ()
  => NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO CardanoNetworkTopology
readTopologyFileOrError nc tr =
      readTopologyFile nc tr
  >>= either (\err -> error $ "Cardano.Node.Configuration.TopologyP2P.readTopologyFile: "
                           <> Text.unpack err)
             pure

readPeerSnapshotFile :: PeerSnapshotFile -> IO (Either Text (LedgerPeerSnapshot BigLedgerPeers))
readPeerSnapshotFile (PeerSnapshotFile file) = do
  content <- first renderException <$> try (BS.readFile file)
  return $ first handler $ content >>= eitherDecodeStrict
  where
    renderException :: IOException -> String
    renderException = displayException

    handler :: String -> Text
    handler msg =
      Text.pack
        $ "Cardano.Node.Configuration.TopologyP2P.readPeerSnapshotFile: " <> msg
