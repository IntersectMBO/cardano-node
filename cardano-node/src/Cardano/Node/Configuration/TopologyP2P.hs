{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Configuration.TopologyP2P
  ( NetworkTopology(..)
  , PublicRootPeers(..)
  , LocalRootPeersGroup(..)
  , LocalRootPeersGroups(..)
  , RootConfig(..)
  , NodeHostIPAddress(..)
  , NodeHostIPv4Address(..)
  , NodeHostIPv6Address(..)
  , NodeSetup(..)
  , PeerAdvertise(..)
  , nodeAddressToSockAddr
  , readTopologyFile
  , readPeerSnapshotFile
  , readTopologyFileOrError
  )
where

import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Cardano.Diffusion.Topology
import           Ouroboros.Network.Diffusion.Topology
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot (..),
                   UseLedgerPeers (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import           Control.Applicative (Alternative (..))
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Exception.Base (Exception (..))
import           Control.Monad.Trans.Except.Extra
import qualified "contra-tracer" Control.Tracer as CT
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

data NodeSetup = NodeSetup
  { nodeId          :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers       :: ![RootConfig]
  , useLedger       :: !UseLedgerPeers
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .:  "nodeId"
                  <*> o .:  "nodeIPv4Address"
                  <*> o .:  "nodeIPv6Address"
                  <*> o .:  "producers"
                  <*> o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers

instance ToJSON NodeSetup where
  toJSON ns =
    object
      [ "nodeId"             .= nodeId ns
      , "nodeIPv4Address"    .= nodeIPv4Address ns
      , "nodeIPv6Address"    .= nodeIPv6Address ns
      , "producers"          .= producers ns
      , "useLedgerAfterSlot" .= useLedger ns
      ]

instance FromJSON RootConfig where
  parseJSON = withObject "RootConfig" $ \o ->
                RootConfig
                  <$> o .:  "accessPoints"
                  <*> o .:? "advertise" .!= DoNotAdvertisePeer

instance ToJSON RootConfig where
  toJSON ra =
    object
      [ "accessPoints" .= rootAccessPoints ra
      , "advertise"    .= rootAdvertise ra
      ]

-- | Does not use the 'FromJSON' instance of 'RootConfig', so that
-- 'accessPoints', 'advertise', 'valency' and 'warmValency' fields are attached to the
-- same object.
instance FromJSON (LocalRootPeersGroup PeerTrustable) where
  parseJSON = withObject "LocalRootPeersGroup" $ \o -> do
                hv@(HotValency v) <- o .: "valency"
                                 <|> o .: "hotValency"
                LocalRootPeersGroup
                  <$> parseJSON (Object o)
                  <*> pure hv
                  <*> o .:? "warmValency" .!= WarmValency v
                      -- deserialise via NodeDiffusionMode
                  <*> (maybe InitiatorAndResponderDiffusionMode getDiffusionMode
                        <$> o .:? "diffusionMode")
                  <*> o .:? "trustable" .!= IsNotTrustable

instance ToJSON (LocalRootPeersGroup PeerTrustable) where
  toJSON lrpg =
    object
      [ "accessPoints" .= rootAccessPoints (localRoots lrpg)
      , "advertise" .= rootAdvertise (localRoots lrpg)
      , "hotValency" .= hotValency lrpg
      , "warmValency" .= warmValency lrpg
      , "trustable" .= extraFlags lrpg
        -- serialise via NodeDiffusionMode
      , "diffusionMode" .= NodeDiffusionMode (rootDiffusionMode lrpg)
      ]

instance FromJSON (LocalRootPeersGroups PeerTrustable) where
  parseJSON = fmap LocalRootPeersGroups . parseJSONList

instance ToJSON (LocalRootPeersGroups PeerTrustable) where
  toJSON = toJSONList . groups

instance FromJSON PublicRootPeers where
  parseJSON = fmap PublicRootPeers . parseJSON

instance ToJSON PublicRootPeers where
  toJSON = toJSON . publicRoots

instance FromJSON CardanoNetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o ->
                NetworkTopology <$> (o .: "localRoots"                                  )
                                <*> (o .: "publicRoots"                                 )
                                <*> (o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers  )
                                <*> (o .:? "peerSnapshotFile"                           )
                                <*> (o .:? "bootstrapPeers" .!= DontUseBootstrapPeers   )

instance ToJSON CardanoNetworkTopology where
  toJSON top =
    case top of
      NetworkTopology { localRootPeersGroups
                       , publicRootPeers
                       , useLedgerPeers
                       , peerSnapshotPath
                       , extraConfig
                       } -> object [ "localRoots"         .= localRootPeersGroups
                                   , "publicRoots"        .= publicRootPeers
                                   , "useLedgerAfterSlot" .= useLedgerPeers
                                   , "bootstrapPeers"     .= extraConfig
                                   , "peerSnapshotFile"   .= peerSnapshotPath
                                   ]

-- | Read the `NetworkTopology` configuration from the specified file.
--
readTopologyFile :: NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO (Either Text CardanoNetworkTopology)
readTopologyFile nc tr = do
  eBs <- Exception.try $ BS.readFile (unTopology $ ncTopologyFile nc)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        case eitherDecode bs' of
          Left err -> return $ Left (handlerJSON err)
          Right t
            | isValidTrustedPeerConfiguration t ->
                if isGenesisCompatible (ncConsensusMode nc) (extraConfig t)
                  then return (Right t)
                  else do
                    CT.traceWith tr $
                      NetworkConfigUpdateError genesisIncompatible
                    return . Right $ t { extraConfig = DontUseBootstrapPeers }
            | otherwise ->
                pure $ Left handlerBootstrap
  where
    handler :: IOException -> Text
    handler e = Text.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                          ++ displayException e
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
      = Text.pack $  "Cardano.Node.Configuration.Topology.readTopologyFile: "
                  <> "Bootstrap peers (field 'bootstrapPeers') are not compatible "
                  <> "with Genesis syncing mode, reverting to 'DontUseBootstrapPeers'. "
                  <> "Big ledger peers will be leveraged for decentralized syncing - it "
                  <> "is recommened to provide an up-to-date big ledger peer snapshot file "
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

readTopologyFileOrError :: NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO CardanoNetworkTopology
readTopologyFileOrError nc tr =
      readTopologyFile nc tr
  >>= either (\err -> error $ "Cardano.Node.Configuration.TopologyP2P.readTopologyFile: "
                           <> Text.unpack err)
             pure

readPeerSnapshotFile :: PeerSnapshotFile -> IO LedgerPeerSnapshot
readPeerSnapshotFile (PeerSnapshotFile psf) = either error pure =<< runExceptT
  (handleLeftT (left . ("Cardano.Node.Configuration.TopologyP2P.readPeerSnapshotFile: " <>)) $ do
    bs <- BS.readFile psf `catchIOExceptT` displayException
    hoistEither (eitherDecode . LBS.fromStrict $ bs))
