{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Api ()

-- import           Cardano.Logging hiding (traceWith)
-- -- import           Cardano.Node.Orphans ()
-- -- import           Cardano.Node.Queries
import           Ouroboros.Consensus.Block (Header)
-- import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainSyncClientHandle,
--                    csCandidate, cschcMap, viewChainSyncState)
-- -- import           Ouroboros.Consensus.Util.Orphans ()
-- import qualified Ouroboros.Network.AnchoredFragment as Net
-- import           Ouroboros.Network.Block (unSlotNo)
-- import qualified Ouroboros.Network.Block as Net
-- import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
-- import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..),
--                    PeerFetchStatus (..), readFetchClientState)
-- import           Ouroboros.Network.ConnectionId (remoteAddress)
-- import           Ouroboros.Network.NodeToNode (RemoteAddress)

-- import           Control.Concurrent (threadDelay)
-- import           Control.Concurrent.Async
-- -- import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
-- import           Control.Monad (forever)
-- import           "contra-tracer" Control.Tracer
-- import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
-- import           Data.Functor ((<&>))
-- import qualified Data.List as List
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import           Data.Text (Text)
-- import qualified Data.Text as Text
-- import           GHC.Conc (labelThread, myThreadId)
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.Genesis (GenesisConfigFlags (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (Flag (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))

-- import           Cardano.Logging.Types --todo
-- import           Cardano.Logging
-- import           Control.DeepSeq (NFData)
-- import qualified Data.List as List
-- import qualified Data.Set as Set
-- import           Text.Printf (printf)
-- import           Data.Aeson (FromJSON, ToJSON)
-- import           Data.Kind (Type)
-- import           Data.Text (Text)
-- import           Ouroboros.Network.ConnectionId (remoteAddress)
-- import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
-- import           Data.Time (UTCTime)
-- import           GHC.Generics (Generic)
-- import           Data.Text (Text)
-- import qualified Data.Text as Text
-- import           Ouroboros.Consensus.Block (Header)
-- import qualified Ouroboros.Network.AnchoredFragment as Net
-- import           Ouroboros.Network.Block (unSlotNo)
-- import qualified Ouroboros.Network.Block as Net
-- import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
-- import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..), PeerFetchStatus (..))
-- import           Ouroboros.Network.NodeToNode (RemoteConnectionId)
-- import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import           Data.Aeson.Types
import qualified Data.Text as Text
import           Text.Printf (PrintfArg (..))

deriving instance Eq NodeDatabasePaths
deriving instance Show NodeDatabasePaths

instance ToJSON AcceptedConnectionsLimit where
  toJSON AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit
          , acceptedConnectionsSoftLimit
          , acceptedConnectionsDelay
          } =
    object [ "AcceptedConnectionsLimit" .=
      object [ "hardLimit" .=
                  toJSON acceptedConnectionsHardLimit
             , "softLimit" .=
                  toJSON acceptedConnectionsSoftLimit
             , "delay" .=
                  toJSON acceptedConnectionsDelay
             ]
           ]

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

instance FromJSON NodeDatabasePaths where
  parseJSON o@(Object{})=
    withObject "NodeDatabasePaths"
     (\v -> MultipleDbPaths
              <$> v .: "ImmutableDbPath"
              <*> v .: "VolatileDbPath"
     ) o
  parseJSON (String s) = return . OnePathForAllDbs $ Text.unpack s
  parseJSON _ = fail "NodeDatabasePaths must be an object or a string"

deriving newtype instance FromJSON (Flag symbol)
deriving newtype instance ToJSON (Flag symbol)

instance FromJSON GenesisConfigFlags where
  parseJSON = withObject "GenesisConfigFlags" $ \v ->
    GenesisConfigFlags
      <$> v .:? "EnableCSJ"       .!= True
      <*> v .:? "EnableLoEAndGDD" .!= True
      <*> v .:? "EnableLoP"       .!= True
      <*> v .:? "BlockFetchGracePeriod"
      <*> v .:? "BucketCapacity"
      <*> v .:? "BucketRate"
      <*> v .:? "CSJJumpSize"
      <*> v .:? "GDDRateLimit"

instance LogFormatting [PeerT blk] where
  forMachine _ []       = mempty
  forMachine dtal xs    = mconcat
    [ "peers" .= toJSON (List.foldl' (\acc x -> forMachine dtal x : acc) [] xs)
    ]
  forHuman peers = Text.concat $ List.intersperse ", " (map ppPeer peers)
  asMetrics peers = [IntM "peersFromNodeKernel" (fromIntegral (length peers))]

instance LogFormatting (PeerT blk) where
  forMachine _dtal (PeerT cid _af status inflight) =
    mconcat [  "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (Text.pack . show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (Text.pack . show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (Text.pack . show . peerFetchBytesInFlight $ inflight)
             ]

instance MetaTrace [PeerT blk] where
  namespaceFor _  =
    Namespace [] ["PeersFromNodeKernel"]
  severityFor  (Namespace _ ["PeersFromNodeKernel"]) (Just []) =
    Just Debug
  severityFor  (Namespace _ ["PeersFromNodeKernel"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor (Namespace _ ["PeersFromNodeKernel"]) =
    Just ""
  documentFor _ns =
    Nothing
  metricsDocFor (Namespace _ ["PeersFromNodeKernel"]) =
    [("peersFromNodeKernel","")]
  metricsDocFor _ns = []
  allNamespaces = [ Namespace [] ["PeersFromNodeKernel"]]

-- | TODO: organize

ppMaxSlotNo :: Net.MaxSlotNo -> String
ppMaxSlotNo Net.NoMaxSlotNo   = "???"
ppMaxSlotNo (Net.MaxSlotNo x) = show (unSlotNo x)

ppStatus :: PeerFetchStatus header -> String
ppStatus = \case
  PeerFetchStatusStarting -> "starting"
  PeerFetchStatusShutdown -> "shutdown"
  PeerFetchStatusAberrant -> "aberrant"
  PeerFetchStatusBusy     -> "fetching"
  PeerFetchStatusReady {} -> "ready"

ppPeer :: PeerT blk -> Text
ppPeer (PeerT cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

ppCid :: RemoteConnectionId -> String
ppCid = takeWhile (/= ':') . show . remoteAddress

ppInFlight :: PeerFetchInFlight header -> String
ppInFlight f = printf
  "%5s  %3d  %5d  %6d"
  (ppMaxSlotNo $ peerFetchMaxSlotNo f)
  (peerFetchReqsInFlight f)
  (Set.size $ peerFetchBlocksInFlight f)
  (getSizeInBytes (peerFetchBytesInFlight f))

