{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.LocalStateQuery
  ( LocalStateQueryError (..)
  , QueryFilter (..)
  , renderLocalStateQueryError
  , queryLocalLedgerState
  , queryUTxOFromLocalState
  , Ledger.UTxO(..)
  , queryPParamsFromLocalState
  , Ledger.PParams
  , queryStakeDistributionFromLocalState
  , Ledger.PoolDistr(..)
  ) where

import           Cardano.Prelude hiding (atomically, option, threadDelay)

import           Cardano.Api.Types (Network, toNetworkMagic, Address (..), ByronAddress, ShelleyAddress)
import           Cardano.Api.TxSubmit.Types (textShow)
import           Cardano.Binary (decodeFull)
import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import           Cardano.BM.Trace (Trace, appendName, logInfo)

import           Cardano.Config.Protocol ()
import           Cardano.Config.Shelley.Protocol (mkNodeClientProtocolTPraos)
import           Cardano.Config.Types (SocketPath (..))

import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.Class.MonadSTM.Strict
                   (MonadSTM, StrictTMVar, atomically, newEmptyTMVarM, tryPutTMVar, takeTMVar)
import           Control.Monad.Trans.Except.Extra (hoistEither, newExceptT)
import           Control.Tracer (Tracer)

import           Data.Functor.Contravariant (contramap)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)

import           Network.Mux (MuxTrace, WithMuxBearer)

import           Ouroboros.Consensus.Cardano (CodecConfig, protocolClientInfo)
import           Ouroboros.Consensus.Ledger.Abstract (Query)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Ouroboros.Network.Block (Point, Serialised (..))
import           Ouroboros.Network.Mux
                   ( AppType(..), OuroborosApplication(..),
                     MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.NodeToClient (ConnectionId, DictVersion, Handshake,
                   LocalAddress, NetworkConnectTracers (..),
                   NodeToClientProtocols (..), NodeToClientVersion, NodeToClientVersionData (..),
                   TraceSendRecv, Versions)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
                   (ClientStAcquired (..), ClientStAcquiring (..), ClientStIdle (..),
                    ClientStQuerying (..), LocalStateQueryClient(..), localStateQueryClientPeer)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))

import qualified Shelley.Spec.Ledger.PParams as Ledger (PParams)
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO(..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger (PoolDistr(..))
import qualified Shelley.Spec.Ledger.LedgerState as Ledger

-- | An error that can occur while querying a node's local state.
data LocalStateQueryError
  = AcquireFailureError !AcquireFailure
  | ByronAddressesNotSupportedError !(Set ByronAddress)
  -- ^ The query does not support Byron addresses.
  deriving (Eq, Show)

-- | UTxO query filtering options.
data QueryFilter
  = FilterByAddress !(Set Address)
  | NoFilter
  deriving (Eq, Show)

renderLocalStateQueryError :: LocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    ByronAddressesNotSupportedError byronAddrs ->
      "The attempted local state query does not support Byron addresses: " <> show byronAddrs

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryUTxOFromLocalState
  :: Network
  -> SocketPath
  -> QueryFilter
  -> Point (ShelleyBlock TPraosStandardCrypto)
  -> ExceptT LocalStateQueryError IO (Ledger.UTxO TPraosStandardCrypto)
queryUTxOFromLocalState network socketPath qFilter point = do
  utxoFilter <- hoistEither $ applyUTxOFilter qFilter
  let pointAndQuery = (point, utxoFilter)
  newExceptT $ queryNodeLocalState
    nullTracer
    (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos)
    network
    socketPath
    pointAndQuery

-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => Network
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO Ledger.PParams
queryPParamsFromLocalState network socketPath point = do
  let pointAndQuery = (point, GetCurrentPParams)
  newExceptT $ liftIO $
    queryNodeLocalState
      nullTracer
      (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos)
      network
      socketPath
      pointAndQuery

-- | Query the current stake distribution from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryStakeDistributionFromLocalState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => Network
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO (Ledger.PoolDistr TPraosStandardCrypto)
queryStakeDistributionFromLocalState network socketPath point = do
  let pointAndQuery = (point, GetStakeDistribution)
  newExceptT $ liftIO $
    queryNodeLocalState
      nullTracer
      (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos)
      network
      socketPath
      pointAndQuery

queryLocalLedgerState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => Network
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO (Either LByteString (Ledger.LedgerState TPraosStandardCrypto))
queryLocalLedgerState network socketPath point = do
  lbs <- fmap unSerialised <$>
            newExceptT . liftIO $
              queryNodeLocalState
                nullTracer
                (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos)
                network
                socketPath
                (point, GetCBOR GetCurrentLedgerState) -- Get CBOR-in-CBOR version
  -- If decode as a LedgerState fails we return the ByteString so we can do a generic
  -- CBOR decode.
  case decodeFull lbs of
    Right lstate -> pure $ Right lstate
    Left _ -> pure $ Left lbs

-- -------------------------------------------------------------------------------------------------

-- | Establish a connection to a node and execute the provided query
-- via the local state query protocol.
--
-- This one is not specific to any era.
--
queryNodeLocalState
  :: forall blk result. RunNode blk
  => Trace IO Text
  -> CodecConfig blk
  -> Network
  -> SocketPath
  -> (Point blk, Query blk result)
  -> IO (Either LocalStateQueryError result)
queryNodeLocalState trce cfg nm (SocketPath socketPath) pointAndQuery = do
    logInfo trce $ "queryNodeLocalState: Connecting to node via " <> textShow socketPath
    NodeToClient.withIOManager $ \iocp -> do
      resultVar <- newEmptyTMVarM
      NodeToClient.connectTo
        (NodeToClient.localSnocket iocp socketPath)
        NetworkConnectTracers
          { nctMuxTracer = muxTracer
          , nctHandshakeTracer = handshakeTracer
          }
        (localInitiatorNetworkApplication trce cfg nm resultVar pointAndQuery)
        socketPath
      atomically $ takeTMVar resultVar
  where
    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    handshakeTracer :: Tracer IO
                        (WithMuxBearer (ConnectionId LocalAddress)
                        (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce

localInitiatorNetworkApplication
  :: forall blk result. RunNode blk
  => Trace IO Text
  -> CodecConfig blk
  -> Network
  -> StrictTMVar IO (Either LocalStateQueryError result)
  -> (Point blk, Query blk result)
  -> Versions
      NodeToClientVersion
      DictVersion
      (OuroborosApplication 'InitiatorApp LocalAddress LByteString IO (Either LocalStateQueryError result) Void)
localInitiatorNetworkApplication trce cfg nm
                                 resultVar pointAndQuery =
    NodeToClient.foldMapVersions
      (\v ->
        NodeToClient.versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (const $ protocols v))
      (supportedNodeToClientVersions proxy)
  where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData { networkMagic = toNetworkMagic nm }

    protocols clientVersion =
        NodeToClientProtocols
          { localChainSyncProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cChainSyncCodec
                  NodeToClient.chainSyncPeerNull

          , localTxSubmissionProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cTxSubmissionCodec
                  NodeToClient.localTxSubmissionPeerNull

          , localStateQueryProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  (contramap (Text.pack . show) . toLogObject $
                    appendName "cardano-local-state-query" trce)
                  cStateQueryCodec
                  (localStateQueryClientPeer (localStateQueryClient pointAndQuery resultVar))
          }
      where
        Codecs
          { cChainSyncCodec
          , cTxSubmissionCodec
          , cStateQueryCodec
          } = defaultCodecs cfg clientVersion

-- | A 'LocalStateQueryClient' which executes the provided local state query
-- and puts the result in the provided 'StrictTMVar'.
localStateQueryClient
  :: forall block query m result. (Applicative m, MonadIO m, MonadSTM m)
  => (Point block, query result)
  -> StrictTMVar m (Either LocalStateQueryError result)
  -> LocalStateQueryClient block query m (Either LocalStateQueryError result)
localStateQueryClient (point, query) resultVar =
  LocalStateQueryClient $ pure $ SendMsgAcquire point $
    ClientStAcquiring
      { recvMsgAcquired = SendMsgQuery query $
          ClientStQuerying
            { recvMsgResult = \result -> do
                void $ atomically $ tryPutTMVar resultVar (Right result)
                pure $ SendMsgRelease $ SendMsgDone (Right result)
            }
      , recvMsgFailure = \failure -> do
          void $ atomically $ tryPutTMVar resultVar (Left $ AcquireFailureError failure)
          pure $ SendMsgDone (Left $ AcquireFailureError failure)
      }

applyUTxOFilter
  :: (blk ~ ShelleyBlock TPraosStandardCrypto, c ~ TPraosStandardCrypto)
  => QueryFilter
  -> Either LocalStateQueryError (Query blk (Ledger.UTxO c))
applyUTxOFilter qFilter =
    case qFilter of
      FilterByAddress addrs -> do shelleyAddrs <- checkAddresses $ partitionAddresses addrs
                                  Right $ GetFilteredUTxO shelleyAddrs
      NoFilter -> Right GetUTxO
  where
    checkAddresses :: (Set ByronAddress, Set ShelleyAddress) -> Either LocalStateQueryError (Set ShelleyAddress)
    checkAddresses (byronAddrs, shelleyAddrs) = if Set.null byronAddrs
                                                then Right $ shelleyAddrs
                                                else Left $ ByronAddressesNotSupportedError byronAddrs


-- | Partitions a 'Set' of addresses such that Byron addresses are on the left
-- and Shelley on the right.
partitionAddresses :: Set Address -> (Set ByronAddress, Set ShelleyAddress)
partitionAddresses = partitionMap isAddressByron getByronAddress getShelleyAddress
  where
    isAddressByron :: Address -> Bool
    isAddressByron (AddressByron _) = True
    isAddressByron _ = False

    getByronAddress :: Address -> ByronAddress
    getByronAddress (AddressByron ab) = ab
    getByronAddress _ =
      panic "Cardano.Api.LocalStateQuery.partitionAddresses.getByronAddress: Impossible"

    getShelleyAddress :: Address -> ShelleyAddress
    getShelleyAddress (AddressShelley as) = as
    getShelleyAddress _ =
       panic "Cardano.Api.LocalStateQuery.partitionAddresses.getShelleyAddress: Impossible"

partitionMap :: (Ord b, Ord c) => (a -> Bool) -> (a -> b) -> (a -> c) -> Set a -> (Set b, Set c)
partitionMap cond leftFn rightFn xs = (Set.map leftFn ys, Set.map rightFn zs)
  where
    (ys, zs) = Set.partition cond xs
