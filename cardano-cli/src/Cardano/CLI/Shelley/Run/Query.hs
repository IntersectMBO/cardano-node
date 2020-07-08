{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Prelude (String)
import           Cardano.Prelude hiding (atomically, option, threadDelay)

import qualified Codec.CBOR.Term as CBOR
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, StrictTMVar,
                   atomically, newEmptyTMVarM, tryPutTMVar, takeTMVar)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
                   hoistEither, newExceptT)
import           Control.Tracer (Tracer)
import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor.Contravariant (contramap)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Void (Void)
import           Numeric (showEFloat)

import           Cardano.Api
import           Cardano.Api.Protocol.Shelley (mkNodeClientProtocolShelley)

import           Cardano.CLI.Shelley.Commands (QueryFilter(..))
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Shelley.Orphans ()
import           Cardano.Config.Types (SocketPath(..))

import           Cardano.Crypto.Hash.Class (getHashBytesAsHex)
import           Network.Mux (MuxMode(..), MuxTrace, WithMuxBearer)
import           Ouroboros.Consensus.Block (CodecConfig)

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Ledger.Abstract (Query)
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.Block (Point, getTipPoint)
import           Ouroboros.Network.NodeToClient (ConnectionId, DictVersion, Handshake,
                   LocalAddress, NetworkConnectTracers (..), NodeToClientProtocols (..),
                   NodeToClientVersion, NodeToClientVersionData (..), TraceSendRecv,
                   Versions, withIOManager)
import qualified Ouroboros.Network.NodeToClient as NodeToClient


import qualified Shelley.Spec.Ledger.Address as Ledger (RewardAcnt (..))
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr(..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger (PoolDistr(..))
import           Shelley.Spec.Ledger.Keys (Hash, KeyHash(..), KeyRole (..), VerKeyVRF)
import           Shelley.Spec.Ledger.LedgerState (EpochState)
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.TxData as Shelley (TxId (..), TxIn (..), TxOut (..))
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO(..))

import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Mux (OuroborosApplication(..),
                   MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
                   (ClientStAcquired (..), ClientStAcquiring (..), ClientStIdle (..),
                    ClientStQuerying (..), LocalStateQueryClient(..), localStateQueryClientPeer)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))



import           Cardano.Binary (decodeFull)
import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import           Cardano.BM.Trace (Trace, appendName, logInfo)





data ShelleyQueryCmdError
  = ShelleyQueryEnvVarSocketErr !EnvSocketError
  | NodeLocalStateQueryError !LocalStateQueryError
  | ShelleyQueryWriteProtocolParamsError !FilePath !IOException
  | ShelleyQueryWriteFilteredUTxOsError !FilePath !IOException
  | ShelleyQueryWriteStakeDistributionError !FilePath !IOException
  | ShelleyQueryWriteLedgerStateError !FilePath !IOException
  | ShelleyQueryWriteStakeAddressInfoError !FilePath !IOException
  | ShelleyHelpersError !HelpersError
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    NodeLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryWriteProtocolParamsError fp ioException ->
      "Error writing protocol parameters at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteFilteredUTxOsError fp ioException ->
      "Error writing filtered UTxOs at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteStakeDistributionError fp ioException ->
      "Error writing stake distribution at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteLedgerStateError fp ioException ->
      "Error writing ledger state at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteStakeAddressInfoError fp ioException ->
      "Error writing stake address info at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyHelpersError helpersErr -> renderHelpersError helpersErr

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters network mOutFile ->
      runQueryProtocolParameters network mOutFile
    QueryTip network mOutFile ->
      runQueryTip network mOutFile
    QueryStakeDistribution network mOutFile ->
      runQueryStakeDistribution network mOutFile
    QueryStakeAddressInfo addr network mOutFile ->
      runQueryStakeAddressInfo addr network mOutFile
    QueryLedgerState network mOutFile ->
      runQueryLedgerState network mOutFile
    QueryUTxO qFilter network mOutFile ->
      runQueryUTxO qFilter network mOutFile
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  pparams <- firstExceptT NodeLocalStateQueryError $
    queryPParamsFromLocalState network sockPath (getTipPoint tip)
  writeProtocolParameters mOutFile pparams

writeProtocolParameters :: Maybe OutputFile -> PParams -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteProtocolParamsError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  case mOutFile of
    Just (OutputFile fpath) -> liftIO . LBS.writeFile fpath $ encodePretty tip
    Nothing -> liftIO $ LBS.putStrLn (encodePretty tip)


runQueryUTxO
  :: QueryFilter
  -> Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO qfilter network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
            getLocalTip iomgr ptclClientInfo network sockPath
  filteredUtxo <- firstExceptT NodeLocalStateQueryError $
    queryUTxOFromLocalState network sockPath qfilter (getTipPoint tip)
  writeFilteredUTxOs mOutFile filteredUtxo

runQueryLedgerState
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
            getLocalTip iomgr ptclClientInfo network sockPath
  els <- firstExceptT NodeLocalStateQueryError $
                      queryLocalLedgerState network sockPath (getTipPoint tip)
  case els of
    Right lstate -> writeLedgerState mOutFile lstate
    Left lbs -> do
      liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
      firstExceptT ShelleyHelpersError $ pPrintCBOR lbs

runQueryStakeAddressInfo
  :: Address
  -> Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo addr network mOutFile = do
    sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
    let ptclClientInfo = pClientInfoCodecConfig
                       . protocolClientInfo
                       $ mkNodeClientProtocolShelley
    tip <- liftIO $ withIOManager $ \iomgr ->
      getLocalTip iomgr ptclClientInfo network sockPath
    delegsAndRwds <- firstExceptT NodeLocalStateQueryError $
      queryDelegationsAndRewardsFromLocalState
        network
        sockPath
        (Set.singleton addr)
        (getTipPoint tip)
    writeStakeAddressInfo mOutFile delegsAndRwds

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data LocalStateQueryError
  = AcquireFailureError !AcquireFailure
  | ByronAddressesNotSupportedError !(Set ByronAddress)
  -- ^ The query does not support Byron addresses.
  | NonStakeAddressesNotSupportedError !(Set Address)
  -- ^ The query does not support non-stake addresses.
  -- Associated with this error are the specific non-stake addresses that were
  -- provided.
  deriving (Eq, Show)

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile dr@(DelegationsAndRewards _delegsAndRwds) =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty dr)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteStakeAddressInfoError fpath)
        $ LBS.writeFile fpath (encodePretty dr)

writeLedgerState :: Maybe OutputFile -> EpochState TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile lstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty lstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteLedgerStateError fpath)
        $ LBS.writeFile fpath (encodePretty lstate)

writeFilteredUTxOs :: Maybe OutputFile -> Ledger.UTxO TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryWriteFilteredUTxOsError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: Ledger.UTxO TPraosStandardCrypto -> IO ()
printFilteredUTxOs (Ledger.UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Lovelace"

    printUtxo :: (Shelley.TxIn TPraosStandardCrypto, Shelley.TxOut TPraosStandardCrypto) -> IO ()
    printUtxo (Shelley.TxIn (Shelley.TxId txhash) txin , Shelley.TxOut _ (Coin coin)) =
      Text.putStrLn $
        mconcat
          [ Text.pack (show txhash)
          , textShowN 6 txin
          , textShowN 18 coin -- enough to display maxLovelaceVal
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

runQueryStakeDistribution
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  stakeDist <- firstExceptT NodeLocalStateQueryError $
    queryStakeDistributionFromLocalState network sockPath (getTipPoint tip)
  writeStakeDistribution mOutFile stakeDist

writeStakeDistribution :: Maybe OutputFile
                       -> PoolDistr TPraosStandardCrypto
                       -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) (PoolDistr stakeDist) =
    handleIOExceptT (ShelleyQueryWriteStakeDistributionError outFile) $
      LBS.writeFile outFile (encodePretty stakeDist)

writeStakeDistribution Nothing stakeDist =
    liftIO $ printStakeDistribution stakeDist

printStakeDistribution :: PoolDistr TPraosStandardCrypto -> IO ()
printStakeDistribution (PoolDistr stakeDist) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    sequence_
      [ putStrLn $ showStakeDistr poolId stakeFraction vrfKeyId
      | (poolId, (stakeFraction, vrfKeyId)) <- Map.toList stakeDist ]
  where
    title :: Text
    title =
      "                           PoolId                                 Stake frac"

    showStakeDistr :: KeyHash 'StakePool crypto
                   -> Rational
                   -> Hash crypto (VerKeyVRF crypto)
                   -> String
    showStakeDistr (KeyHash poolId) stakeFraction _vrfKeyId =
      concat
        [ BS.unpack (getHashBytesAsHex poolId)
        , "   "
        , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
-- TODO: we could show the VRF id, but it will then not fit in 80 cols
--      , show vrfKeyId
        ]


-- From Cardano.Api

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
    (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
    network
    socketPath
    pointAndQuery

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
      panic "Cardano.CLI.Shelley.Run.Query.partitionAddresses.getByronAddress: Impossible"

    getShelleyAddress :: Address -> ShelleyAddress
    getShelleyAddress (AddressShelley as) = as
    getShelleyAddress _ =
       panic "Cardano.CLI.Shelley.Run.Query.partitionAddresses.getShelleyAddress: Impossible"

partitionMap :: (Ord b, Ord c) => (a -> Bool) -> (a -> b) -> (a -> c) -> Set a -> (Set b, Set c)
partitionMap cond leftFn rightFn xs = (Set.map leftFn ys, Set.map rightFn zs)
  where
    (ys, zs) = Set.partition cond xs

getShelleyStakeCredentials
  :: Set Address
  -> Either LocalStateQueryError (Set ShelleyCredentialStaking)
getShelleyStakeCredentials = getStakeCreds
  where
    isStakeAddress :: Address -> Bool
    isStakeAddress (AddressShelleyReward (Ledger.RewardAcnt _ _)) = True
    isStakeAddress _ = False

    getStakeCred :: Address -> ShelleyCredentialStaking
    getStakeCred (AddressShelleyReward (Ledger.RewardAcnt _ cred)) = cred
    getStakeCred _ =
      panic "Cardano.CLI.Shelley.Run.Query.getShelleyStakeCredentials.getStakeCred: Impossible"

    getStakeCreds :: Set Address -> Either LocalStateQueryError (Set ShelleyCredentialStaking)
    getStakeCreds addrs = do
      let (stakeCreds, nonStakeAddrs) = partitionMap isStakeAddress getStakeCred identity addrs
      if Set.null nonStakeAddrs
      then Right stakeCreds
      else Left $ NonStakeAddressesNotSupportedError nonStakeAddrs

renderLocalStateQueryError :: LocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    ByronAddressesNotSupportedError byronAddrs ->
      "The attempted local state query does not support Byron addresses: " <> show byronAddrs
    NonStakeAddressesNotSupportedError addrs ->
      "The attempted local state query does not support non-stake addresses: " <> show addrs

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards
      (Map ShelleyRewardAccount (Maybe ShelleyVerificationKeyHashStakePool, ShelleyCoin))
  deriving Generic
  deriving newtype NoUnexpectedThunks

instance ToJSON DelegationsAndRewards where
  toJSON (DelegationsAndRewards delegsAndRwds) =
      Aeson.Object $
        Map.foldlWithKey' delegAndRwdToJson HMS.empty delegsAndRwds
    where
      delegAndRwdToJson
        :: HashMap Text Aeson.Value
        -> ShelleyRewardAccount
        -> (Maybe ShelleyVerificationKeyHashStakePool, ShelleyCoin)
        -> HashMap Text Aeson.Value
      delegAndRwdToJson acc k (d, r) =
        HMS.insert
          (addressToHex $ AddressShelleyReward k)
          (Aeson.object ["delegation" .= d, "rewardAccountBalance" .= r])
          acc

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
  -> ExceptT LocalStateQueryError IO PParams
queryPParamsFromLocalState network socketPath point = do
  let pointAndQuery = (point, GetCurrentPParams)
  newExceptT $ liftIO $
    queryNodeLocalState
      nullTracer
      (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
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
      (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
      network
      socketPath
      pointAndQuery

queryLocalLedgerState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => Network
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO (Either LByteString (Ledger.EpochState TPraosStandardCrypto))
queryLocalLedgerState network socketPath point = do
  lbs <- fmap unSerialised <$>
            newExceptT . liftIO $
              queryNodeLocalState
                nullTracer
                (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
                network
                socketPath
                (point, GetCBOR GetCurrentEpochState) -- Get CBOR-in-CBOR version
  -- If decode as a LedgerState fails we return the ByteString so we can do a generic
  -- CBOR decode.
  case decodeFull lbs of
    Right lstate -> pure $ Right lstate
    Left _ -> pure $ Left lbs

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryDelegationsAndRewardsFromLocalState
  :: Network
  -> SocketPath
  -> Set Address
  -> Point (ShelleyBlock TPraosStandardCrypto)
  -> ExceptT LocalStateQueryError IO DelegationsAndRewards
queryDelegationsAndRewardsFromLocalState network socketPath addrs point = do
    creds <- hoistEither $ getShelleyStakeCredentials addrs
    let pointAndQuery = (point, GetFilteredDelegationsAndRewardAccounts creds)
    res <- newExceptT $ liftIO $
      queryNodeLocalState
        nullTracer
        (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
        network
        socketPath
        pointAndQuery
    pure $ toDelegsAndRwds res
  where
    toDelegsAndRwds (delegs, rwdAcnts) =
      DelegationsAndRewards $
        Map.mapWithKey
          (\k v -> (Map.lookup (Ledger.getRwdCred k) delegs, v))
          rwdAcnts

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
      (OuroborosApplication 'InitiatorMode LocalAddress LByteString IO (Either LocalStateQueryError result) Void)
localInitiatorNetworkApplication trce cfg nm
                                 resultVar pointAndQuery =
    NodeToClient.foldMapVersions
      (\v ->
        NodeToClient.versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (\_ _ -> protocols v))
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
