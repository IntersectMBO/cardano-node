{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-error=unused-imports #-}

module Cardano.Tracer.Acceptors.Utils
  ( handshakeCodec
  , notifyAboutNodeDisconnected
  , parseHandshakeLog
  , parseSDU
  , prepareDataPointRequestor
  , prepareMetricsStores
  , removeDisconnectedNode
  , decodeConfirm
  , decodeDone
  , decodeHandshake
  , decodePropose
  , decodeToken'
  ) where

#if RTVIEW
import           Cardano.Logging (SeverityS (..))
import           Cardano.Tracer.Handlers.Notifications.Types
import           Cardano.Tracer.Handlers.Notifications.Utils
#endif
import           Cardano.Logging.Version (ForwardingVersion, forwardingVersionCodec)
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import qualified Network.Mux.Codec as Mux (decodeSDU)
import qualified Network.Mux.Trace as Mux (Error (..))
import qualified Network.Mux.Types as Mux (RemoteClockModel (..), SDU (..), SDUHeader (..))
import           Network.TypedProtocol.Codec (Codec (..), DecodeStep (..), SomeMessage (..), runDecoder)
import           Network.TypedProtocol.Core (IsActiveState (..), Protocol (..))
import           Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake (..), SingHandshake (..))
import           Ouroboros.Network.Snocket (LocalAddress)
import           Ouroboros.Network.Socket (ConnectionId (..))

import qualified Codec.CBOR.Read as CBOR (DeserialiseFailure)
import qualified Codec.CBOR.Term as CBOR (Term)

import           Control.Arrow (ArrowChoice (..))
-- This as-of-yet unused import reflects a goal to generalize from
-- monomorphic use of the IO monad to future monad-polymorphic
-- potentially pure use in the decoding functions.
import           Control.Monad.Class.MonadST () -- (MonadST)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Bimap as BM
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile, splitAt)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Time.Clock.POSIX (getPOSIXTime)
#if RTVIEW
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
#endif
import           GHC.Generics (Generic (..))
import qualified System.Metrics as EKG
import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)

import           Trace.Forward.Utils.DataPoint (DataPointRequestor, initDataPointRequestor)

handshakeCodec :: Codec (Handshake ForwardingVersion CBOR.Term) CBOR.DeserialiseFailure IO LBS.ByteString
handshakeCodec = codecHandshake forwardingVersionCodec

parseSDU :: LBS.ByteString -> Either Mux.Error (Mux.SDU, LBS.ByteString)
parseSDU (LBS.splitAt 8 -> (sduBS, bs))
  = right (, bs) $ Mux.decodeSDU sduBS

deriving instance Show Mux.RemoteClockModel
deriving instance Show Mux.SDUHeader
deriving instance Show Mux.SDU

decodeHandshake
  :: forall (st :: Handshake ForwardingVersion CBOR.Term) . ()
  => IsActiveState st (StateAgency st)
  => SingHandshake st
       -> IO (DecodeStep
             LBS.ByteString CBOR.DeserialiseFailure IO (SomeMessage st))
decodeHandshake = decode handshakeCodec

decodePropose :: 'StPropose ~ (stPropose :: Handshake ForwardingVersion CBOR.Term)
  => IsActiveState stPropose (StateAgency stPropose)
  => IO (DecodeStep LBS.ByteString CBOR.DeserialiseFailure IO (SomeMessage stPropose))
decodePropose = decodeHandshake SingPropose

decodeConfirm :: 'StConfirm ~ (stConfirm :: Handshake ForwardingVersion CBOR.Term)
  => IsActiveState stConfirm (StateAgency stConfirm)
  => IO (DecodeStep LBS.ByteString CBOR.DeserialiseFailure IO (SomeMessage stConfirm))
decodeConfirm = decodeHandshake SingConfirm

decodeDone :: 'StDone ~ (stDone :: Handshake ForwardingVersion CBOR.Term)
  => IsActiveState stDone (StateAgency stDone)
  => IO (DecodeStep LBS.ByteString CBOR.DeserialiseFailure IO (SomeMessage stDone))
decodeDone = decodeHandshake SingDone

data Either3 t t' t''
  = Left3 t
  | Middle3 t'
  | Right3 t''
  deriving (Eq, Foldable, FromJSON, ToJSON, Generic, Ord, Read, Show)

decodeToken' :: forall stConfirm stDone stPropose . ()
  => 'StConfirm ~ (stConfirm :: Handshake ForwardingVersion CBOR.Term)
  => IsActiveState stConfirm (StateAgency stConfirm)
  => 'StDone ~ (stDone :: Handshake ForwardingVersion CBOR.Term)
  => IsActiveState stDone (StateAgency stDone)
  => 'StPropose ~ (stPropose :: Handshake ForwardingVersion CBOR.Term)
  => IsActiveState stPropose (StateAgency stPropose)
  => [LBS.ByteString]
     -> IO ( Either CBOR.DeserialiseFailure (SomeMessage stConfirm)
           , Either CBOR.DeserialiseFailure (SomeMessage stDone)
           , Either CBOR.DeserialiseFailure (SomeMessage stPropose))
decodeToken' bytes = do
  confirm <- runDecoder bytes =<< decodeConfirm
  done    <- runDecoder bytes =<< decodeDone
  propose <- runDecoder bytes =<< decodePropose
  pure (confirm, done, propose)

parseHandshakeLog :: FilePath -> IO ()
parseHandshakeLog logFile = parseSDU <$> LBS.readFile logFile >>= \case
  Left msg -> print msg
  Right (sdu, cborBS) -> do
    print sdu
    print . take 1024 $ show cborBS
    -- decodeHandshake undefined
    pure ()

prepareDataPointRequestor
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO DataPointRequestor
prepareDataPointRequestor TracerEnv{teConnectedNodes, teDPRequestors} connId = do
  addConnectedNode teConnectedNodes connId
  dpRequestor <- initDataPointRequestor
  atomically $
    modifyTVar' teDPRequestors $ M.insert (connIdToNodeId connId) dpRequestor
  return dpRequestor

prepareMetricsStores
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores TracerEnv{teConnectedNodes, teAcceptedMetrics} connId = do
  addConnectedNode teConnectedNodes connId
  store <- EKG.newStore

  EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs store
  storesForNewNode <- (store ,) <$> newTVarIO emptyMetricsLocalStore

  atomically do
    modifyTVar' teAcceptedMetrics do
      M.insert (connIdToNodeId connId) storesForNewNode

  return storesForNewNode

  where
    -- forkServer definition of `getTimeMs'. The ekg frontend relies
    -- on the "ekg.server_timestamp_ms" metric being in every
    -- store. While forkServer adds that that automatically we must
    -- manually add it.
    -- url
    --  + https://github.com/tvh/ekg-wai/blob/master/System/Remote/Monitoring/Wai.hs#L237-L238
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

addConnectedNode
  :: ConnectedNodes
  -> ConnectionId LocalAddress
  -> IO ()
addConnectedNode connectedNodes connId = atomically $
  modifyTVar' connectedNodes $ S.insert (connIdToNodeId connId)

-- | This handler is called when 'runPeer' function throws an exception,
--   which means that there is a problem with network connection.
removeDisconnectedNode
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO ()
removeDisconnectedNode tracerEnv connId =
  -- Remove all the stuff related to disconnected node.
  atomically $ do
    modifyTVar' teConnectedNodes      $ S.delete  nodeId
    modifyTVar' teConnectedNodesNames $ BM.delete nodeId
    modifyTVar' teAcceptedMetrics     $ M.delete  nodeId
    modifyTVar' teDPRequestors        $ M.delete  nodeId
 where
  TracerEnv{teConnectedNodes, teConnectedNodesNames, teAcceptedMetrics, teDPRequestors} = tracerEnv
  nodeId = connIdToNodeId connId

notifyAboutNodeDisconnected
  :: TracerEnvRTView
  -> ConnectionId LocalAddress
  -> IO ()
#if RTVIEW
notifyAboutNodeDisconnected TracerEnvRTView{teEventsQueues} connId = do
  now <- systemToUTCTime <$> getSystemTime
  addNewEvent teEventsQueues EventNodeDisconnected $ Event nodeId now Warning msg
 where
  nodeId = connIdToNodeId connId
  msg = "Node is disconnected"
#else
notifyAboutNodeDisconnected _ _ = pure ()
#endif
