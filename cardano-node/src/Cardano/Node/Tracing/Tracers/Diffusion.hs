{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Diffusion
  () where

import           Cardano.Logging
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))

import           Cardano.Node.Configuration.TopologyP2P (UseLedger (..))

import qualified Data.List as List
import qualified Ouroboros.Network.Diffusion as ND
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..), PoolStake (..),
                   TraceLedgerPeers (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS


--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, Show peer) =>
    LogFormatting (WithMuxBearer peer MuxTrace) where
    forMachine dtal (WithMuxBearer b ev) =
      mconcat [ "kind" .= String "MuxTrace"
              , "bearer" .= forMachine dtal b
              , "event" .= showT ev ]
    forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

instance MetaTrace tr => MetaTrace (WithMuxBearer peer tr) where
    namespaceFor (WithMuxBearer _peer obj) = (nsCast . namespaceFor) obj
    severityFor ns Nothing = severityFor (nsCast ns :: Namespace tr) Nothing
    severityFor ns (Just (WithMuxBearer _peer obj)) =
      severityFor (nsCast ns) (Just obj)
    privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace tr) Nothing
    privacyFor ns (Just (WithMuxBearer _peer obj)) =
      privacyFor (nsCast ns) (Just obj)
    detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace tr) Nothing
    detailsFor ns (Just (WithMuxBearer _peer obj)) =
      detailsFor (nsCast ns) (Just obj)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace tr)
    allNamespaces = map nsCast (allNamespaces :: [Namespace tr])

instance MetaTrace MuxTrace where
    namespaceFor MuxTraceRecvHeaderStart {}       =
      Namespace [] ["RecvHeaderStart"]
    namespaceFor MuxTraceRecvHeaderEnd {}         =
      Namespace [] ["RecvHeaderEnd"]
    namespaceFor MuxTraceRecvStart {}             =
      Namespace [] ["RecvStart"]
    namespaceFor MuxTraceRecvEnd {}               =
      Namespace [] ["RecvEnd"]
    namespaceFor MuxTraceSendStart {}             =
      Namespace [] ["SendStart"]
    namespaceFor MuxTraceSendEnd                  =
      Namespace [] ["SendEnd"]
    namespaceFor MuxTraceState {}                 =
      Namespace [] ["State"]
    namespaceFor MuxTraceCleanExit {}             =
      Namespace [] ["CleanExit"]
    namespaceFor MuxTraceExceptionExit {}         =
      Namespace [] ["ExceptionExit"]
    namespaceFor MuxTraceChannelRecvStart {}      =
      Namespace [] ["ChannelRecvStart"]
    namespaceFor MuxTraceChannelRecvEnd {}        =
      Namespace [] ["ChannelRecvEnd"]
    namespaceFor MuxTraceChannelSendStart {}      =
      Namespace [] ["ChannelSendStart"]
    namespaceFor MuxTraceChannelSendEnd {}        =
      Namespace [] ["ChannelSendEnd"]
    namespaceFor MuxTraceHandshakeStart           =
      Namespace [] ["HandshakeStart"]
    namespaceFor MuxTraceHandshakeClientEnd {}    =
      Namespace [] ["HandshakeClientEnd"]
    namespaceFor MuxTraceHandshakeServerEnd       =
      Namespace [] ["HandshakeServerEnd"]
    namespaceFor MuxTraceHandshakeClientError {}  =
      Namespace [] ["HandshakeClientError"]
    namespaceFor MuxTraceHandshakeServerError {}  =
      Namespace [] ["HandshakeServerError"]
    namespaceFor MuxTraceRecvDeltaQObservation {} =
      Namespace [] ["RecvDeltaQObservation"]
    namespaceFor MuxTraceRecvDeltaQSample {}      =
      Namespace [] ["RecvDeltaQSample"]
    namespaceFor MuxTraceSDUReadTimeoutException  =
      Namespace [] ["SDUReadTimeoutException"]
    namespaceFor MuxTraceSDUWriteTimeoutException =
      Namespace [] ["SDUWriteTimeoutException"]
    namespaceFor MuxTraceStartEagerly {}          =
      Namespace [] ["StartEagerly"]
    namespaceFor MuxTraceStartOnDemand {}         =
      Namespace [] ["StartOnDemand"]
    namespaceFor MuxTraceStartedOnDemand {}       =
      Namespace [] ["StartedOnDemand"]
    namespaceFor MuxTraceTerminating {}           =
      Namespace [] ["Terminating"]
    namespaceFor MuxTraceStopping                 =
      Namespace [] ["Stopping"]
    namespaceFor MuxTraceStopped                  =
      Namespace [] ["Stopped"]
    namespaceFor MuxTraceTCPInfo {}               =
      Namespace [] ["TCPInfo"]

    severityFor (Namespace _ ["RecvHeaderStart"]) _       = Just Debug
    severityFor (Namespace _ ["RecvHeaderEnd"]) _         = Just Debug
    severityFor (Namespace _ ["RecvStart"]) _             = Just Debug
    severityFor (Namespace _ ["RecvEnd"]) _               = Just Debug
    severityFor (Namespace _ ["SendStart"]) _             = Just Debug
    severityFor (Namespace _ ["SendEnd"]) _               = Just Debug
    severityFor (Namespace _ ["State"]) _                 = Just Info
    severityFor (Namespace _ ["CleanExit"]) _             = Just Notice
    severityFor (Namespace _ ["ExceptionExit"]) _         = Just Notice
    severityFor (Namespace _ ["ChannelRecvStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelRecvEnd"]) _        = Just Debug
    severityFor (Namespace _ ["ChannelSendStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelSendEnd"]) _        = Just Debug
    severityFor (Namespace _ ["HandshakeStart"]) _        = Just Debug
    severityFor (Namespace _ ["HandshakeClientEnd"]) _    = Just Info
    severityFor (Namespace _ ["HandshakeServerEnd"]) _    = Just Debug
    severityFor (Namespace _ ["HandshakeClientError"]) _  = Just Error
    severityFor (Namespace _ ["HandshakeServerError"]) _  = Just Error
    severityFor (Namespace _ ["RecvDeltaQObservation"]) _ = Just Debug
    severityFor (Namespace _ ["RecvDeltaQSample"]) _      = Just Debug
    severityFor (Namespace _ ["SDUReadTimeoutException"]) _  = Just Notice
    severityFor (Namespace _ ["SDUWriteTimeoutException"]) _ = Just Notice
    severityFor (Namespace _ ["StartEagerly"]) _          = Just Debug
    severityFor (Namespace _ ["StartOnDemand"]) _         = Just Debug
    severityFor (Namespace _ ["StartedOnDemand"]) _       = Just Debug
    severityFor (Namespace _ ["Terminating"]) _           = Just Debug
    severityFor (Namespace _ ["Shutdown"]) _              = Just Debug
    severityFor (Namespace _ ["TCPInfo"]) _               = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RecvHeaderStart"])       = Just
      "Bearer receive header start."
    documentFor (Namespace _ ["RecvHeaderEnd"])         = Just
      "Bearer receive header end."
    documentFor (Namespace _ ["RecvStart"])             = Just
      "Bearer receive start."
    documentFor (Namespace _ ["RecvEnd"])               = Just
      "Bearer receive end."
    documentFor (Namespace _ ["SendStart"])             = Just
      "Bearer send start."
    documentFor (Namespace _ ["SendEnd"])               = Just
      "Bearer send end."
    documentFor (Namespace _ ["State"])                 = Just
      "State."
    documentFor (Namespace _ ["CleanExit"])             = Just
      "Miniprotocol terminated cleanly."
    documentFor (Namespace _ ["ExceptionExit"])         = Just
      "Miniprotocol terminated with exception."
    documentFor (Namespace _ ["ChannelRecvStart"])      = Just
      "Channel receive start."
    documentFor (Namespace _ ["ChannelRecvEnd"])        = Just
      "Channel receive end."
    documentFor (Namespace _ ["ChannelSendStart"])      = Just
      "Channel send start."
    documentFor (Namespace _ ["ChannelSendEnd"])        = Just
      "Channel send end."
    documentFor (Namespace _ ["HandshakeStart"])        = Just
      "Handshake start."
    documentFor (Namespace _ ["HandshakeClientEnd"])    = Just
      "Handshake client end."
    documentFor (Namespace _ ["HandshakeServerEnd"])    = Just
      "Handshake server end."
    documentFor (Namespace _ ["HandshakeClientError"])  = Just
      "Handshake client error."
    documentFor (Namespace _ ["HandshakeServerError"])  = Just
      "Handshake server error."
    documentFor (Namespace _ ["RecvDeltaQObservation"]) = Just
      "Bearer DeltaQ observation."
    documentFor (Namespace _ ["RecvDeltaQSample"])      = Just
      "Bearer DeltaQ sample."
    documentFor (Namespace _ ["SDUReadTimeoutException"])  = Just
      "Timed out reading SDU."
    documentFor (Namespace _ ["SDUWriteTimeoutException"]) = Just
      "Timed out writing SDU."
    documentFor (Namespace _ ["StartEagerly"])          = Just
      "Eagerly started."
    documentFor (Namespace _ ["StartOnDemand"])         = Just
      "Preparing to start."
    documentFor (Namespace _ ["StartedOnDemand"])       = Just
      "Started on demand."
    documentFor (Namespace _ ["Terminating"])           = Just
      "Terminating."
    documentFor (Namespace _ ["Shutdown"])              = Just
      "Mux shutdown."
    documentFor (Namespace _ ["TCPInfo"])               = Just
      "TCPInfo."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["RecvHeaderStart"]
      , Namespace [] ["RecvHeaderEnd"]
      , Namespace [] ["RecvStart"]
      , Namespace [] ["RecvEnd"]
      , Namespace [] ["SendStart"]
      , Namespace [] ["SendEnd"]
      , Namespace [] ["State"]
      , Namespace [] ["CleanExit"]
      , Namespace [] ["ExceptionExit"]
      , Namespace [] ["ChannelRecvStart"]
      , Namespace [] ["ChannelRecvEnd"]
      , Namespace [] ["ChannelSendStart"]
      , Namespace [] ["ChannelSendEnd"]
      , Namespace [] ["HandshakeStart"]
      , Namespace [] ["HandshakeClientEnd"]
      , Namespace [] ["HandshakeServerEnd"]
      , Namespace [] ["HandshakeClientError"]
      , Namespace [] ["HandshakeServerError"]
      , Namespace [] ["RecvDeltaQObservation"]
      , Namespace [] ["RecvDeltaQSample"]
      , Namespace [] ["SDUReadTimeoutException"]
      , Namespace [] ["SDUWriteTimeoutException"]
      , Namespace [] ["StartEagerly"]
      , Namespace [] ["StartOnDemand"]
      , Namespace [] ["StartedOnDemand"]
      , Namespace [] ["Terminating"]
      , Namespace [] ["Shutdown"]
      , Namespace [] ["TCPInfo"]
      ]

--------------------------------------------------------------------------------
-- Handshake Tracer
--------------------------------------------------------------------------------

instance (Show adr, Show ver) => LogFormatting (NtN.HandshakeTr adr ver) where
    forMachine _dtal (WithMuxBearer b ev) =
      mconcat [ "kind" .= String "HandshakeTrace"
              , "bearer" .= show b
              , "event" .= show ev ]
    forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                        <> ". " <> showT ev

instance MetaTrace (AnyMessageAndAgency (HS.Handshake nt term)) where
    namespaceFor (AnyMessageAndAgency _stok HS.MsgProposeVersions {}) =
      Namespace [] ["ProposeVersions"]
    namespaceFor (AnyMessageAndAgency _stok HS.MsgReplyVersions {})   =
      Namespace [] ["ReplyVersions"]
    namespaceFor (AnyMessageAndAgency _stok HS.MsgAcceptVersion {})   =
      Namespace [] ["AcceptVersion"]
    namespaceFor (AnyMessageAndAgency _stok HS.MsgRefuse {})          =
      Namespace [] ["Refuse"]

    severityFor (Namespace _ ["ProposeVersions"]) _ = Just Info
    severityFor (Namespace _ ["ReplyVersions"]) _ = Just Info
    severityFor (Namespace _ ["AcceptVersion"]) _ = Just Info
    severityFor (Namespace _ ["Refuse"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ProposeVersions"]) = Just $ mconcat
      [ "Propose versions together with version parameters.  It must be"
      , " encoded to a sorted list.."
      ]
    documentFor (Namespace _ ["ReplyVersions"]) = Just $ mconcat
      [ "`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It"
      , " is not supported to explicitly send this message. It can only be"
      , " received as a copy of 'MsgProposeVersions' in a simultaneous open"
      , " scenario."
      ]
    documentFor (Namespace _ ["AcceptVersion"]) = Just $ mconcat
      [ "The remote end decides which version to use and sends chosen version."
      , "The server is allowed to modify version parameters."
      ]
    documentFor (Namespace _ ["Refuse"]) = Just
        "It refuses to run any version."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["ProposeVersions"]
      , Namespace [] ["ReplyVersions"]
      , Namespace [] ["AcceptVersion"]
      , Namespace [] ["Refuse"]
      ]


--------------------------------------------------------------------------------
-- DiffusionInit Tracer
--------------------------------------------------------------------------------

instance (Show ntnAddr, Show ntcAddr) =>
  LogFormatting (ND.DiffusionTracer ntnAddr ntcAddr) where
  forMachine _dtal (ND.RunServer sockAddr) = mconcat
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  forMachine _dtal (ND.RunLocalServer localAddress) = mconcat
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  forMachine _dtal (ND.UsingSystemdSocket localAddress) = mconcat
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack . show $ localAddress)
    ]

  forMachine _dtal (ND.CreateSystemdSocketForSnocketPath localAddress) = mconcat
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack . show $ localAddress)
    ]
  forMachine _dtal (ND.CreatedLocalSocket localAddress) = mconcat
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    ]
  forMachine _dtal (ND.ConfiguringLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ListeningLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .=  String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.LocalSocketUp localAddress fd) = mconcat
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    ]
  forMachine _dtal (ND.CreatingServerSocket socket) = mconcat
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ListeningServerSocket socket) = mconcat
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ServerSocketUp socket) = mconcat
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ConfiguringServerSocket socket) = mconcat
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.UnsupportedLocalSystemdSocket path) = mconcat
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  forMachine _dtal ND.UnsupportedReadySocketCase = mconcat
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  forMachine _dtal (ND.DiffusionErrored exception) = mconcat
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]
  forMachine _dtal (ND.SystemdSocketConfiguration config) = mconcat
    [ "kind" .= String "SystemdSocketConfiguration"
    , "path" .= String (pack (show config))
    ]

instance MetaTrace (ND.DiffusionTracer ntnAddr ntcAddr) where
    namespaceFor ND.RunServer {} =
      Namespace [] ["RunServer"]
    namespaceFor ND.RunLocalServer {} =
      Namespace [] ["RunLocalServer"]
    namespaceFor ND.UsingSystemdSocket {} =
      Namespace [] ["UsingSystemdSocket"]
    namespaceFor ND.CreateSystemdSocketForSnocketPath {} =
      Namespace [] ["CreateSystemdSocketForSnocketPath"]
    namespaceFor ND.CreatedLocalSocket {} =
      Namespace [] ["CreatedLocalSocket"]
    namespaceFor ND.ConfiguringLocalSocket {} =
      Namespace [] ["ConfiguringLocalSocket"]
    namespaceFor ND.ListeningLocalSocket {} =
      Namespace [] ["ListeningLocalSocket"]
    namespaceFor ND.LocalSocketUp {} =
      Namespace [] ["LocalSocketUp"]
    namespaceFor ND.CreatingServerSocket {} =
      Namespace [] ["CreatingServerSocket"]
    namespaceFor ND.ListeningServerSocket {} =
      Namespace [] ["ListeningServerSocket"]
    namespaceFor ND.ServerSocketUp {} =
      Namespace [] ["ServerSocketUp"]
    namespaceFor ND.ConfiguringServerSocket {} =
      Namespace [] ["ConfiguringServerSocket"]
    namespaceFor ND.UnsupportedLocalSystemdSocket {} =
      Namespace [] ["UnsupportedLocalSystemdSocket"]
    namespaceFor ND.UnsupportedReadySocketCase {} =
      Namespace [] ["UnsupportedReadySocketCase"]
    namespaceFor ND.DiffusionErrored {} =
      Namespace [] ["DiffusionErrored"]
    namespaceFor ND.SystemdSocketConfiguration {} =
      Namespace [] ["SystemdSocketConfiguration"]

    severityFor (Namespace _ ["RunServer"]) _ = Just Info
    severityFor (Namespace _ ["RunLocalServer"]) _ = Just Info
    severityFor (Namespace _ ["UsingSystemdSocket"]) _ = Just Info
    severityFor (Namespace _ ["CreateSystemdSocketForSnocketPath"]) _ = Just Info
    severityFor (Namespace _ ["CreatedLocalSocket"]) _ = Just Info
    severityFor (Namespace _ ["ConfiguringLocalSocket"]) _ = Just Info
    severityFor (Namespace _ ["ListeningLocalSocket"]) _ = Just Info
    severityFor (Namespace _ ["LocalSocketUp"]) _ = Just Info
    severityFor (Namespace _ ["CreatingServerSocket"]) _ = Just Info
    severityFor (Namespace _ ["ListeningServerSocket"]) _ = Just Info
    severityFor (Namespace _ ["ServerSocketUp"]) _ = Just Info
    severityFor (Namespace _ ["ConfiguringServerSocket"]) _ = Just Info
    severityFor (Namespace _ ["UnsupportedLocalSystemdSocket"]) _ = Just Warning
    severityFor (Namespace _ ["UnsupportedReadySocketCase"]) _ = Just Info
    severityFor (Namespace _ ["DiffusionErrored"]) _ = Just Critical
    severityFor (Namespace _ ["SystemdSocketConfiguration"]) _ = Just Warning
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RunServer"]) = Just
      "RunServer"
    documentFor (Namespace _ ["RunLocalServer"]) = Just
      "RunLocalServer"
    documentFor (Namespace _ ["UsingSystemdSocket"]) = Just
      "UsingSystemdSocket"
    documentFor (Namespace _ ["CreateSystemdSocketForSnocketPath"]) = Just
      "CreateSystemdSocketForSnocketPath"
    documentFor (Namespace _ ["CreatedLocalSocket"]) = Just
      "CreatedLocalSocket"
    documentFor (Namespace _ ["ConfiguringLocalSocket"]) = Just
      "ConfiguringLocalSocket"
    documentFor (Namespace _ ["ListeningLocalSocket"]) = Just
      "ListeningLocalSocket"
    documentFor (Namespace _ ["LocalSocketUp"]) = Just
      "LocalSocketUp"
    documentFor (Namespace _ ["CreatingServerSocket"]) = Just
      "CreatingServerSocket"
    documentFor (Namespace _ ["ListeningServerSocket"]) = Just
      "ListeningServerSocket"
    documentFor (Namespace _ ["ServerSocketUp"]) = Just
      "ServerSocketUp"
    documentFor (Namespace _ ["ConfiguringServerSocket"]) = Just
      "ConfiguringServerSocket"
    documentFor (Namespace _ ["UnsupportedLocalSystemdSocket"]) = Just
      "UnsupportedLocalSystemdSocket"
    documentFor (Namespace _ ["UnsupportedReadySocketCase"]) = Just
      "UnsupportedReadySocketCase"
    documentFor (Namespace _ ["DiffusionErrored"]) = Just
      "DiffusionErrored"
    documentFor (Namespace _ ["SystemdSocketConfiguration"]) = Just
      "SystemdSocketConfiguration"
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["RunServer"]
      , Namespace [] ["RunLocalServer"]
      , Namespace [] ["UsingSystemdSocket"]
      , Namespace [] ["CreateSystemdSocketForSnocketPath"]
      , Namespace [] ["CreatedLocalSocket"]
      , Namespace [] ["ConfiguringLocalSocket"]
      , Namespace [] ["ListeningLocalSocket"]
      , Namespace [] ["LocalSocketUp"]
      , Namespace [] ["CreatingServerSocket"]
      , Namespace [] ["ListeningServerSocket"]
      , Namespace [] ["ServerSocketUp"]
      , Namespace [] ["ConfiguringServerSocket"]
      , Namespace [] ["UnsupportedLocalSystemdSocket"]
      , Namespace [] ["UnsupportedReadySocketCase"]
      , Namespace [] ["DiffusionErrored"]
      , Namespace [] ["SystemdSocketConfiguration"]
      ]

--------------------------------------------------------------------------------
-- LedgerPeers Tracer
--------------------------------------------------------------------------------

instance LogFormatting TraceLedgerPeers where
  forMachine _dtal (PickedPeer addr _ackStake stake) =
    mconcat
      [ "kind" .= String "PickedPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  forMachine _dtal (PickedPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedPeers"
      , "desiredCount" .= n
      , "count" .= List.length addrs
      , "addresses" .= show addrs
      ]
  forMachine _dtal (FetchingNewLedgerState cnt) =
    mconcat
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfPools" .= cnt
      ]
  forMachine _dtal DisabledLedgerPeers =
    mconcat
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  forMachine _dtal (TraceUseLedgerAfter ula) =
    mconcat
      [ "kind" .= String "UseLedgerAfter"
      , "useLedgerAfter" .= UseLedger ula
      ]
  forMachine _dtal WaitingOnRequest =
    mconcat
      [ "kind" .= String "WaitingOnRequest"
      ]
  forMachine _dtal (RequestForPeers (NumberOfPeers np)) =
    mconcat
      [ "kind" .= String "RequestForPeers"
      , "numberOfPeers" .= np
      ]
  forMachine _dtal (ReusingLedgerState cnt age) =
    mconcat
      [ "kind" .= String "ReusingLedgerState"
      , "numberOfPools" .= cnt
      , "ledgerStateAge" .= age
      ]
  forMachine _dtal FallingBackToBootstrapPeers =
    mconcat
      [ "kind" .= String "FallingBackToBootstrapPeers"
      ]

instance MetaTrace TraceLedgerPeers where
    namespaceFor PickedPeer {} =
      Namespace [] ["PickedPeer"]
    namespaceFor PickedPeers {} =
      Namespace [] ["PickedPeers"]
    namespaceFor FetchingNewLedgerState {} =
      Namespace [] ["FetchingNewLedgerState"]
    namespaceFor DisabledLedgerPeers {} =
      Namespace [] ["DisabledLedgerPeers"]
    namespaceFor TraceUseLedgerAfter {} =
      Namespace [] ["TraceUseLedgerAfter"]
    namespaceFor WaitingOnRequest {} =
      Namespace [] ["WaitingOnRequest"]
    namespaceFor RequestForPeers {} =
      Namespace [] ["RequestForPeers"]
    namespaceFor ReusingLedgerState {} =
      Namespace [] ["ReusingLedgerState"]
    namespaceFor FallingBackToBootstrapPeers {} =
      Namespace [] ["FallingBackToBootstrapPeers"]

    severityFor (Namespace _ ["PickedPeer"]) _ = Just Debug
    severityFor (Namespace _ ["PickedPeers"]) _ = Just Info
    severityFor (Namespace _ ["FetchingNewLedgerState"]) _ = Just Info
    severityFor (Namespace _ ["DisabledLedgerPeers"]) _ = Just Info
    severityFor (Namespace _ ["TraceUseLedgerAfter"]) _ = Just Info
    severityFor (Namespace _ ["WaitingOnRequest"]) _ = Just Debug
    severityFor (Namespace _ ["RequestForPeers"]) _ = Just Debug
    severityFor (Namespace _ ["ReusingLedgerState"]) _ = Just Debug
    severityFor (Namespace _ ["FallingBackToBootstrapPeers"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["PickedPeer"]) = Just
      "Trace for a peer picked with accumulated and relative stake of its pool."
    documentFor (Namespace _ ["PickedPeers"]) = Just
      "Trace for the number of peers we wanted to pick and the list of peers picked."
    documentFor (Namespace _ ["FetchingNewLedgerState"]) = Just $ mconcat
      [ "Trace for fetching a new list of peers from the ledger. Int is the number of peers"
      , " returned."
      ]
    documentFor (Namespace _ ["DisabledLedgerPeers"]) = Just
      "Trace for when getting peers from the ledger is disabled, that is DontUseLedger."
    documentFor (Namespace _ ["TraceUseLedgerAfter"]) = Just
      "Trace UseLedgerAfter value."
    documentFor (Namespace _ ["WaitingOnRequest"]) = Just
      ""
    documentFor (Namespace _ ["RequestForPeers"]) = Just
      "RequestForPeers (NumberOfPeers 1)"
    documentFor (Namespace _ ["ReusingLedgerState"]) = Just
      ""
    documentFor (Namespace _ ["FallingBackToBootstrapPeers"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["PickedPeer"]
      , Namespace [] ["PickedPeers"]
      , Namespace [] ["FetchingNewLedgerState"]
      , Namespace [] ["DisabledLedgerPeers"]
      , Namespace [] ["TraceUseLedgerAfter"]
      , Namespace [] ["WaitingOnRequest"]
      , Namespace [] ["RequestForPeers"]
      , Namespace [] ["ReusingLedgerState"]
      , Namespace [] ["FallingBackToBootstrapPeers"]
      ]
