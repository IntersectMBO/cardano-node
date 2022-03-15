{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Diffusion
  (
    severityMux
  , namesForMux
  , docMuxLocal
  , docMuxRemote

  , severityHandshake
  , namesForHandshake
  , docHandshake

  , severityLocalHandshake
  , namesForLocalHandshake
  , docLocalHandshake

  , severityDiffusionInit
  , namesForDiffusionInit
  , docDiffusionInit

  , severityLedgerPeers
  , namesForLedgerPeers
  , docLedgerPeers
  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import qualified Codec.CBOR.Term as CBOR
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import           Text.Show

import           Cardano.Node.Configuration.TopologyP2P (UseLedger (..))

import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.LedgerPeers (AccPoolStake (..), NumberOfPeers (..),
                   PoolStake (..), TraceLedgerPeers (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (Message (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import           Ouroboros.Network.Snocket (LocalAddress (..))

protoRemoteAddr :: NtN.RemoteAddress
protoRemoteAddr = Socket.SockAddrUnix "loopback"

protoLocalAddress :: LocalAddress
protoLocalAddress = LocalAddress "loopback"

protoSomeException :: SomeException
protoSomeException = SomeException (AssertionFailed "just fooled")

protoPeerLocal :: NtN.ConnectionId LocalAddress
protoPeerLocal = NtN.ConnectionId protoLocalAddress protoLocalAddress

protoPeerRemote :: NtN.ConnectionId NtN.RemoteAddress
protoPeerRemote = NtN.ConnectionId protoRemoteAddr protoRemoteAddr

--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

severityMux :: WithMuxBearer peer MuxTrace -> SeverityS
severityMux (WithMuxBearer _ mt) = severityMux' mt

severityMux' :: MuxTrace -> SeverityS
severityMux' MuxTraceRecvHeaderStart {}       = Debug
severityMux' MuxTraceRecvHeaderEnd {}         = Debug
severityMux' MuxTraceRecvStart {}             = Debug
severityMux' MuxTraceRecvEnd {}               = Debug
severityMux' MuxTraceSendStart {}             = Debug
severityMux' MuxTraceSendEnd                  = Debug
severityMux' MuxTraceState {}                 = Info
severityMux' MuxTraceCleanExit {}             = Notice
severityMux' MuxTraceExceptionExit {}         = Notice
severityMux' MuxTraceChannelRecvStart {}      = Debug
severityMux' MuxTraceChannelRecvEnd {}        = Debug
severityMux' MuxTraceChannelSendStart {}      = Debug
severityMux' MuxTraceChannelSendEnd {}        = Debug
severityMux' MuxTraceHandshakeStart           = Debug
severityMux' MuxTraceHandshakeClientEnd {}    = Info
severityMux' MuxTraceHandshakeServerEnd       = Debug
severityMux' MuxTraceHandshakeClientError {}  = Error
severityMux' MuxTraceHandshakeServerError {}  = Error
severityMux' MuxTraceRecvDeltaQObservation {} = Debug
severityMux' MuxTraceRecvDeltaQSample {}      = Debug
severityMux' MuxTraceSDUReadTimeoutException  = Notice
severityMux' MuxTraceSDUWriteTimeoutException = Notice
severityMux' MuxTraceStartEagerly {}          = Debug
severityMux' MuxTraceStartOnDemand {}         = Debug
severityMux' MuxTraceStartedOnDemand {}       = Debug
severityMux' MuxTraceTerminating {}           = Debug
severityMux' MuxTraceShutdown {}              = Debug

namesForMux :: WithMuxBearer peer MuxTrace -> [Text]
namesForMux (WithMuxBearer _ mt) = namesForMux' mt

namesForMux' :: MuxTrace -> [Text]
namesForMux' MuxTraceRecvHeaderStart {}       = ["RecvHeaderStart"]
namesForMux' MuxTraceRecvHeaderEnd {}         = ["RecvHeaderEnd"]
namesForMux' MuxTraceRecvStart {}             = ["RecvStart"]
namesForMux' MuxTraceRecvEnd {}               = ["RecvEnd"]
namesForMux' MuxTraceSendStart {}             = ["SendStart"]
namesForMux' MuxTraceSendEnd                  = ["SendEnd"]
namesForMux' MuxTraceState {}                 = ["State"]
namesForMux' MuxTraceCleanExit {}             = ["CleanExit"]
namesForMux' MuxTraceExceptionExit {}         = ["ExceptionExit"]
namesForMux' MuxTraceChannelRecvStart {}      = ["ChannelRecvStart"]
namesForMux' MuxTraceChannelRecvEnd {}        = ["ChannelRecvEnd"]
namesForMux' MuxTraceChannelSendStart {}      = ["ChannelSendStart"]
namesForMux' MuxTraceChannelSendEnd {}        = ["ChannelSendEnd"]
namesForMux' MuxTraceHandshakeStart           = ["HandshakeStart "]
namesForMux' MuxTraceHandshakeClientEnd {}    = ["HandshakeClientEnd"]
namesForMux' MuxTraceHandshakeServerEnd       = ["HandshakeServerEnd"]
namesForMux' MuxTraceHandshakeClientError {}  = ["HandshakeClientError"]
namesForMux' MuxTraceHandshakeServerError {}  = ["HandshakeServerError"]
namesForMux' MuxTraceRecvDeltaQObservation {} = ["RecvDeltaQObservation"]
namesForMux' MuxTraceRecvDeltaQSample {}      = ["RecvDeltaQSample"]
namesForMux' MuxTraceSDUReadTimeoutException  = ["SDUReadTimeoutException"]
namesForMux' MuxTraceSDUWriteTimeoutException = ["SDUWriteTimeoutException"]
namesForMux' MuxTraceStartEagerly {}          = ["StartEagerly"]
namesForMux' MuxTraceStartOnDemand {}         = ["StartOnDemand"]
namesForMux' MuxTraceStartedOnDemand {}       = ["StartedOnDemand"]
namesForMux' MuxTraceTerminating {}           = ["Terminating"]
namesForMux' MuxTraceShutdown {}              = ["Shutdown"]



instance (LogFormatting peer, Show peer) =>
    LogFormatting (WithMuxBearer peer MuxTrace) where
  forMachine dtal (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= forMachine dtal b
             , "event" .= showT ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev


docMuxLocal :: Documented (WithMuxBearer (NtN.ConnectionId LocalAddress) MuxTrace)
docMuxLocal = docMux protoPeerLocal

docMuxRemote :: Documented (WithMuxBearer (NtN.ConnectionId NtN.RemoteAddress) MuxTrace)
docMuxRemote = docMux protoPeerRemote


docMux :: peer -> Documented (WithMuxBearer peer MuxTrace)
docMux peer = Documented [
      DocMsg
        (WithMuxBearer peer
          MuxTraceRecvHeaderStart)
        []
        "Bearer receive header start."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceRecvHeaderEnd anyProto))
        []
        "Bearer receive header end."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceRecvDeltaQObservation anyProto anyProto))
        []
        "Bearer DeltaQ observation."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceRecvDeltaQSample 1.0 1 1 1.0 1.0 1.0 1.0 ""))
        []
        "Bearer DeltaQ sample."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceRecvStart 1))
        []
        "Bearer receive start."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceRecvEnd 1))
        []
        "Bearer receive end."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceSendStart anyProto))
        []
        "Bearer send start."
    , DocMsg
        (WithMuxBearer peer
          MuxTraceSendEnd)
        []
        "Bearer send end."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceState anyProto))
        []
        "State."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceCleanExit anyProto anyProto))
        []
        "Miniprotocol terminated cleanly."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceExceptionExit
              anyProto anyProto anyProto))
        []
        "Miniprotocol terminated with exception."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceChannelRecvStart anyProto))
        []
        "Channel receive start."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceChannelRecvEnd anyProto 1))
        []
        "Channel receive end."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceChannelSendStart anyProto 1))
        []
        "Channel send start."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceChannelSendEnd anyProto))
        []
        "Channel send end."
    , DocMsg
        (WithMuxBearer peer
          MuxTraceHandshakeStart)
        []
        "Handshake start."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceHandshakeClientEnd anyProto))
        []
        "Handshake client end."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceHandshakeClientError protoSomeException anyProto))
        []
        "Handshake client error."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceHandshakeServerError protoSomeException))
        []
        "Handshake server error."
    , DocMsg
        (WithMuxBearer peer
          MuxTraceSDUReadTimeoutException)
        []
        "Timed out reading SDU."
    , DocMsg
        (WithMuxBearer peer
          MuxTraceSDUWriteTimeoutException)
        []
        "Timed out writing SDU."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceStartEagerly anyProto anyProto))
        []
        "Eagerly started."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceStartOnDemand anyProto anyProto))
        []
        "Preparing to start."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceStartedOnDemand anyProto anyProto))
        []
        "Started on demand."
    , DocMsg
        (WithMuxBearer peer
          (MuxTraceTerminating anyProto anyProto))
        []
        "Terminating."
    , DocMsg
        (WithMuxBearer peer
          MuxTraceShutdown)
        []
        "Mux shutdown."
  ]

--------------------------------------------------------------------------------
-- Handshake Tracer
--------------------------------------------------------------------------------

severityHandshake :: NtN.HandshakeTr adr ver -> SeverityS
severityHandshake (WithMuxBearer _ e) = severityHandshake' e

severityHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> SeverityS
severityHandshake' (TraceSendMsg m) = severityHandshake'' m
severityHandshake' (TraceRecvMsg m) = severityHandshake'' m

severityHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> SeverityS
severityHandshake'' (AnyMessageAndAgency _agency msg) = severityHandshake''' msg

severityHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> SeverityS
severityHandshake''' HS.MsgProposeVersions {} = Info
severityHandshake''' HS.MsgReplyVersions {}   = Info
severityHandshake''' HS.MsgAcceptVersion {}   = Info
severityHandshake''' HS.MsgRefuse {}          = Info

namesForHandshake :: NtN.HandshakeTr adr ver -> [Text]
namesForHandshake (WithMuxBearer _ e) = namesForHandshake' e

namesForHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> [Text]
namesForHandshake' (TraceSendMsg m) = namesForHandshake'' m
namesForHandshake' (TraceRecvMsg m) = namesForHandshake'' m

namesForHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
namesForHandshake'' (AnyMessageAndAgency _agency msg) = namesForHandshake''' msg

namesForHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
namesForHandshake''' HS.MsgProposeVersions {} = ["ProposeVersions"]
namesForHandshake''' HS.MsgReplyVersions {}   = ["ReplyVersions"]
namesForHandshake''' HS.MsgAcceptVersion {}   = ["AcceptVersion"]
namesForHandshake''' HS.MsgRefuse {}          = ["Refuse"]

instance LogFormatting (NtN.HandshakeTr NtN.RemoteAddress NtN.NodeToNodeVersion) where
  forMachine _dtal (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

docHandshake :: Documented (NtN.HandshakeTr NtN.RemoteAddress ver)
docHandshake = Documented [
      DocMsg
        (WithMuxBearer protoPeerRemote
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgProposeVersions anyProto))))
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        (WithMuxBearer protoPeerRemote
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgAcceptVersion anyProto anyProto))))
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        (WithMuxBearer protoPeerRemote
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgRefuse anyProto))))
        []
        "It refuses to run any version."
    ]

--------------------------------------------------------------------------------
-- LocalHandshake Tracer
--------------------------------------------------------------------------------

severityLocalHandshake :: NtC.HandshakeTr adr ver -> SeverityS
severityLocalHandshake (WithMuxBearer _ e) = severityLocalHandshake' e

severityLocalHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> SeverityS
severityLocalHandshake' (TraceSendMsg m) = severityLocalHandshake'' m
severityLocalHandshake' (TraceRecvMsg m) = severityLocalHandshake'' m

severityLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> SeverityS
severityLocalHandshake'' (AnyMessageAndAgency _agency msg) = severityLocalHandshake''' msg

severityLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> SeverityS
severityLocalHandshake''' HS.MsgProposeVersions {} = Info
severityLocalHandshake''' HS.MsgReplyVersions {}   = Info
severityLocalHandshake''' HS.MsgAcceptVersion {}   = Info
severityLocalHandshake''' HS.MsgRefuse {}          = Info

namesForLocalHandshake :: NtC.HandshakeTr adr ver -> [Text]
namesForLocalHandshake (WithMuxBearer _ e) = namesForLocalHandshake' e

namesForLocalHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> [Text]
namesForLocalHandshake' (TraceSendMsg m) = namesForLocalHandshake'' m
namesForLocalHandshake' (TraceRecvMsg m) = namesForLocalHandshake'' m

namesForLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
namesForLocalHandshake'' (AnyMessageAndAgency _agency msg) = namesForLocalHandshake''' msg

namesForLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
namesForLocalHandshake''' HS.MsgProposeVersions {} = ["ProposeVersions"]
namesForLocalHandshake''' HS.MsgReplyVersions {}   = ["ReplyVersions"]
namesForLocalHandshake''' HS.MsgAcceptVersion {}   = ["AcceptVersion"]
namesForLocalHandshake''' HS.MsgRefuse {}          = ["Refuse"]

instance LogFormatting (NtC.HandshakeTr NtC.LocalAddress NtC.NodeToClientVersion) where
  forMachine _dtal (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

docLocalHandshake :: Documented (NtC.HandshakeTr LocalAddress ver)
docLocalHandshake = Documented [
      DocMsg
        (WithMuxBearer protoPeerLocal
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgProposeVersions anyProto))))
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        (WithMuxBearer protoPeerLocal
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgAcceptVersion anyProto anyProto))))
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        (WithMuxBearer protoPeerLocal
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgRefuse anyProto))))
        []
        "It refuses to run any version."
    ]

--------------------------------------------------------------------------------
-- DiffusionInit Tracer
--------------------------------------------------------------------------------

severityDiffusionInit :: ND.InitializationTracer rard ladr -> SeverityS
severityDiffusionInit ND.RunServer {}                         = Info
severityDiffusionInit ND.RunLocalServer {}                    = Info
severityDiffusionInit ND.UsingSystemdSocket {}                = Info
severityDiffusionInit ND.CreateSystemdSocketForSnocketPath {} = Info
severityDiffusionInit ND.CreatedLocalSocket {}                = Info
severityDiffusionInit ND.ConfiguringLocalSocket {}            = Info
severityDiffusionInit ND.ListeningLocalSocket {}              = Info
severityDiffusionInit ND.LocalSocketUp  {}                    = Info
severityDiffusionInit ND.CreatingServerSocket {}              = Info
severityDiffusionInit ND.ConfiguringServerSocket {}           = Info
severityDiffusionInit ND.ListeningServerSocket {}             = Info
severityDiffusionInit ND.ServerSocketUp {}                    = Info
severityDiffusionInit ND.UnsupportedLocalSystemdSocket {}     = Info
severityDiffusionInit ND.UnsupportedReadySocketCase {}        = Info
severityDiffusionInit ND.DiffusionErrored {}                  = Info

namesForDiffusionInit  :: ND.InitializationTracer rard ladr -> [Text]
namesForDiffusionInit  ND.RunServer {}                         =
  ["RunServer"]
namesForDiffusionInit  ND.RunLocalServer {}                    =
  ["RunLocalServer"]
namesForDiffusionInit  ND.UsingSystemdSocket {}                =
  ["UsingSystemdSocket"]
namesForDiffusionInit  ND.CreateSystemdSocketForSnocketPath {} =
  ["CreateSystemdSocketForSnocketPath"]
namesForDiffusionInit  ND.CreatedLocalSocket {}                =
  ["CreatedLocalSocket"]
namesForDiffusionInit  ND.ConfiguringLocalSocket {}            =
  ["ConfiguringLocalSocket"]
namesForDiffusionInit  ND.ListeningLocalSocket {}              =
  ["ListeningLocalSocket"]
namesForDiffusionInit  ND.LocalSocketUp  {}                    =
  ["LocalSocketUp"]
namesForDiffusionInit  ND.CreatingServerSocket {}              =
  ["CreatingServerSocket"]
namesForDiffusionInit  ND.ConfiguringServerSocket {}           =
  ["ConfiguringServerSocket"]
namesForDiffusionInit  ND.ListeningServerSocket {}             =
  ["ListeningServerSocket"]
namesForDiffusionInit  ND.ServerSocketUp {}                    =
  ["ServerSocketUp"]
namesForDiffusionInit  ND.UnsupportedLocalSystemdSocket {}     =
  ["UnsupportedLocalSystemdSocket"]
namesForDiffusionInit  ND.UnsupportedReadySocketCase {}        =
  ["UnsupportedReadySocketCase"]
namesForDiffusionInit  ND.DiffusionErrored {}                  =
  ["DiffusionErrored"]

instance (Show ntnAddr, Show ntcAddr) =>
  LogFormatting (ND.InitializationTracer ntnAddr ntcAddr)  where
  forMachine _dtal (ND.RunServer sockAddr) = mkObject
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  forMachine _dtal (ND.RunLocalServer localAddress) = mkObject
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  forMachine _dtal (ND.UsingSystemdSocket localAddress) = mkObject
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack . show $ localAddress)
    ]

  forMachine _dtal (ND.CreateSystemdSocketForSnocketPath localAddress) = mkObject
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack . show $ localAddress)
    ]
  forMachine _dtal (ND.CreatedLocalSocket localAddress) = mkObject
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    ]
  forMachine _dtal (ND.ConfiguringLocalSocket localAddress socket) = mkObject
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ListeningLocalSocket localAddress socket) = mkObject
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .=  String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.LocalSocketUp localAddress fd) = mkObject
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    ]
  forMachine _dtal (ND.CreatingServerSocket socket) = mkObject
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ListeningServerSocket socket) = mkObject
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ServerSocketUp socket) = mkObject
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ConfiguringServerSocket socket) = mkObject
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.UnsupportedLocalSystemdSocket path) = mkObject
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  forMachine _dtal ND.UnsupportedReadySocketCase = mkObject
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  forMachine _dtal (ND.DiffusionErrored exception) = mkObject
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]

docDiffusionInit :: Documented (ND.InitializationTracer Socket.SockAddr NtC.LocalAddress)
docDiffusionInit = Documented [
    DocMsg
      (ND.RunServer (pure anyProto))
      []
      "RunServer "
  , DocMsg
      (ND.RunLocalServer anyProto)
      []
      "RunLocalServer "
  , DocMsg
      (ND.UsingSystemdSocket anyProto)
      []
      "UsingSystemdSocket "
  , DocMsg
      (ND.CreateSystemdSocketForSnocketPath anyProto)
      []
      "CreateSystemdSocketForSnocketPath "
  , DocMsg
      (ND.CreatedLocalSocket anyProto)
      []
      "CreatedLocalSocket "
  , DocMsg
      (ND.ConfiguringLocalSocket anyProto anyProto)
      []
      "ConfiguringLocalSocket "
  , DocMsg
      (ND.ListeningLocalSocket anyProto anyProto)
      []
      "ListeningLocalSocket "
  , DocMsg
      (ND.LocalSocketUp anyProto anyProto)
      []
      "LocalSocketUp "
  , DocMsg
      (ND.CreatingServerSocket anyProto)
      []
      "CreatingServerSocket "
  , DocMsg
      (ND.ConfiguringServerSocket anyProto)
      []
      "ConfiguringServerSocket "
  , DocMsg
      (ND.ListeningServerSocket anyProto)
      []
      "ListeningServerSocket "
  , DocMsg
      (ND.ServerSocketUp anyProto)
      []
      "ServerSocketUp "
  , DocMsg
      (ND.UnsupportedLocalSystemdSocket anyProto)
      []
      "UnsupportedLocalSystemdSocket "
  , DocMsg
      ND.UnsupportedReadySocketCase
      []
      "UnsupportedReadySocketCase "
  , DocMsg
      (ND.DiffusionErrored anyProto)
      []
      "DiffusionErrored "
  ]

--------------------------------------------------------------------------------
-- LedgerPeers Tracer
--------------------------------------------------------------------------------

severityLedgerPeers :: TraceLedgerPeers -> SeverityS
severityLedgerPeers PickedPeer {}                  = Debug
severityLedgerPeers PickedPeers {}                 = Info
severityLedgerPeers FetchingNewLedgerState {}      = Info
severityLedgerPeers DisabledLedgerPeers {}         = Info
severityLedgerPeers TraceUseLedgerAfter {}         = Info
severityLedgerPeers WaitingOnRequest {}            = Debug
severityLedgerPeers RequestForPeers {}             = Debug
severityLedgerPeers ReusingLedgerState {}          = Debug
severityLedgerPeers FallingBackToBootstrapPeers {} = Info

namesForLedgerPeers :: TraceLedgerPeers -> [Text]
namesForLedgerPeers PickedPeer {}                  = ["PickedPeer"]
namesForLedgerPeers PickedPeers {}                 = ["PickedPeers"]
namesForLedgerPeers FetchingNewLedgerState {}      = ["FetchingNewLedgerState"]
namesForLedgerPeers DisabledLedgerPeers {}         = ["DisabledLedgerPeers"]
namesForLedgerPeers TraceUseLedgerAfter {}         = ["TraceUseLedgerAfter"]
namesForLedgerPeers WaitingOnRequest {}            = ["WaitingOnRequest"]
namesForLedgerPeers RequestForPeers {}             = ["RequestForPeers"]
namesForLedgerPeers ReusingLedgerState {}          = ["ReusingLedgerState"]
namesForLedgerPeers FallingBackToBootstrapPeers {} = ["FallingBackToBootstrapPeers"]


instance LogFormatting TraceLedgerPeers where
  forMachine _dtal (PickedPeer addr _ackStake stake) =
    mkObject
      [ "kind" .= String "PickedPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  forMachine _dtal (PickedPeers (NumberOfPeers n) addrs) =
    mkObject
      [ "kind" .= String "PickedPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= show addrs
      ]
  forMachine _dtal (FetchingNewLedgerState cnt) =
    mkObject
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfPools" .= cnt
      ]
  forMachine _dtal DisabledLedgerPeers =
    mkObject
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  forMachine _dtal (TraceUseLedgerAfter ula) =
    mkObject
      [ "kind" .= String "UseLedgerAfter"
      , "useLedgerAfter" .= UseLedger ula
      ]
  forMachine _dtal WaitingOnRequest =
    mkObject
      [ "kind" .= String "WaitingOnRequest"
      ]
  forMachine _dtal (RequestForPeers (NumberOfPeers np)) =
    mkObject
      [ "kind" .= String "RequestForPeers"
      , "numberOfPeers" .= np
      ]
  forMachine _dtal (ReusingLedgerState cnt age) =
    mkObject
      [ "kind" .= String "ReusingLedgerState"
      , "numberOfPools" .= cnt
      , "ledgerStateAge" .= age
      ]
  forMachine _dtal FallingBackToBootstrapPeers =
    mkObject
      [ "kind" .= String "FallingBackToBootstrapPeers"
      ]


docLedgerPeers :: Documented TraceLedgerPeers
docLedgerPeers = Documented [
    DocMsg
      (PickedPeer
        (RelayAccessDomain  anyProto 1)
        (AccPoolStake 0.5)
        (PoolStake 0.5))
      []
      "Trace for a peer picked with accumulated and relative stake of its pool."
  , DocMsg
      (PickedPeers (NumberOfPeers 1) [])
      []
      "Trace for the number of peers we wanted to pick and the list of peers picked."
  , DocMsg
      (FetchingNewLedgerState 1)
      []
      "Trace for fetching a new list of peers from the ledger. Int is the number of peers\
      \ returned."
  , DocMsg
      DisabledLedgerPeers
      []
      "Trace for when getting peers from the ledger is disabled, that is DontUseLedger."
  , DocMsg
      DisabledLedgerPeers
      []
      "Trace for when getting peers from the ledger is disabled, that is DontUseLedger."
  , DocMsg
      (TraceUseLedgerAfter DontUseLedger)
      []
      "Trace UseLedgerAfter value."
  , DocMsg
      WaitingOnRequest
      []
      ""
  , DocMsg
      WaitingOnRequest
      []
      "RequestForPeers (NumberOfPeers 1)"
  , DocMsg
      (ReusingLedgerState 1 anyProto)
      []
      ""
  , DocMsg
      FallingBackToBootstrapPeers
      []
      ""
  ]
