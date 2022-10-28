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
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..),
                   PoolStake (..), TraceLedgerPeers (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (Message (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import           Ouroboros.Network.Snocket (LocalAddress (..))


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
severityMux' MuxTraceTCPInfo {}               = Debug

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
namesForMux' MuxTraceTCPInfo {}               = ["TCPInfo"]



instance (LogFormatting peer, Show peer) =>
    LogFormatting (WithMuxBearer peer MuxTrace) where
  forMachine dtal (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "MuxTrace"
             , "bearer" .= forMachine dtal b
             , "event" .= showT ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev


docMuxLocal :: Documented (WithMuxBearer (NtN.ConnectionId LocalAddress) MuxTrace)
docMuxLocal = addDocumentedNamespace  [] docMux'

docMuxRemote :: Documented (WithMuxBearer (NtN.ConnectionId NtN.RemoteAddress) MuxTrace)
docMuxRemote = addDocumentedNamespace  [] docMux'


docMux' :: Documented (WithMuxBearer peer MuxTrace)
docMux' = Documented [
      DocMsg
        ["RecvHeaderStart"]
        []
        "Bearer receive header start."
    , DocMsg
        ["RecvHeaderEnd"]
        []
        "Bearer receive header end."
    , DocMsg
        ["RecvStart"]
        []
        "Bearer receive start."
    , DocMsg
        ["RecvEnd"]
        []
        "Bearer receive end."
    , DocMsg
        ["SendStart"]
        []
        "Bearer send start."
    , DocMsg
        ["SendEnd"]
        []
        "Bearer send end."
    , DocMsg
        ["State"]
        []
        "State."
    , DocMsg
        ["CleanExit"]
        []
        "Miniprotocol terminated cleanly."
    , DocMsg
        ["ExceptionExit"]
        []
        "Miniprotocol terminated with exception."
    , DocMsg
        ["ChannelRecvStart"]
        []
        "Channel receive start."
    , DocMsg
        ["ChannelRecvEnd"]
        []
        "Channel receive end."
    , DocMsg
        ["ChannelSendStart"]
        []
        "Channel send start."
    , DocMsg
        ["ChannelSendEnd"]
        []
        "Channel send end."
    , DocMsg
        ["HandshakeStart"]
        []
        "Handshake start."
    , DocMsg
        ["HandshakeClientEnd"]
        []
        "Handshake client end."
    , DocMsg
        ["HandshakeServerEnd"]
        []
        "Handshake server end."
    , DocMsg
        ["HandshakeClientError"]
        []
        "Handshake client error."
    , DocMsg
        ["HandshakeServerError"]
        []
        "Handshake server error."
    , DocMsg
        ["RecvDeltaQObservation"]
        []
        "Bearer DeltaQ observation."
    , DocMsg
        ["RecvDeltaQSample"]
        []
        "Bearer DeltaQ sample."
    , DocMsg
        ["SDUReadTimeoutException"]
        []
        "Timed out reading SDU."
    , DocMsg
        ["SDUWriteTimeoutException"]
        []
        "Timed out writing SDU."
    , DocMsg
        ["StartEagerly"]
        []
        "Eagerly started."
    , DocMsg
        ["StartOnDemand"]
        []
        "Preparing to start."
    , DocMsg
        ["StartedOnDemand"]
        []
        "Started on demand."
    , DocMsg
        ["Terminating"]
        []
        "Terminating."
    , DocMsg
        ["Shutdown"]
        []
        "Mux shutdown."
    , DocMsg
        ["TCPInfo"]
        []
        "TCPInfo."
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
namesForHandshake' (TraceSendMsg m) = "Send" : namesForHandshake'' m
namesForHandshake' (TraceRecvMsg m) = "Receive" : namesForHandshake'' m

namesForHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
namesForHandshake'' (AnyMessageAndAgency _agency msg) = namesForHandshake''' msg

namesForHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
namesForHandshake''' HS.MsgProposeVersions {} = ["ProposeVersions"]
namesForHandshake''' HS.MsgReplyVersions {}   = ["ReplyVersions"]
namesForHandshake''' HS.MsgAcceptVersion {}   = ["AcceptVersion"]
namesForHandshake''' HS.MsgRefuse {}          = ["Refuse"]

instance LogFormatting (NtN.HandshakeTr NtN.RemoteAddress NtN.NodeToNodeVersion) where
  forMachine _dtal (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

docHandshake :: Documented (NtN.HandshakeTr NtN.RemoteAddress ver)
docHandshake = addDocumentedNamespace  ["Send"] docHandshake'
               `addDocs` addDocumentedNamespace  ["Receive"] docHandshake'

docHandshake' :: Documented (NtN.HandshakeTr adr ver)
docHandshake' = Documented [
      DocMsg
        ["ProposeVersions"]
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        ["ReplyVersions"]
        []
        "`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It\
        \ is not supported to explicitly send this message. It can only be\
        \ received as a copy of 'MsgProposeVersions' in a simultaneous open\
        \ scenario."
    , DocMsg
        ["AcceptVersion"]
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        ["Refuse"]
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
namesForLocalHandshake' (TraceSendMsg m) = "Send" : namesForLocalHandshake'' m
namesForLocalHandshake' (TraceRecvMsg m) = "Receive" : namesForLocalHandshake'' m

namesForLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
namesForLocalHandshake'' (AnyMessageAndAgency _agency msg) = namesForLocalHandshake''' msg

namesForLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
namesForLocalHandshake''' HS.MsgProposeVersions {} = ["ProposeVersions"]
namesForLocalHandshake''' HS.MsgReplyVersions {}   = ["ReplyVersions"]
namesForLocalHandshake''' HS.MsgAcceptVersion {}   = ["AcceptVersion"]
namesForLocalHandshake''' HS.MsgRefuse {}          = ["Refuse"]

instance LogFormatting (NtC.HandshakeTr NtC.LocalAddress NtC.NodeToClientVersion) where
  forMachine _dtal (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

docLocalHandshake :: Documented (NtC.HandshakeTr LocalAddress ver)
docLocalHandshake = addDocumentedNamespace  ["Send"] docHandshake'
               `addDocs` addDocumentedNamespace  ["Receive"] docHandshake'

--------------------------------------------------------------------------------
-- DiffusionInit Tracer
--------------------------------------------------------------------------------

severityDiffusionInit :: ND.DiffusionTracer rard ladr -> SeverityS
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
severityDiffusionInit ND.UnsupportedLocalSystemdSocket {}     = Warning
severityDiffusionInit ND.UnsupportedReadySocketCase {}        = Info
severityDiffusionInit ND.DiffusionErrored {}                  = Critical
severityDiffusionInit ND.SystemdSocketConfiguration {}        = Warning

namesForDiffusionInit  :: ND.DiffusionTracer rard ladr -> [Text]
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
namesForDiffusionInit  ND.SystemdSocketConfiguration {}        =
  ["SystemdSocketConfiguration"]

instance (Show ntnAddr, Show ntcAddr) =>
  LogFormatting (ND.DiffusionTracer ntnAddr ntcAddr)  where
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

docDiffusionInit :: Documented (ND.DiffusionTracer Socket.SockAddr NtC.LocalAddress)
docDiffusionInit =  addDocumentedNamespace  [] docDiffusionInit'

docDiffusionInit' :: Documented (ND.DiffusionTracer Socket.SockAddr NtC.LocalAddress)
docDiffusionInit' = Documented [
    DocMsg
      ["RunServer"]
      []
      "RunServer "
  , DocMsg
      ["RunLocalServer"]
      []
      "RunLocalServer "
  , DocMsg
     ["UsingSystemdSocket"]
      []
      "UsingSystemdSocket "
  , DocMsg
     ["CreateSystemdSocketForSnocketPath"]
      []
      "CreateSystemdSocketForSnocketPath "
  , DocMsg
      ["CreatedLocalSocket"]
      []
      "CreatedLocalSocket "
  , DocMsg
      ["ConfiguringLocalSocket"]
      []
      "ConfiguringLocalSocket "
  , DocMsg
      ["ListeningLocalSocket"]
      []
      "ListeningLocalSocket "
  , DocMsg
      ["LocalSocketUp"]
      []
      "LocalSocketUp "
  , DocMsg
      ["CreatingServerSocket"]
      []
      "CreatingServerSocket "
  , DocMsg
      ["ConfiguringServerSocket"]
      []
      "ConfiguringServerSocket "
  , DocMsg
      ["ListeningServerSocket"]
      []
      "ListeningServerSocket "
  , DocMsg
      ["ServerSocketUp"]
      []
      "ServerSocketUp "
  , DocMsg
      ["UnsupportedLocalSystemdSocket"]
      []
      "UnsupportedLocalSystemdSocket "
  , DocMsg
      ["UnsupportedReadySocketCase"]
      []
      "UnsupportedReadySocketCase "
  , DocMsg
      ["DiffusionErrored"]
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
    mconcat
      [ "kind" .= String "PickedPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  forMachine _dtal (PickedPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
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

docLedgerPeers :: Documented TraceLedgerPeers
docLedgerPeers =  addDocumentedNamespace  [] docLedgerPeers'

docLedgerPeers' :: Documented TraceLedgerPeers
docLedgerPeers' = Documented [
    DocMsg
      ["PickedPeer"]
      []
      "Trace for a peer picked with accumulated and relative stake of its pool."
  , DocMsg
      ["PickedPeers"]
      []
      "Trace for the number of peers we wanted to pick and the list of peers picked."
  , DocMsg
      ["FetchingNewLedgerState"]
      []
      "Trace for fetching a new list of peers from the ledger. Int is the number of peers\
      \ returned."
  , DocMsg
      ["DisabledLedgerPeers"]
      []
      "Trace for when getting peers from the ledger is disabled, that is DontUseLedger."
  , DocMsg
      ["TraceUseLedgerAfter"]
      []
      "Trace UseLedgerAfter value."
  , DocMsg
      ["WaitingOnRequest"]
      []
      ""
  , DocMsg
      ["RequestForPeers"]
      []
      "RequestForPeers (NumberOfPeers 1)"
  , DocMsg
      ["ReusingLedgerState"]
      []
      ""
  , DocMsg
      ["FallingBackToBootstrapPeers"]
      []
      ""
  ]
