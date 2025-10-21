{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Diffusion
  () where


import           Cardano.Logging
import           Cardano.Node.Configuration.TopologyP2P ()

#ifdef linux_HOST_OS
import           Network.Mux.TCPInfo (StructTCPInfo (..))
#endif
import qualified Ouroboros.Network.Diffusion.Types as Diff
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..), PoolStake (..),
                   TraceLedgerPeers (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Network.Mux as Mux
import           Network.Mux.Types (SDUHeader (..), unRemoteClockModel)
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Data.Aeson (Value (String), (.=))
import qualified Data.List as List
import           Data.Text (Text, pack)
import           Data.Typeable
import           Formatting

--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, LogFormatting tr, Typeable tr) =>
    LogFormatting (Mux.WithBearer peer tr) where
    forMachine dtal (Mux.WithBearer b ev) =
      mconcat [ "kind"   .= (show . typeOf $ ev)
              , "bearer" .= forMachine dtal b
              , "event"  .= forMachine dtal ev ]
    forHuman (Mux.WithBearer b ev) = "With mux bearer " <> forHumanOrMachine b
                                      <> ". " <> forHumanOrMachine ev

instance MetaTrace tr => MetaTrace (Mux.WithBearer peer tr) where
    namespaceFor (Mux.WithBearer _peer obj) = (nsCast . namespaceFor) obj
    severityFor ns Nothing = severityFor (nsCast ns :: Namespace tr) Nothing
    severityFor ns (Just (Mux.WithBearer _peer obj)) =
      severityFor (nsCast ns) (Just obj)
    privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace tr) Nothing
    privacyFor ns (Just (Mux.WithBearer _peer obj)) =
      privacyFor (nsCast ns) (Just obj)
    detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace tr) Nothing
    detailsFor ns (Just (Mux.WithBearer _peer obj)) =
      detailsFor (nsCast ns) (Just obj)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace tr)
    allNamespaces = map nsCast (allNamespaces :: [Namespace tr])

instance LogFormatting Mux.BearerTrace where
    forMachine _dtal Mux.TraceRecvHeaderStart = mconcat
      [ "kind" .= String "Mux.TraceRecvHeaderStart"
      , "msg"  .= String "Bearer Receive Header Start"
      ]
    forMachine _dtal (Mux.TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "kind" .= String "Mux.TraceRecvHeaderStart"
      , "msg"  .=  String "Bearer Receive Header End"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal (Mux.TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) = mconcat
      [ "kind" .= String "Mux.TraceRecvDeltaQObservation"
      , "msg"  .=  String "Bearer DeltaQ observation"
      , "timeRemote" .=  String (showT ts)
      , "timeLocal" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal (Mux.TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = mconcat
      [ "kind" .= String "Mux.TraceRecvDeltaQSample"
      , "msg"  .=  String "Bearer DeltaQ Sample"
      , "duration" .=  String (showT d)
      , "packets" .= String (showT sp)
      , "sumBytes" .= String (showT so)
      , "DeltaQ_S" .= String (showT dqs)
      , "DeltaQ_VMean" .= String (showT dqvm)
      , "DeltaQ_VVar" .= String (showT dqvs)
      , "DeltaQ_estR" .= String (showT estR)
      , "sizeDist" .= String (showT sdud)
      ]
    forMachine _dtal (Mux.TraceRecvStart len) = mconcat
      [ "kind" .= String "Mux.TraceRecvStart"
      , "msg"  .= String "Bearer Receive Start"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceRecvRaw len) = mconcat
      [ "kind" .= String "Mux.TraceRecvRaw"
      , "msg"  .= String "Bearer Receive Raw"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceRecvEnd len) = mconcat
      [ "kind" .= String "Mux.TraceRecvEnd"
      , "msg"  .= String "Bearer Receive End"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceSendStart SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "kind" .= String "Mux.TraceSendStart"
      , "msg"  .= String "Bearer Send Start"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal Mux.TraceSendEnd = mconcat
      [ "kind" .= String "Mux.TraceSendEnd"
      , "msg"  .= String "Bearer Send End"
      ]
    forMachine _dtal Mux.TraceSDUReadTimeoutException = mconcat
      [ "kind" .= String "Mux.TraceSDUReadTimeoutException"
      , "msg"  .= String "Timed out reading SDU"
      ]
    forMachine _dtal Mux.TraceSDUWriteTimeoutException = mconcat
      [ "kind" .= String "Mux.TraceSDUWriteTimeoutException"
      , "msg"  .= String "Timed out writing SDU"
      ]
    forMachine _dtal Mux.TraceEmitDeltaQ = mempty
#ifdef linux_HOST_OS
    forMachine _dtal (Mux.TraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) = mconcat
      [ "kind" .= String "Mux.TraceTCPInfo"
      , "msg"  .= String "TCPInfo"
      , "rtt"  .= (fromIntegral tcpi_rtt :: Word)
      , "rttvar" .= (fromIntegral tcpi_rttvar :: Word)
      , "snd_cwnd" .= (fromIntegral tcpi_snd_cwnd :: Word)
      , "snd_mss" .= (fromIntegral tcpi_snd_mss :: Word)
      , "rcv_mss" .= (fromIntegral tcpi_rcv_mss :: Word)
      , "lost" .= (fromIntegral tcpi_lost :: Word)
      , "retrans" .= (fromIntegral tcpi_retrans :: Word)
      , "length" .= len
      ]
#else
    forMachine _dtal (Mux.TraceTCPInfo _ len) = mconcat
      [ "kind" .= String "Mux.TraceTCPInfo"
      , "msg"  .= String "TCPInfo"
      , "len"  .= String (showT len)
      ]
#endif

    forHuman Mux.TraceRecvHeaderStart =
      "Bearer Receive Header Start"
    forHuman (Mux.TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) =
      sformat ("Bearer Receive Header End: ts:" % prefixHex % "(" % shown % ") " % shown % " len " % int)
        (unRemoteClockModel mhTimestamp) mhNum mhDir mhLength
    forHuman (Mux.TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) =
      sformat ("Bearer DeltaQ observation: remote ts" % int % " local ts " % shown % " length " % int)
         (unRemoteClockModel mhTimestamp) ts mhLength
    forHuman (Mux.TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) =
      sformat ("Bearer DeltaQ Sample: duration " % fixed 3 % " packets " % int % " sumBytes "
        % int % " DeltaQ_S " % fixed 3 % " DeltaQ_VMean " % fixed 3 % "DeltaQ_VVar " % fixed 3
        % " DeltaQ_estR " % fixed 3 % " sizeDist " % string)
        d sp so dqs dqvm dqvs estR sdud
    forHuman (Mux.TraceRecvStart len) =
      sformat ("Bearer Receive Start: length " % int) len
    forHuman (Mux.TraceRecvRaw len) =
      sformat ("Bearer Receive Raw: length " % int) len
    forHuman (Mux.TraceRecvEnd len) =
      sformat ("Bearer Receive End: length " % int) len
    forHuman (Mux.TraceSendStart SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) =
      sformat ("Bearer Send Start: ts: " % prefixHex % " (" % shown % ") " % shown % " length " % int)
        (unRemoteClockModel mhTimestamp) mhNum mhDir mhLength
    forHuman Mux.TraceSendEnd =
      "Bearer Send End"
    forHuman Mux.TraceSDUReadTimeoutException =
      "Timed out reading SDU"
    forHuman Mux.TraceSDUWriteTimeoutException =
      "Timed out writing SDU"
    forHuman Mux.TraceEmitDeltaQ = mempty
#ifdef linux_HOST_OS
    forHuman (Mux.TraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) =
      sformat ("TCPInfo rtt " % int % " rttvar " % int % " snd_cwnd " % int %
               " snd_mss " % int % " rcv_mss " % int % " lost " % int %
               " retrans " % int % " len " % int)
              (fromIntegral tcpi_rtt :: Word)
              (fromIntegral tcpi_rttvar :: Word)
              (fromIntegral tcpi_snd_cwnd :: Word)
              (fromIntegral tcpi_snd_mss :: Word)
              (fromIntegral tcpi_rcv_mss :: Word)
              (fromIntegral tcpi_lost :: Word)
              (fromIntegral tcpi_retrans :: Word)
              len
#else
    forHuman (Mux.TraceTCPInfo _ len) = sformat ("TCPInfo len " % int) len
#endif

instance MetaTrace Mux.BearerTrace where
    namespaceFor Mux.TraceRecvHeaderStart {}       =
      Namespace [] ["RecvHeaderStart"]
    namespaceFor Mux.TraceRecvHeaderEnd {}         =
      Namespace [] ["RecvHeaderEnd"]
    namespaceFor Mux.TraceRecvStart {}             =
      Namespace [] ["RecvStart"]
    namespaceFor Mux.TraceRecvRaw {}               =
      Namespace [] ["RecvRaw"]
    namespaceFor Mux.TraceRecvEnd {}               =
      Namespace [] ["RecvEnd"]
    namespaceFor Mux.TraceSendStart {}             =
      Namespace [] ["SendStart"]
    namespaceFor Mux.TraceSendEnd                  =
      Namespace [] ["SendEnd"]
    namespaceFor Mux.TraceRecvDeltaQObservation {} =
      Namespace [] ["RecvDeltaQObservation"]
    namespaceFor Mux.TraceRecvDeltaQSample {}      =
      Namespace [] ["RecvDeltaQSample"]
    namespaceFor Mux.TraceSDUReadTimeoutException  =
      Namespace [] ["SDUReadTimeoutException"]
    namespaceFor Mux.TraceSDUWriteTimeoutException =
      Namespace [] ["SDUWriteTimeoutException"]
    namespaceFor Mux.TraceEmitDeltaQ               =
      Namespace [] ["TraceEmitDeltaQ"]
    namespaceFor Mux.TraceTCPInfo {}               =
      Namespace [] ["TCPInfo"]

    severityFor (Namespace _ ["RecvHeaderStart"]) _       = Just Debug
    severityFor (Namespace _ ["RecvRaw"]) _               = Just Debug
    severityFor (Namespace _ ["RecvHeaderEnd"]) _         = Just Debug
    severityFor (Namespace _ ["RecvStart"]) _             = Just Debug
    severityFor (Namespace _ ["RecvEnd"]) _               = Just Debug
    severityFor (Namespace _ ["SendStart"]) _             = Just Debug
    severityFor (Namespace _ ["SendEnd"]) _               = Just Debug
    severityFor (Namespace _ ["RecvDeltaQObservation"]) _ = Just Debug
    severityFor (Namespace _ ["RecvDeltaQSample"]) _      = Just Debug
    severityFor (Namespace _ ["SDUReadTimeoutException"]) _  = Just Notice
    severityFor (Namespace _ ["SDUWriteTimeoutException"]) _ = Just Notice
    severityFor (Namespace _ ["TCPInfo"]) _               = Just Debug
    severityFor (Namespace _ ["TraceEmitDeltaQ"]) _       = Nothing
    severityFor _ _                                       = Nothing

    documentFor (Namespace _ ["RecvHeaderStart"])       = Just
      "Bearer receive header start."
    documentFor (Namespace _ ["RecvRaw"])               = Just
      "Bearer receive raw."
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
    documentFor (Namespace _ ["RecvDeltaQObservation"]) = Just
      "Bearer DeltaQ observation."
    documentFor (Namespace _ ["RecvDeltaQSample"])      = Just
      "Bearer DeltaQ sample."
    documentFor (Namespace _ ["SDUReadTimeoutException"])  = Just
      "Timed out reading SDU."
    documentFor (Namespace _ ["SDUWriteTimeoutException"]) = Just
      "Timed out writing SDU."
    documentFor (Namespace _ ["TraceEmitDeltaQ"])       = Nothing
    documentFor (Namespace _ ["TCPInfo"])               = Just
      "TCPInfo."
    documentFor _                                       = Nothing

    allNamespaces = [
        Namespace [] ["RecvHeaderStart"]
      , Namespace [] ["RecvRaw"]
      , Namespace [] ["RecvHeaderEnd"]
      , Namespace [] ["RecvStart"]
      , Namespace [] ["RecvEnd"]
      , Namespace [] ["SendStart"]
      , Namespace [] ["SendEnd"]
      , Namespace [] ["RecvDeltaQObservation"]
      , Namespace [] ["RecvDeltaQSample"]
      , Namespace [] ["SDUReadTimeoutException"]
      , Namespace [] ["SDUWriteTimeoutException"]
      , Namespace [] ["TraceEmitDeltaQ"]
      , Namespace [] ["TCPInfo"]
      ]

instance LogFormatting Mux.ChannelTrace where
    forMachine _dtal (Mux.TraceChannelRecvStart mid) = mconcat
      [ "kind" .= String "Mux.TraceChannelRecvStart"
      , "msg"  .= String "Channel Receive Start"
      , "miniProtocolNum" .= String (showT mid)
      ]
    forMachine _dtal (Mux.TraceChannelRecvEnd mid len) = mconcat
      [ "kind" .= String "Mux.TraceChannelRecvEnd"
      , "msg"  .= String "Channel Receive End"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceChannelSendStart mid len) = mconcat
      [ "kind" .= String "Mux.TraceChannelSendStart"
      , "msg"  .= String "Channel Send Start"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceChannelSendEnd mid) = mconcat
      [ "kind" .= String "Mux.TraceChannelSendEnd"
      , "msg"  .= String "Channel Send End"
      , "miniProtocolNum" .= String (showT mid)
      ]

    forHuman (Mux.TraceChannelRecvStart mid) =
      sformat ("Channel Receive Start on " % shown) mid
    forHuman (Mux.TraceChannelRecvEnd mid len) =
      sformat ("Channel Receive End on (" % shown % ") " % int) mid len
    forHuman (Mux.TraceChannelSendStart mid len) =
      sformat ("Channel Send Start on (" % shown % ") " % int) mid len
    forHuman (Mux.TraceChannelSendEnd mid) =
      sformat ("Channel Send End on " % shown) mid

instance MetaTrace Mux.ChannelTrace where
    namespaceFor Mux.TraceChannelRecvStart {}      =
      Namespace [] ["ChannelRecvStart"]
    namespaceFor Mux.TraceChannelRecvEnd {}        =
      Namespace [] ["ChannelRecvEnd"]
    namespaceFor Mux.TraceChannelSendStart {}      =
      Namespace [] ["ChannelSendStart"]
    namespaceFor Mux.TraceChannelSendEnd {}        =
      Namespace [] ["ChannelSendEnd"]

    severityFor (Namespace _ ["ChannelRecvStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelRecvEnd"]) _        = Just Debug
    severityFor (Namespace _ ["ChannelSendStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelSendEnd"]) _        = Just Debug
    severityFor _ _                                       = Nothing

    documentFor (Namespace _ ["ChannelRecvStart"])      = Just
      "Channel receive start."
    documentFor (Namespace _ ["ChannelRecvEnd"])        = Just
      "Channel receive end."
    documentFor (Namespace _ ["ChannelSendStart"])      = Just
      "Channel send start."
    documentFor (Namespace _ ["ChannelSendEnd"])        = Just
      "Channel send end."
    documentFor _                                       = Nothing

    allNamespaces = [
        Namespace [] ["ChannelRecvStart"]
      , Namespace [] ["ChannelRecvEnd"]
      , Namespace [] ["ChannelSendStart"]
      , Namespace [] ["ChannelSendEnd"]
      ]

instance LogFormatting Mux.Trace where
    forMachine _dtal (Mux.TraceState new) = mconcat
      [ "kind" .= String "Mux.TraceState"
      , "msg"  .= String "MuxState"
      , "state" .= String (showT new)
      ]
    forMachine _dtal (Mux.TraceCleanExit mid dir) = mconcat
      [ "kind" .= String "Mux.TraceCleanExit"
      , "msg"  .= String "Miniprotocol terminated cleanly"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceExceptionExit mid dir exc) = mconcat
      [ "kind" .= String "Mux.TraceExceptionExit"
      , "msg"  .= String "Miniprotocol terminated with exception"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      , "exception" .= String (showT exc)
      ]
    forMachine _dtal (Mux.TraceStartEagerly mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartEagerly"
      , "msg"  .= String "Eagerly started"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceStartOnDemand mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartOnDemand"
      , "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceStartOnDemandAny mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartOnDemandAny"
      , "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceStartedOnDemand mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartedOnDemand"
      , "msg"  .= String "Started on demand"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceTerminating mid dir) = mconcat
      [ "kind" .= String "Mux.TraceTerminating"
      , "msg"  .= String "Terminating"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal Mux.TraceStopping = mconcat
      [ "kind" .= String "Mux.TraceStopping"
      , "msg"  .= String "Mux stopping"
      ]
    forMachine _dtal Mux.TraceStopped = mconcat
      [ "kind" .= String "Mux.TraceStopped"
      , "msg"  .= String "Mux stoppped"
      ]

    forHuman (Mux.TraceState new) =
      sformat ("State: " % shown) new
    forHuman (Mux.TraceCleanExit mid dir) =
      sformat ("Miniprotocol (" % shown % ") " % shown % " terminated cleanly")
      mid dir
    forHuman (Mux.TraceExceptionExit mid dir e) =
      sformat ("Miniprotocol (" % shown % ") " % shown %
        " terminated with exception " % shown) mid dir e
    forHuman (Mux.TraceStartEagerly mid dir) =
      sformat ("Eagerly started (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceStartOnDemand mid dir) =
      sformat ("Preparing to start (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceStartOnDemandAny mid dir) =
      sformat ("Preparing to start (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceStartedOnDemand mid dir) =
      sformat ("Started on demand (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceTerminating mid dir) =
      sformat ("Terminating (" % shown % ") in " % shown) mid dir
    forHuman Mux.TraceStopping = "Mux stopping"
    forHuman Mux.TraceStopped  = "Mux stoppped"

instance MetaTrace Mux.Trace where
    namespaceFor Mux.TraceState {}                 =
      Namespace [] ["State"]
    namespaceFor Mux.TraceCleanExit {}             =
      Namespace [] ["CleanExit"]
    namespaceFor Mux.TraceExceptionExit {}         =
      Namespace [] ["ExceptionExit"]
    namespaceFor Mux.TraceStartEagerly {}          =
      Namespace [] ["StartEagerly"]
    namespaceFor Mux.TraceStartOnDemand {}         =
      Namespace [] ["StartOnDemand"]
    namespaceFor Mux.TraceStartOnDemandAny {}      =
      Namespace [] ["StartOnDemandAny"]
    namespaceFor Mux.TraceStartedOnDemand {}       =
      Namespace [] ["StartedOnDemand"]
    namespaceFor Mux.TraceTerminating {}           =
      Namespace [] ["Terminating"]
    namespaceFor Mux.TraceStopping                 =
      Namespace [] ["Stopping"]
    namespaceFor Mux.TraceStopped                  =
      Namespace [] ["Stopped"]

    severityFor (Namespace _ ["State"]) _                 = Just Info
    severityFor (Namespace _ ["CleanExit"]) _             = Just Notice
    severityFor (Namespace _ ["ExceptionExit"]) _         = Just Notice
    severityFor (Namespace _ ["StartEagerly"]) _          = Just Debug
    severityFor (Namespace _ ["StartOnDemand"]) _         = Just Debug
    severityFor (Namespace _ ["StartOnDemandAny"]) _       = Just Debug
    severityFor (Namespace _ ["StartedOnDemand"]) _       = Just Debug
    severityFor (Namespace _ ["Terminating"]) _           = Just Debug
    severityFor (Namespace _ ["Stopping"]) _              = Just Debug
    severityFor (Namespace _ ["Stopped"]) _               = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["State"])                 = Just
      "State."
    documentFor (Namespace _ ["CleanExit"])             = Just
      "Miniprotocol terminated cleanly."
    documentFor (Namespace _ ["ExceptionExit"])         = Just
      "Miniprotocol terminated with exception."
    documentFor (Namespace _ ["StartEagerly"])          = Just
      "Eagerly started."
    documentFor (Namespace _ ["StartOnDemand"])         = Just
      "Preparing to start."
    documentFor (Namespace _ ["StartedOnDemand"])       = Just
      "Started on demand."
    documentFor (Namespace _ ["StartOnDemandAny"])      = Just
      "Start whenever any other protocol has started."
    documentFor (Namespace _ ["Terminating"])           = Just
      "Terminating."
    documentFor (Namespace _ ["Stopping"])              = Just
      "Mux shutdown."
    documentFor (Namespace _ ["Stopped"])              = Just
      "Mux shutdown."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["State"]
      , Namespace [] ["CleanExit"]
      , Namespace [] ["ExceptionExit"]
      , Namespace [] ["StartEagerly"]
      , Namespace [] ["StartOnDemand"]
      , Namespace [] ["StartOnDemandAny"]
      , Namespace [] ["StartedOnDemand"]
      , Namespace [] ["Terminating"]
      , Namespace [] ["Stopping"]
      , Namespace [] ["Stopped"]
      ]


--------------------------------------------------------------------------------
-- Handshake Tracer
--------------------------------------------------------------------------------

instance (Show term, Show ntcVersion) =>
  LogFormatting (AnyMessage (HS.Handshake ntcVersion term)) where
  forMachine _dtal (AnyMessageAndAgency stok msg) =
    mconcat [ "kind" .= String kind
            , "msg" .= (String . showT $ msg)
            , "agency" .= String (pack $ show stok)
            ]
    where
      kind = case msg of
        HS.MsgProposeVersions {} -> "ProposeVersions"
        HS.MsgReplyVersions   {} -> "ReplyVersions"
        HS.MsgQueryReply      {} -> "QueryReply"
        HS.MsgAcceptVersion   {} -> "AcceptVersion"
        HS.MsgRefuse          {} -> "Refuse"

  forHuman (AnyMessageAndAgency stok msg) =
    "Handshake (agency, message) = " <> "(" <> showT stok <> "," <> showT msg <> ")"

instance MetaTrace (AnyMessage (HS.Handshake a b)) where
  namespaceFor (AnyMessage msg) = Namespace [] $ case msg of
    HS.MsgProposeVersions {} -> ["ProposeVersions"]
    HS.MsgReplyVersions   {} -> ["ReplyVersions"]
    HS.MsgQueryReply      {} -> ["QueryReply"]
    HS.MsgAcceptVersion   {} -> ["AcceptVersion"]
    HS.MsgRefuse          {} -> ["Refuse"]

  severityFor (Namespace _ [sym]) _ = case sym of
    "ProposeVersions" -> Just Info
    "ReplyVersions"   -> Just Info
    "QueryReply"      -> Just Info
    "AcceptVersion"   -> Just Info
    "Refuse"          -> Just Info
    _otherwise        -> Nothing
  severityFor _ _ = Nothing

  documentFor (Namespace _ sym) = wrap . mconcat $ case sym of
    ["ProposeVersions"] ->
      [ "Propose versions together with version parameters.  It must be"
      , " encoded to a sorted list.."
      ]
    ["ReplyVersions"]   ->
      [ "`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It"
      , " is not supported to explicitly send this message. It can only be"
      , " received as a copy of 'MsgProposeVersions' in a simultaneous open"
      , " scenario."
      ]
    ["QueryReply"]      ->
      [ "`MsgQueryReply` received as a response to a handshake query in "
      , " 'MsgProposeVersions' and lists the supported versions."
      ]
    ["AcceptVersion"]   ->
      [ "The remote end decides which version to use and sends chosen version."
      , "The server is allowed to modify version parameters."
      ]
    ["Refuse"]          -> ["It refuses to run any version."]
    _otherwise          -> [] :: [Text]
    where
      wrap it = case it of
        ""  -> Nothing
        it' -> Just it'

  allNamespaces = [
      Namespace [] ["ProposeVersions"]
    , Namespace [] ["ReplyVersions"]
    , Namespace [] ["QueryReply"]
    , Namespace [] ["AcceptVersion"]
    , Namespace [] ["Refuse"]
    ]


--------------------------------------------------------------------------------
-- DiffusionInit Tracer
--------------------------------------------------------------------------------

instance (Show ntnAddr, Show ntcAddr) =>
  LogFormatting (Diff.DiffusionTracer ntnAddr ntcAddr) where
  forMachine _dtal (Diff.RunServer sockAddr) = mconcat
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  forMachine _dtal (Diff.RunLocalServer localAddress) = mconcat
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  forMachine _dtal (Diff.UsingSystemdSocket localAddress) = mconcat
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack . show $ localAddress)
    ]

  forMachine _dtal (Diff.CreateSystemdSocketForSnocketPath localAddress) = mconcat
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack . show $ localAddress)
    ]
  forMachine _dtal (Diff.CreatedLocalSocket localAddress) = mconcat
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    ]
  forMachine _dtal (Diff.ConfiguringLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (Diff.ListeningLocalSocket localAddress socket) = mconcat
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .=  String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (Diff.LocalSocketUp localAddress fd) = mconcat
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    ]
  forMachine _dtal (Diff.CreatingServerSocket socket) = mconcat
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (Diff.ListeningServerSocket socket) = mconcat
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (Diff.ServerSocketUp socket) = mconcat
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (Diff.ConfiguringServerSocket socket) = mconcat
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (Diff.UnsupportedLocalSystemdSocket path) = mconcat
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  forMachine _dtal Diff.UnsupportedReadySocketCase = mconcat
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  forMachine _dtal (Diff.DiffusionErrored exception) = mconcat
    [ "kind" .= String "DiffusionErrored"
    , "error" .= String (pack (show exception))
    ]
  forMachine _dtal (Diff.SystemdSocketConfiguration config) = mconcat
    [ "kind" .= String "SystemdSocketConfiguration"
    , "path" .= String (pack (show config))
    ]

instance MetaTrace (Diff.DiffusionTracer ntnAddr ntcAddr) where
    namespaceFor Diff.RunServer {} =
      Namespace [] ["RunServer"]
    namespaceFor Diff.RunLocalServer {} =
      Namespace [] ["RunLocalServer"]
    namespaceFor Diff.UsingSystemdSocket {} =
      Namespace [] ["UsingSystemdSocket"]
    namespaceFor Diff.CreateSystemdSocketForSnocketPath {} =
      Namespace [] ["CreateSystemdSocketForSnocketPath"]
    namespaceFor Diff.CreatedLocalSocket {} =
      Namespace [] ["CreatedLocalSocket"]
    namespaceFor Diff.ConfiguringLocalSocket {} =
      Namespace [] ["ConfiguringLocalSocket"]
    namespaceFor Diff.ListeningLocalSocket {} =
      Namespace [] ["ListeningLocalSocket"]
    namespaceFor Diff.LocalSocketUp {} =
      Namespace [] ["LocalSocketUp"]
    namespaceFor Diff.CreatingServerSocket {} =
      Namespace [] ["CreatingServerSocket"]
    namespaceFor Diff.ListeningServerSocket {} =
      Namespace [] ["ListeningServerSocket"]
    namespaceFor Diff.ServerSocketUp {} =
      Namespace [] ["ServerSocketUp"]
    namespaceFor Diff.ConfiguringServerSocket {} =
      Namespace [] ["ConfiguringServerSocket"]
    namespaceFor Diff.UnsupportedLocalSystemdSocket {} =
      Namespace [] ["UnsupportedLocalSystemdSocket"]
    namespaceFor Diff.UnsupportedReadySocketCase {} =
      Namespace [] ["UnsupportedReadySocketCase"]
    namespaceFor Diff.DiffusionErrored {} =
      Namespace [] ["DiffusionErrored"]
    namespaceFor Diff.SystemdSocketConfiguration {} =
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
  forMachine _dtal (PickedLedgerPeer addr _ackStake stake) =
    mconcat
      [ "kind" .= String "PickedLedgerPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  forMachine _dtal (PickedLedgerPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedLedgerPeers"
      , "desiredCount" .= n
      , "count" .= List.length addrs
      , "addresses" .= show addrs
      ]
  forMachine _dtal (PickedBigLedgerPeer addr _ackStake stake) =
    mconcat
      [ "kind" .= String "PickedBigLedgerPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  forMachine _dtal (PickedBigLedgerPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "kind" .= String "PickedBigLedgerPeers"
      , "desiredCount" .= n
      , "count" .= List.length addrs
      , "addresses" .= show addrs
      ]
  forMachine _dtal (FetchingNewLedgerState cnt bigCnt) =
    mconcat
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfLedgerPeers" .= cnt
      , "numberOfBigLedgerPeers" .= bigCnt
      ]
  forMachine _dtal DisabledLedgerPeers =
    mconcat
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  forMachine _dtal (TraceUseLedgerPeers ulp) =
    mconcat
      [ "kind" .= String "UseLedgerPeers"
      , "useLedgerPeers" .= ulp
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
  forMachine _dtal FallingBackToPublicRootPeers =
    mconcat
      [ "kind" .= String "FallingBackToPublicRootPeers"
      ]
  forMachine _dtal (NotEnoughLedgerPeers (NumberOfPeers target) numOfLedgerPeers) =
    mconcat
      [ "kind" .= String "NotEnoughLedgerPeers"
      , "target" .= target
      , "numOfLedgerPeers" .= numOfLedgerPeers
      ]
  forMachine _dtal (NotEnoughBigLedgerPeers (NumberOfPeers target) numOfBigLedgerPeers) =
    mconcat
      [ "kind" .= String "NotEnoughBigLedgerPeers"
      , "target" .= target
      , "numOfBigLedgerPeers" .= numOfBigLedgerPeers
      ]
  forMachine _dtal (TraceLedgerPeersDomains daps) =
    mconcat
      [ "kind" .= String "TraceLedgerPeersDomains"
      , "domainAccessPoints" .= daps
      ]
  forMachine _dtal UsingBigLedgerPeerSnapshot =
    mconcat
      [ "kind" .= String "UsingBigLedgerPeerSnapshot"
      ]

instance MetaTrace TraceLedgerPeers where
    namespaceFor PickedLedgerPeer {} =
      Namespace [] ["PickedLedgerPeer"]
    namespaceFor PickedLedgerPeers {} =
      Namespace [] ["PickedLedgerPeers"]
    namespaceFor PickedBigLedgerPeer {} =
      Namespace [] ["PickedBigLedgerPeer"]
    namespaceFor PickedBigLedgerPeers {} =
      Namespace [] ["PickedBigLedgerPeers"]
    namespaceFor FetchingNewLedgerState {} =
      Namespace [] ["FetchingNewLedgerState"]
    namespaceFor DisabledLedgerPeers {} =
      Namespace [] ["DisabledLedgerPeers"]
    namespaceFor TraceUseLedgerPeers {} =
      Namespace [] ["TraceUseLedgerPeers"]
    namespaceFor WaitingOnRequest {} =
      Namespace [] ["WaitingOnRequest"]
    namespaceFor RequestForPeers {} =
      Namespace [] ["RequestForPeers"]
    namespaceFor ReusingLedgerState {} =
      Namespace [] ["ReusingLedgerState"]
    namespaceFor FallingBackToPublicRootPeers {} =
      Namespace [] ["FallingBackToPublicRootPeers"]
    namespaceFor NotEnoughLedgerPeers {} =
      Namespace [] ["NotEnoughLedgerPeers"]
    namespaceFor NotEnoughBigLedgerPeers {} =
      Namespace [] ["NotEnoughBigLedgerPeers"]
    namespaceFor TraceLedgerPeersDomains {} =
      Namespace [] ["TraceLedgerPeersDomains"]
    namespaceFor UsingBigLedgerPeerSnapshot {} =
      Namespace [] ["UsingBigLedgerPeerSnapshot"]

    severityFor (Namespace _ ["PickedPeer"]) _ = Just Debug
    severityFor (Namespace _ ["PickedPeers"]) _ = Just Info
    severityFor (Namespace _ ["FetchingNewLedgerState"]) _ = Just Info
    severityFor (Namespace _ ["DisabledLedgerPeers"]) _ = Just Info
    severityFor (Namespace _ ["TraceUseLedgerAfter"]) _ = Just Info
    severityFor (Namespace _ ["WaitingOnRequest"]) _ = Just Debug
    severityFor (Namespace _ ["RequestForPeers"]) _ = Just Debug
    severityFor (Namespace _ ["ReusingLedgerState"]) _ = Just Debug
    severityFor (Namespace _ ["FallingBackToPublicRootPeers"]) _ = Just Info
    severityFor (Namespace _ ["NotEnoughLedgerPeers"]) _ = Just Warning
    severityFor (Namespace _ ["NotEnoughBigLedgerPeers"]) _ = Just Warning
    severityFor (Namespace _ ["TraceLedgerPeersDomains"]) _ = Just Debug
    severityFor (Namespace _ ["TraceLedgerPeersResult"]) _ = Just Debug
    severityFor (Namespace _ ["TraceLedgerPeersFailure"]) _ = Just Debug
    severityFor (Namespace _ ["UsingBigLedgerPeerSnapshot"]) _ = Just Debug
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
    documentFor (Namespace _ ["FallingBackToPublicRootPeers"]) = Just
      ""
    documentFor (Namespace _ ["TraceLedgerPeersDomains"]) = Just
      ""
    documentFor (Namespace _ ["TraceLedgerPeersResult"]) = Just
      ""
    documentFor (Namespace _ ["TraceLedgerPeersFailure"]) = Just
      ""
    documentFor (Namespace _ ["UsingBigLedgerPeerSnapshot"]) = Just $ mconcat
      [ "Trace for when a request for big ledger peers is fulfilled from the snapshot file"
      , " defined in the topology configuration file."]
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
      , Namespace [] ["FallingBackToPublicRootPeers"]
      , Namespace [] ["TraceLedgerPeersDomains"]
      , Namespace [] ["TraceLedgerPeersResult"]
      , Namespace [] ["TraceLedgerPeersFailure"]
      , Namespace [] ["UsingBigLedgerPeerSnapshot"]
      ]
