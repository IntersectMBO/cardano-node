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
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Formatting
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.Mux.Types
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))

import qualified Data.List as List
import qualified Ouroboros.Network.Diffusion as ND
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..), PoolStake (..),
                   TraceLedgerPeers (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import Cardano.Node.Configuration.TopologyP2P ()


--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, LogFormatting MuxTrace) =>
    LogFormatting (WithMuxBearer peer MuxTrace) where
    forMachine dtal (WithMuxBearer b ev) =
      mconcat [ "kind"   .= String "MuxTrace"
              , "bearer" .= forMachine dtal b
              , "event"  .= forMachine dtal ev ]
    forHuman (WithMuxBearer b ev) = "With mux bearer " <> forHumanOrMachine b
                                      <> ". " <> forHumanOrMachine ev

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

instance LogFormatting MuxTrace where
    forMachine _dtal MuxTraceRecvHeaderStart = mconcat
      [ "kind" .= String "MuxTraceRecvHeaderStart"
      , "msg"  .= String "Bearer Receive Header Start"
      ]
    forMachine _dtal (MuxTraceRecvHeaderEnd MuxSDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "kind" .= String "MuxTraceRecvHeaderStart"
      , "msg"  .=  String "Bearer Receive Header End"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal (MuxTraceRecvDeltaQObservation MuxSDUHeader { mhTimestamp, mhLength } ts) = mconcat
      [ "kind" .= String "MuxTraceRecvDeltaQObservation"
      , "msg"  .=  String "Bearer DeltaQ observation"
      , "timeRemote" .=  String (showT ts)
      , "timeLocal" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal (MuxTraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = mconcat
      [ "kind" .= String "MuxTraceRecvDeltaQSample"
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
    forMachine _dtal (MuxTraceRecvStart len) = mconcat
      [ "kind" .= String "MuxTraceRecvStart"
      , "msg"  .= String "Bearer Receive Start"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (MuxTraceRecvEnd len) = mconcat
      [ "kind" .= String "MuxTraceRecvEnd"
      , "msg"  .= String "Bearer Receive End"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (MuxTraceSendStart MuxSDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "kind" .= String "MuxTraceSendStart"
      , "msg"  .= String "Bearer Send Start"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal MuxTraceSendEnd = mconcat
      [ "kind" .= String "MuxTraceSendEnd"
      , "msg"  .= String "Bearer Send End"
      ]
    forMachine _dtal (MuxTraceState new) = mconcat
      [ "kind" .= String "MuxTraceState"
      , "msg"  .= String "MuxState"
      , "state" .= String (showT new)
      ]
    forMachine _dtal (MuxTraceCleanExit mid dir) = mconcat
      [ "kind" .= String "MuxTraceCleanExit"
      , "msg"  .= String "Miniprotocol terminated cleanly"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (MuxTraceExceptionExit mid dir exc) = mconcat
      [ "kind" .= String "MuxTraceExceptionExit"
      , "msg"  .= String "Miniprotocol terminated with exception"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      , "exception" .= String (showT exc)
      ]
    forMachine _dtal (MuxTraceChannelRecvStart mid) = mconcat
      [ "kind" .= String "MuxTraceChannelRecvStart"
      , "msg"  .= String "Channel Receive Start"
      , "miniProtocolNum" .= String (showT mid)
      ]
    forMachine _dtal (MuxTraceChannelRecvEnd mid len) = mconcat
      [ "kind" .= String "MuxTraceChannelRecvEnd"
      , "msg"  .= String "Channel Receive End"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]
    forMachine _dtal (MuxTraceChannelSendStart mid len) = mconcat
      [ "kind" .= String "MuxTraceChannelSendStart"
      , "msg"  .= String "Channel Send Start"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]
    forMachine _dtal (MuxTraceChannelSendEnd mid) = mconcat
      [ "kind" .= String "MuxTraceChannelSendEnd"
      , "msg"  .= String "Channel Send End"
      , "miniProtocolNum" .= String (showT mid)
      ]
    forMachine _dtal MuxTraceHandshakeStart = mconcat
      [ "kind" .= String "MuxTraceHandshakeStart"
      , "msg"  .= String "Handshake start"
      ]
    forMachine _dtal (MuxTraceHandshakeClientEnd duration) = mconcat
      [ "kind" .= String "MuxTraceHandshakeClientEnd"
      , "msg"  .= String "Handshake Client end"
      , "duration" .= String (showT duration)
      ]
    forMachine _dtal MuxTraceHandshakeServerEnd = mconcat
      [ "kind" .= String "MuxTraceHandshakeServerEnd"
      , "msg"  .= String "Handshake Server end"
      ]
    forMachine dtal (MuxTraceHandshakeClientError e duration) = mconcat
      [ "kind" .= String "MuxTraceHandshakeClientError"
      , "msg"  .= String "Handshake Client Error"
      , "duration" .= String (showT duration)
      -- Client Error can include an error string from the peer which could be very large.
      , "error" .= if dtal >= DDetailed
                      then show e
                      else take 256 $ show e
      ]
    forMachine dtal (MuxTraceHandshakeServerError e) = mconcat
      [ "kind" .= String "MuxTraceHandshakeServerError"
      , "msg"  .= String "Handshake Server Error"
      , "error" .= if dtal >= DDetailed
                      then show e
                      else take 256 $ show e
      ]
    forMachine _dtal MuxTraceSDUReadTimeoutException = mconcat
      [ "kind" .= String "MuxTraceSDUReadTimeoutException"
      , "msg"  .= String "Timed out reading SDU"
      ]
    forMachine _dtal MuxTraceSDUWriteTimeoutException = mconcat
      [ "kind" .= String "MuxTraceSDUWriteTimeoutException"
      , "msg"  .= String "Timed out writing SDU"
      ]
    forMachine _dtal (MuxTraceStartEagerly mid dir) = mconcat
      [ "kind" .= String "MuxTraceStartEagerly"
      , "msg"  .= String "Eagerly started"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (MuxTraceStartOnDemand mid dir) = mconcat
      [ "kind" .= String "MuxTraceStartOnDemand"
      , "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (MuxTraceStartedOnDemand mid dir) = mconcat
      [ "kind" .= String "MuxTraceStartedOnDemand"
      , "msg"  .= String "Started on demand"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (MuxTraceTerminating mid dir) = mconcat
      [ "kind" .= String "MuxTraceTerminating"
      , "msg"  .= String "Terminating"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal MuxTraceStopping = mconcat
      [ "kind" .= String "MuxTraceStopping"
      , "msg"  .= String "Mux stopping"
      ]
    forMachine _dtal MuxTraceStopped = mconcat
      [ "kind" .= String "MuxTraceStopped"
      , "msg"  .= String "Mux stoppped"
      ]
#ifdef os_HOST_linux
    forMachine _dtal (MuxTraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) =
      [ "kind" .= String "MuxTraceTCPInfo"
      , "msg"  .= String "TCPInfo"
      , "rtt"  .= String (show (fromIntegral tcpi_rtt :: Word))
      , "rttvar" .= String (show (fromIntegral tcpi_rttvar :: Word))
      , "snd_cwnd" .= String (show (fromIntegral tcpi_snd_cwnd :: Word))
      , "snd_mss" .= String (show (fromIntegral tcpi_snd_mss :: Word))
      , "rcv_mss" .= String (show (fromIntegral tcpi_rcv_mss :: Word))
      , "lost" .= String (show (fromIntegral tcpi_lost :: Word))
      , "retrans" .= String (show (fromIntegral tcpi_retrans :: Word))
      , "length" .= String (showT len)
      ]
#else
    forMachine _dtal (MuxTraceTCPInfo _ len) = mconcat
      [ "kind" .= String "MuxTraceTCPInfo"
      , "msg"  .= String "TCPInfo"
      , "len"  .= String (showT len)
      ]
#endif

    forHuman MuxTraceRecvHeaderStart =
      "Bearer Receive Header Start"
    forHuman (MuxTraceRecvHeaderEnd MuxSDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) =
      sformat ("Bearer Receive Header End: ts:" % prefixHex % "(" % shown % ") " % shown % " len " % int)
        (unRemoteClockModel mhTimestamp) mhNum mhDir mhLength
    forHuman (MuxTraceRecvDeltaQObservation MuxSDUHeader { mhTimestamp, mhLength } ts) =
      sformat ("Bearer DeltaQ observation: remote ts" % int % " local ts " % shown % " length " % int)
         (unRemoteClockModel mhTimestamp) ts mhLength
    forHuman (MuxTraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) =
      sformat ("Bearer DeltaQ Sample: duration " % fixed 3 % " packets " % int % " sumBytes "
        % int % " DeltaQ_S " % fixed 3 % " DeltaQ_VMean " % fixed 3 % "DeltaQ_VVar " % fixed 3
        % " DeltaQ_estR " % fixed 3 % " sizeDist " % string)
        d sp so dqs dqvm dqvs estR sdud
    forHuman (MuxTraceRecvStart len) =
      sformat ("Bearer Receive Start: length " % int) len
    forHuman (MuxTraceRecvEnd len) =
      sformat ("Bearer Receive End: length " % int) len
    forHuman (MuxTraceSendStart MuxSDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) =
      sformat ("Bearer Send Start: ts: " % prefixHex % " (" % shown % ") " % shown % " length " % int)
        (unRemoteClockModel mhTimestamp) mhNum mhDir mhLength
    forHuman MuxTraceSendEnd =
      "Bearer Send End"
    forHuman (MuxTraceState new) =
      sformat ("State: " % shown) new
    forHuman (MuxTraceCleanExit mid dir) =
      sformat ("Miniprotocol (" % shown % ") " % shown % " terminated cleanly")
      mid dir
    forHuman (MuxTraceExceptionExit mid dir e) =
      sformat ("Miniprotocol (" % shown % ") " % shown %
        " terminated with exception " % shown) mid dir e
    forHuman (MuxTraceChannelRecvStart mid) =
      sformat ("Channel Receive Start on " % shown) mid
    forHuman (MuxTraceChannelRecvEnd mid len) =
      sformat ("Channel Receive End on (" % shown % ") " % int) mid len
    forHuman (MuxTraceChannelSendStart mid len) =
      sformat ("Channel Send Start on (" % shown % ") " % int) mid len
    forHuman (MuxTraceChannelSendEnd mid) =
      sformat ("Channel Send End on " % shown) mid
    forHuman MuxTraceHandshakeStart =
      "Handshake start"
    forHuman (MuxTraceHandshakeClientEnd duration) =
      sformat ("Handshake Client end, duration " % shown) duration
    forHuman MuxTraceHandshakeServerEnd =
      "Handshake Server end"
    forHuman (MuxTraceHandshakeClientError e duration) =
         -- Client Error can include an error string from the peer which could be very large.
        sformat ("Handshake Client Error " % string % " duration " % shown)
          (take 256 $ show e) duration
    forHuman (MuxTraceHandshakeServerError e) =
      sformat ("Handshake Server Error " % shown) e
    forHuman MuxTraceSDUReadTimeoutException =
      "Timed out reading SDU"
    forHuman MuxTraceSDUWriteTimeoutException =
      "Timed out writing SDU"
    forHuman (MuxTraceStartEagerly mid dir) =
      sformat ("Eagerly started (" % shown % ") in " % shown) mid dir
    forHuman (MuxTraceStartOnDemand mid dir) =
      sformat ("Preparing to start (" % shown % ") in " % shown) mid dir
    forHuman (MuxTraceStartedOnDemand mid dir) =
      sformat ("Started on demand (" % shown % ") in " % shown) mid dir
    forHuman (MuxTraceTerminating mid dir) =
      sformat ("Terminating (" % shown % ") in " % shown) mid dir
    forHuman MuxTraceStopping = "Mux stopping"
    forHuman MuxTraceStopped  = "Mux stoppped"
#ifdef os_HOST_linux
    forHuman (MuxTraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) =
      sformat ("TCPInfo rtt % int % " rttvar " % ínt % " cwnd " % int %
               " smss " % int % " rmss " % int % " lost " % int %
               " retrans " % int % " len " %int)
              (fromIntegral tcpi_rtt :: Word) (fromIntegral tcpi_rttvar :: Word)
              (fromIntegral tcpi_snd_cwnd :: Word) (fromIntegral tcpi_snd_mss :: Word)
              (fromIntegral tcpi_rcv_mss :: Word) (fromIntegral tcpi_lost :: Word)
              (fromIntegral tcpi_retrans :: Word)
              len
#else
    forHuman (MuxTraceTCPInfo _ len) = sformat ("TCPInfo len " % int) len
#endif

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
    severityFor (Namespace _ ["Stopping"]) _               = Just Debug
    severityFor (Namespace _ ["Stopped"]) _               = Just Debug
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
    documentFor (Namespace _ ["Stopping"])              = Just
      "Mux shutdown."
    documentFor (Namespace _ ["Stopped"])              = Just
      "Mux shutdown."
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
      , Namespace [] ["Stopping"]
      , Namespace [] ["Stopped"]
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
    namespaceFor (AnyMessageAndAgency _stok HS.MsgQueryReply {})   =
      Namespace [] ["MsgQueryReply"]
    namespaceFor (AnyMessageAndAgency _stok HS.MsgAcceptVersion {})   =
      Namespace [] ["AcceptVersion"]
    namespaceFor (AnyMessageAndAgency _stok HS.MsgRefuse {})          =
      Namespace [] ["Refuse"]

    severityFor (Namespace _ ["ProposeVersions"]) _ = Just Info
    severityFor (Namespace _ ["ReplyVersions"]) _ = Just Info
    severityFor (Namespace _ ["MsgQueryReply"]) _ = Just Info
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
    documentFor (Namespace _ ["MsgQueryReply"]) = Just $ mconcat
      [ "`MsgQueryReply` received as a response to a handshake query in "
      , " 'MsgProposeVersions' and lists the supported versions."
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
      , Namespace [] ["MsgQueryReply"]
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
  forMachine _dtal (TraceLedgerPeersResult dap ips) =
    mconcat
      [ "kind" .= String "TraceLedgerPeersResult"
      , "domainAccessPoint" .= show dap
      , "ips" .= map show ips
      ]
  forMachine _dtal (TraceLedgerPeersFailure dap reason) =
    mconcat
      [ "kind" .= String "TraceLedgerPeersFailure"
      , "domainAccessPoint" .= show dap
      , "error" .= show reason
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
    namespaceFor TraceLedgerPeersResult {} =
      Namespace [] ["TraceLedgerPeersResult"]
    namespaceFor TraceLedgerPeersFailure {} =
      Namespace [] ["TraceLedgerPeersFailure"]

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
      ]
