{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.NonP2P
    () where

import           Cardano.Logging
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..), WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (SubscriptionTrace, WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (ConnectResult (..), SubscriberError,
                   SubscriptionTrace (..))

import           Control.Exception (Exception (..), SomeException (..))
import           Data.Aeson (Value (String), (.=))
import qualified Data.IP as IP
import           Data.Text (pack)
import qualified Network.Socket as Socket


--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

instance LogFormatting LocalAddress where
    forMachine _dtal (LocalAddress path) =
        mconcat ["path" .= path]

instance LogFormatting NtN.RemoteAddress where
    forMachine _dtal (Socket.SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        mconcat [ "addr" .= show ip
                 , "port" .= show port
                 ]
    forMachine _dtal (Socket.SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        mconcat [ "addr" .= show ip
                 , "port" .= show port
                 ]
    forMachine _dtal (Socket.SockAddrUnix path) =
        mconcat [ "path" .= show path ]

--------------------------------------------------------------------------------
-- Subscription Tracer
--------------------------------------------------------------------------------

instance LogFormatting (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  forMachine _dtal (WithIPList localAddresses dests ev) =
    mconcat [ "kind" .= String "IP SubscriptionTrace"
             , "localAddresses" .= String (pack $ show localAddresses)
             , "dests" .= String (pack $ show dests)
             , "event" .= String (pack $ show ev)]
  forHuman (WithIPList localAddresses dests ev) =
                     pack (show ev)
                  <> ". Local addresses are "
                  <> pack (show localAddresses)
                  <> ". Destinations are "
                  <> pack (show dests)
                  <> "."

instance LogFormatting (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  forMachine _dtal (WithDomainName dom ev) =
    mconcat [ "kind" .= String "DNS SubscriptionTrace"
             , "domain" .= String (pack $ show dom)
             , "event" .= String (pack $ show ev)]
  forHuman (WithDomainName dom ev) =
                     pack (show ev)
                  <> ". Domain is "
                  <> pack (show dom)
                  <> "."

instance MetaTrace tr => MetaTrace (WithIPList tr) where
    namespaceFor (WithIPList _ _ ev) = nsCast (namespaceFor ev)
    severityFor ns Nothing = severityFor (nsCast ns :: Namespace tr) Nothing
    severityFor ns (Just (WithIPList _ _ ev)) =
      severityFor (nsCast ns) (Just ev)
    detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace tr) Nothing
    detailsFor ns (Just (WithIPList _ _ ev)) =
      detailsFor (nsCast ns) (Just ev)
    privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace tr) Nothing
    privacyFor ns (Just (WithIPList _ _ ev)) =
      privacyFor (nsCast ns) (Just ev)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    allNamespaces  = fmap nsCast
          (allNamespaces :: [Namespace tr])

instance MetaTrace tr => MetaTrace (WithDomainName tr) where
    namespaceFor (WithDomainName _ ev) = nsCast (namespaceFor ev)
    severityFor ns Nothing = severityFor (nsCast ns :: Namespace tr) Nothing
    severityFor ns (Just (WithDomainName _ ev)) =
      severityFor (nsCast ns) (Just ev)
    detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace tr) Nothing
    detailsFor ns (Just (WithDomainName _ ev)) =
      detailsFor (nsCast ns) (Just ev)
    privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace tr) Nothing
    privacyFor ns (Just (WithDomainName _ ev)) =
      privacyFor (nsCast ns) (Just ev)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    allNamespaces  = fmap nsCast
          (allNamespaces :: [Namespace tr])

instance MetaTrace (SubscriptionTrace adr) where
    namespaceFor SubscriptionTraceConnectStart {} =
          Namespace []  ["ConnectStart"]
    namespaceFor SubscriptionTraceConnectEnd {} =
          Namespace []  ["ConnectEnd"]
    namespaceFor SubscriptionTraceConnectException {} =
          Namespace []  ["ConnectException"]
    namespaceFor SubscriptionTraceSocketAllocationException {} =
          Namespace []  ["SocketAllocationException"]
    namespaceFor SubscriptionTraceTryConnectToPeer {}  =
          Namespace []  ["TryConnectToPeer"]
    namespaceFor SubscriptionTraceSkippingPeer {} =
          Namespace []  ["SkippingPeer"]
    namespaceFor SubscriptionTraceSubscriptionRunning =
          Namespace []  ["SubscriptionRunning"]
    namespaceFor SubscriptionTraceSubscriptionWaiting {} =
          Namespace []  ["SubscriptionWaiting"]
    namespaceFor SubscriptionTraceSubscriptionFailed =
          Namespace []  ["SubscriptionFailed"]
    namespaceFor SubscriptionTraceSubscriptionWaitingNewConnection {} =
          Namespace []  ["SubscriptionWaitingNewConnection"]
    namespaceFor SubscriptionTraceStart {} =
          Namespace []  ["Start"]
    namespaceFor SubscriptionTraceRestart {} =
          Namespace []  ["Restart"]
    namespaceFor SubscriptionTraceConnectionExist {} =
          Namespace []  ["ConnectionExist"]
    namespaceFor SubscriptionTraceUnsupportedRemoteAddr {} =
          Namespace []  ["UnsupportedRemoteAddr"]
    namespaceFor SubscriptionTraceMissingLocalAddress =
          Namespace []  ["MissingLocalAddress"]
    namespaceFor SubscriptionTraceApplicationException {} =
          Namespace []  ["ApplicationException"]
    namespaceFor SubscriptionTraceAllocateSocket {} =
          Namespace []  ["AllocateSocket"]
    namespaceFor SubscriptionTraceCloseSocket {} =
          Namespace []  ["CloseSocket"]

    severityFor (Namespace _  ["ConnectStart"]) _ = Just Info
    severityFor (Namespace _  ["ConnectEnd"])
      (Just (SubscriptionTraceConnectEnd _ connectResult)) =
        case connectResult of
          ConnectSuccess         -> Just Info
          ConnectSuccessLast     -> Just Notice
          ConnectValencyExceeded -> Just Warning
    severityFor (Namespace _  ["ConnectEnd"]) Nothing = Just Info
    severityFor (Namespace _  ["ConnectException"])
      (Just (SubscriptionTraceConnectException _ e)) =
        case fromException $ SomeException e of
              Just (_::SubscriberError) -> Just Debug
              _                         -> Just Info
    severityFor (Namespace _  ["ConnectException"]) Nothing = Just Info
    severityFor (Namespace _  ["SocketAllocationException"]) _ = Just Error
    severityFor (Namespace _  ["TryConnectToPeer"]) _ = Just Info
    severityFor (Namespace _  ["SkippingPeer"]) _ = Just Info
    severityFor (Namespace _  ["SubscriptionRunning"]) _ = Just Debug
    severityFor (Namespace _  ["SubscriptionWaiting"]) _ = Just Debug
    severityFor (Namespace _  ["SubscriptionFailed"]) _ = Just Error
    severityFor (Namespace _  ["SubscriptionWaitingNewConnection"]) _ = Just Notice
    severityFor (Namespace _  ["Start"]) _ = Just Debug
    severityFor (Namespace _  ["Restart"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionExist"]) _ = Just Notice
    severityFor (Namespace _  ["UnsupportedRemoteAddr"]) _ = Just Error
    severityFor (Namespace _  ["MissingLocalAddress"]) _ = Just Warning
    severityFor (Namespace _  ["ApplicationException"])
      (Just (SubscriptionTraceApplicationException _ e)) =
        case fromException $ SomeException e of
            Just (_::SubscriberError) -> Just Debug
            _                         -> Just Error
    severityFor (Namespace _  ["ApplicationException"]) Nothing = Just Error
    severityFor (Namespace _  ["AllocateSocket"]) _ = Just Debug
    severityFor (Namespace _  ["CloseSocket"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _  ["ConnectStart"]) = Just
      "Connection Attempt Start with destination."
    documentFor (Namespace _  ["ConnectEnd"]) = Just
      "Connection Attempt end with destination and outcome."
    documentFor (Namespace _  ["ConnectException"])  = Just
      "Socket Allocation Exception with destination and the exception."
    documentFor (Namespace _  ["SocketAllocationException"]) = Just
      "Socket Allocation Exception with destination and the exception."
    documentFor (Namespace _  ["TryConnectToPeer"]) = Just
      "Trying to connect to peer with address."
    documentFor (Namespace _  ["SkippingPeer"]) = Just
      "Skipping peer with address."
    documentFor (Namespace _  ["SubscriptionRunning"]) = Just
      "Required subscriptions started."
    documentFor (Namespace _  ["SubscriptionWaiting"]) = Just
      "Waiting on address with active connections."
    documentFor (Namespace _  ["SubscriptionFailed"]) = Just
      "Failed to start all required subscriptions."
    documentFor (Namespace _  ["SubscriptionWaitingNewConnection"]) = Just
      "Waiting delay time before attempting a new connection."
    documentFor (Namespace _  ["Start"]) = Just
      "Starting Subscription Worker with a valency."
    documentFor (Namespace _  ["Restart"]) = Just $ mconcat
      [ "Restarting Subscription after duration with desired valency and"
      , " current valency."
      ]
    documentFor (Namespace _  ["ConnectionExist"]) = Just
      "Connection exists to destination."
    documentFor (Namespace _  ["UnsupportedRemoteAddr"]) = Just
      "Unsupported remote target address."
    documentFor (Namespace _  ["MissingLocalAddress"]) = Just
      "Missing local address."
    documentFor (Namespace _  ["ApplicationException"]) = Just
      "Application Exception occurred."
    documentFor (Namespace _  ["AllocateSocket"]) = Just
      "Allocate socket to address."
    documentFor (Namespace _  ["CloseSocket"]) = Just
      "Closed socket to address."
    documentFor _ = Nothing

    allNamespaces = [
            Namespace []  ["ConnectStart"]
          , Namespace []  ["ConnectEnd"]
          , Namespace []  ["ConnectException"]
          , Namespace []  ["SocketAllocationException"]
          , Namespace []  ["TryConnectToPeer"]
          , Namespace []  ["SkippingPeer"]
          , Namespace []  ["SubscriptionRunning"]
          , Namespace []  ["SubscriptionWaiting"]
          , Namespace []  ["SubscriptionFailed"]
          , Namespace []  ["SubscriptionWaitingNewConnection"]
          , Namespace []  ["Start"]
          , Namespace []  ["Restart"]
          , Namespace []  ["ConnectionExist"]
          , Namespace []  ["UnsupportedRemoteAddr"]
          , Namespace []  ["MissingLocalAddress"]
          , Namespace []  ["ApplicationException"]
          , Namespace []  ["AllocateSocket"]
          , Namespace []  ["CloseSocket"]
          ]




--------------------------------------------------------------------------------
-- DNSResolver Tracer
--------------------------------------------------------------------------------

instance LogFormatting (WithDomainName DnsTrace) where
    forMachine _dtal (WithDomainName dom ev) =
      mconcat [ "kind" .= String "DnsTrace"
              , "domain" .= String (pack $ show dom)
              , "event" .= String (pack $ show ev)]
    forHuman (WithDomainName dom ev) =
                      pack (show ev)
                    <> ". Domain is "
                    <> pack (show dom)
                    <> "."

instance MetaTrace DnsTrace where
    namespaceFor DnsTraceLookupException {} =
      Namespace [] ["LookupException"]
    namespaceFor DnsTraceLookupAError {} =
      Namespace [] ["LookupAError"]
    namespaceFor DnsTraceLookupAAAAError {} =
      Namespace [] ["LookupAAAAError"]
    namespaceFor DnsTraceLookupIPv6First =
      Namespace [] ["LookupIPv6First"]
    namespaceFor DnsTraceLookupIPv4First =
      Namespace [] ["LookupIPv4First"]
    namespaceFor DnsTraceLookupAResult {} =
      Namespace [] ["LookupAResult"]
    namespaceFor DnsTraceLookupAAAAResult {} =
      Namespace [] ["LookupAAAAResult"]

    severityFor (Namespace _ ["LookupException"]) _ = Just Error
    severityFor (Namespace _ ["LookupAError"]) _    = Just Error
    severityFor (Namespace _ ["LookupAAAAError"]) _ = Just Error
    severityFor (Namespace _ ["LookupIPv6First"]) _ = Just Debug
    severityFor (Namespace _ ["LookupIPv4First"]) _ = Just Debug
    severityFor (Namespace _ ["LookupAResult"]) _   = Just Debug
    severityFor (Namespace _ ["LookupAAAAResult"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["LookupException"]) = Just
      "A DNS lookup exception occurred."
    documentFor (Namespace _ ["LookupAError"])    = Just
      "A lookup failed with an error."
    documentFor (Namespace _ ["LookupAAAAError"]) = Just
      "AAAA lookup failed with an error."
    documentFor (Namespace _ ["LookupIPv6First"]) = Just
      "Returning IPv6 address first."
    documentFor (Namespace _ ["LookupIPv4First"]) = Just
      "Returning IPv4 address first."
    documentFor (Namespace _ ["LookupAResult"])   = Just
      "Lookup A result."
    documentFor (Namespace _ ["LookupAAAAResult"]) = Just
      "Lookup AAAA result."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["LookupException"]
      , Namespace [] ["LookupAError"]
      , Namespace [] ["LookupAAAAError"]
      , Namespace [] ["LookupIPv6First"]
      , Namespace [] ["LookupIPv4First"]
      , Namespace [] ["LookupAResult"]
      , Namespace [] ["LookupAAAAResult"]
      ]


--------------------------------------------------------------------------------
-- ErrorPolicy Tracer
--------------------------------------------------------------------------------

instance Show addr => LogFormatting (NtN.WithAddr addr NtN.ErrorPolicyTrace) where
    forMachine _dtal (NtN.WithAddr addr ev) =
      mconcat [ "kind" .= String "ErrorPolicyTrace"
               , "address" .= show addr
               , "event" .= show ev ]
    forHuman (NtN.WithAddr addr ev) = "With address " <> showT addr <> ". " <> showT ev

instance MetaTrace tr => MetaTrace (NtN.WithAddr addr tr) where
    namespaceFor (NtN.WithAddr _ ev) = nsCast (namespaceFor ev)
    severityFor ns Nothing = severityFor (nsCast ns :: Namespace tr) Nothing
    severityFor ns (Just (NtN.WithAddr _ ev)) =
      severityFor (nsCast ns) (Just ev)
    detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace tr) Nothing
    detailsFor ns (Just (NtN.WithAddr _ ev)) =
      detailsFor (nsCast ns) (Just ev)
    privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace tr) Nothing
    privacyFor ns (Just (NtN.WithAddr _ ev)) =
      privacyFor (nsCast ns) (Just ev)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    allNamespaces  = fmap nsCast
          (allNamespaces :: [Namespace tr])

instance MetaTrace NtN.ErrorPolicyTrace where
    namespaceFor ErrorPolicySuspendPeer {} =
      Namespace [] ["SuspendPeer"]
    namespaceFor ErrorPolicySuspendConsumer {} =
      Namespace [] ["SuspendConsumer"]
    namespaceFor ErrorPolicyLocalNodeError {} =
      Namespace [] ["LocalNodeError"]
    namespaceFor ErrorPolicyResumePeer {} =
      Namespace [] ["ResumePeer"]
    namespaceFor ErrorPolicyKeepSuspended {} =
      Namespace [] ["KeepSuspended"]
    namespaceFor ErrorPolicyResumeConsumer {} =
      Namespace [] ["ResumeConsumer"]
    namespaceFor ErrorPolicyResumeProducer {} =
      Namespace [] ["ResumeProducer"]
    namespaceFor ErrorPolicyUnhandledApplicationException {} =
      Namespace [] ["UnhandledApplicationException"]
    namespaceFor ErrorPolicyUnhandledConnectionException {} =
      Namespace [] ["UnhandledConnectionException"]
    namespaceFor ErrorPolicyAcceptException {} =
      Namespace [] ["AcceptException"]

    severityFor (Namespace _ ["SuspendPeer"]) _ = Just Warning
    severityFor (Namespace _ ["SuspendConsumer"]) _ = Just Notice
    severityFor (Namespace _ ["LocalNodeError"]) _ = Just Error
    severityFor (Namespace _ ["ResumePeer"]) _ = Just Debug
    severityFor (Namespace _ ["KeepSuspended"]) _ = Just Debug
    severityFor (Namespace _ ["ResumeConsumer"]) _ = Just Debug
    severityFor (Namespace _ ["ResumeProducer"]) _ = Just Debug
    severityFor (Namespace _ ["UnhandledApplicationException"]) _ = Just Error
    severityFor (Namespace _ ["UnhandledConnectionException"]) _ = Just Error
    severityFor (Namespace _ ["AcceptException"]) _ = Just Error
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["SuspendPeer"]) = Just
      "Suspending peer with a given exception."
    documentFor (Namespace _ ["SuspendConsumer"]) = Just
      "Suspending consumer."
    documentFor (Namespace _ ["LocalNodeError"]) = Just
      "Caught a local exception."
    documentFor (Namespace _ ["ResumePeer"]) = Just
      "Resume a peer (both consumer and producer)."
    documentFor (Namespace _ ["KeepSuspended"]) = Just
      "Consumer was suspended until producer will resume."
    documentFor (Namespace _ ["ResumeConsumer"]) = Just
      "Resume consumer."
    documentFor (Namespace _ ["ResumeProducer"]) = Just
      "Resume producer."
    documentFor (Namespace _ ["UnhandledApplicationException"]) = Just
      "An application threw an exception, which was not handled."
    documentFor (Namespace _ ["UnhandledConnectionException"]) = Just
      ""
    documentFor (Namespace _ ["AcceptException"]) = Just
      "'accept' threw an exception."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["SuspendPeer"]
      , Namespace [] ["SuspendConsumer"]
      , Namespace [] ["LocalNodeError"]
      , Namespace [] ["ResumePeer"]
      , Namespace [] ["KeepSuspended"]
      , Namespace [] ["ResumeConsumer"]
      , Namespace [] ["ResumeProducer"]
      , Namespace [] ["UnhandledApplicationException"]
      , Namespace [] ["UnhandledConnectionException"]
      , Namespace [] ["AcceptException"]
      ]
