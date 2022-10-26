{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.NonP2P
  (
    severityIPSubscription
  , namesForIPSubscription
  , docIPSubscription

  , severityDNSSubscription
  , namesForDNSSubscription
  , docDNSSubscription

  , severityDNSResolver
  , namesForDNSResolver
  , docDNSResolver

  , severityErrorPolicy
  , namesForErrorPolicy
  , docErrorPolicy

  , severityLocalErrorPolicy
  , namesForLocalErrorPolicy
  , docLocalErrorPolicy

  , severityAcceptPolicy
  , namesForAcceptPolicy
  , docAcceptPolicy

) where

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Data.Aeson (Value (String), (.=))
import qualified Data.IP as IP
import           Data.Text (pack)
import qualified Network.Socket as Socket
import           Text.Show

import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..), WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (SubscriptionTrace, WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (ConnectResult (..), SubscriberError,
                   SubscriptionTrace (..))


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


instance LogFormatting NtN.RemoteConnectionId where
    forMachine dtal (NtN.ConnectionId l r) =
        mconcat [ "local" .= forMachine dtal l
                 , "remote" .= forMachine dtal r
                 ]

instance LogFormatting LocalAddress where
    forMachine _dtal (LocalAddress path) =
        mconcat ["path" .= path]

instance LogFormatting NtC.LocalConnectionId where
    forMachine dtal (NtC.ConnectionId l r) =
        mconcat [ "local" .= forMachine dtal l
                 , "remote" .= forMachine dtal r
                 ]

--------------------------------------------------------------------------------
-- IPSubscription Tracer
--------------------------------------------------------------------------------

severityIPSubscription ::
     WithIPList (SubscriptionTrace Socket.SockAddr)
  -> SeverityS
severityIPSubscription WithIPList {..} = case wilEvent of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess         -> Info
      ConnectSuccessLast     -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             _                         -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             _                         -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

namesForSubscription ::
     SubscriptionTrace Socket.SockAddr
  -> [Text]
namesForSubscription SubscriptionTraceConnectStart {} = ["ConnectStart"]
namesForSubscription SubscriptionTraceConnectEnd {} = ["ConnectEnd"]
namesForSubscription SubscriptionTraceConnectException {} = ["ConnectException"]
namesForSubscription SubscriptionTraceSocketAllocationException {} = ["SocketAllocationException"]
namesForSubscription SubscriptionTraceTryConnectToPeer {}  = ["TryConnectToPeer"]
namesForSubscription SubscriptionTraceSkippingPeer {} = ["SkippingPeer"]
namesForSubscription SubscriptionTraceSubscriptionRunning = ["SubscriptionRunning"]
namesForSubscription SubscriptionTraceSubscriptionWaiting {} = ["SubscriptionWaiting"]
namesForSubscription SubscriptionTraceSubscriptionFailed = ["SubscriptionFailed"]
namesForSubscription SubscriptionTraceSubscriptionWaitingNewConnection {} = ["SubscriptionWaitingNewConnection"]
namesForSubscription SubscriptionTraceStart {} = ["Start"]
namesForSubscription SubscriptionTraceRestart {} = ["Restart"]
namesForSubscription SubscriptionTraceConnectionExist {} = ["ConnectionExist"]
namesForSubscription SubscriptionTraceUnsupportedRemoteAddr {} = ["UnsupportedRemoteAddr"]
namesForSubscription SubscriptionTraceMissingLocalAddress = ["MissingLocalAddress"]
namesForSubscription SubscriptionTraceApplicationException {} = ["ApplicationException"]
namesForSubscription SubscriptionTraceAllocateSocket {} = ["AllocateSocket"]
namesForSubscription SubscriptionTraceCloseSocket {} = ["CloseSocket"]

namesForIPSubscription ::
     WithIPList (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForIPSubscription(WithIPList _ _ e) = namesForSubscription e

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

docIPSubscription :: Documented (WithIPList (SubscriptionTrace Socket.SockAddr))
docIPSubscription = Documented $ map withIPList (undoc docSubscription)
  where
    withIPList (DocMsg v nl comment) =
      DocMsg
        v
        nl
        ("IP Subscription: " <> comment)

--------------------------------------------------------------------------------
-- DNSSubscription Tracer
--------------------------------------------------------------------------------

namesForDNSSubscription ::
     NtN.WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForDNSSubscription(NtN.WithDomainName _ e) = namesForSubscription e

severityDNSSubscription ::
     NtN.WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> SeverityS
severityDNSSubscription NtN.WithDomainName {..} = case wdnEvent of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             _                         -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             _                         -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


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

docDNSSubscription :: Documented (WithDomainName (SubscriptionTrace Socket.SockAddr))
docDNSSubscription = Documented $ map withDomainName (undoc docSubscription)
  where
    withDomainName (DocMsg v nl comment) =
      DocMsg
        v
        nl
        ("DNS Subscription: " <> comment)

docSubscription :: Documented (SubscriptionTrace Socket.SockAddr)
docSubscription = Documented [
      DocMsg
        ["ConnectStart"]
        []
        "Connection Attempt Start with destination."
    , DocMsg
        ["ConnectEnd"]
        []
        "Connection Attempt end with destination and outcome."
    , DocMsg
        ["ConnectException"]
        []
        "Socket Allocation Exception with destination and the exception."
    , DocMsg
        ["SocketAllocationException"]
        []
        "Connection Attempt Exception with destination and exception."
    , DocMsg
        ["TryConnectToPeer"]
        []
        "Trying to connect to peer with address."
    , DocMsg
        ["SkippingPeer"]
        []
        "Skipping peer with address."
    , DocMsg
        ["SubscriptionRunning"]
        []
        "Required subscriptions started."
    , DocMsg
        ["SubscriptionWaiting"]
        []
        "Waiting on address with active connections."
    , DocMsg
        ["SubscriptionFailed"]
        []
        "Failed to start all required subscriptions."
    , DocMsg
        ["SubscriptionWaitingNewConnection"]
        []
        "Waiting delay time before attempting a new connection."
    , DocMsg
        ["Start"]
        []
        "Starting Subscription Worker with a valency."
    , DocMsg
        ["Restart"]
        []
        "Restarting Subscription after duration with desired valency and\
        \ current valency."
    , DocMsg
        ["ConnectionExist"]
        []
        "Connection exists to destination."
    , DocMsg
        ["UnsupportedRemoteAddr"]
        []
        "Unsupported remote target address."
    , DocMsg
        ["MissingLocalAddress"]
        []
        "Missing local address."
    , DocMsg
        ["ApplicationException"]
        []
        "Application Exception occurred."
    , DocMsg
        ["AllocateSocket"]
        []
        "Allocate socket to address."
    , DocMsg
        ["CloseSocket"]
        []
        "Closed socket to address."
  ]

--------------------------------------------------------------------------------
-- DNSResolver Tracer
--------------------------------------------------------------------------------

severityDNSResolver :: NtN.WithDomainName DnsTrace -> SeverityS
severityDNSResolver (NtN.WithDomainName _ ev) = case ev of
    DnsTraceLookupException {}  -> Error
    DnsTraceLookupAError {}     -> Error
    DnsTraceLookupAAAAError {}  -> Error
    DnsTraceLookupIPv6First     -> Debug
    DnsTraceLookupIPv4First     -> Debug
    DnsTraceLookupAResult {}    -> Debug
    DnsTraceLookupAAAAResult {} -> Debug

namesForDNSResolver :: NtN.WithDomainName DnsTrace -> [Text]
namesForDNSResolver (NtN.WithDomainName _ ev) = case ev of
    DnsTraceLookupException {}  -> ["LookupException"]
    DnsTraceLookupAError {}     -> ["LookupAError"]
    DnsTraceLookupAAAAError {}  -> ["LookupAAAAError"]
    DnsTraceLookupIPv6First     -> ["LookupIPv6First"]
    DnsTraceLookupIPv4First     -> ["LookupIPv4First"]
    DnsTraceLookupAResult {}    -> ["LookupAResult"]
    DnsTraceLookupAAAAResult {} -> ["LookupAAAAResult"]

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

docDNSResolver :: Documented (WithDomainName DnsTrace)
docDNSResolver = addDocumentedNamespace  [] docDNSResolver'

docDNSResolver' :: Documented (WithDomainName DnsTrace)
docDNSResolver' = Documented [
      DocMsg
        ["LookupException"]
        []
        "A DNS lookup exception occurred."
    , DocMsg
        ["LookupAError"]
        []
        "A lookup failed with an error."
    , DocMsg
        ["LookupAAAAError"]
        []
        "AAAA lookup failed with an error."
    , DocMsg
        ["LookupIPv6First"]
        []
        "Returning IPv6 address first."
    , DocMsg
        ["LookupIPv4First"]
        []
        "Returning IPv4 address first."
    , DocMsg
        ["LookupAResult"]
        []
        "Lookup A result."
    , DocMsg
        ["LookupAAAAResult"]
        []
        "Lookup AAAA result."
    ]

--------------------------------------------------------------------------------
-- ErrorPolicy Tracer
--------------------------------------------------------------------------------

severityErrorPolicy :: WithAddr Socket.SockAddr ErrorPolicyTrace -> SeverityS
severityErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {}               -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {}                -> Error
    ErrorPolicyResumePeer {}                    -> Debug
    ErrorPolicyKeepSuspended {}                 -> Debug
    ErrorPolicyResumeConsumer {}                -> Debug
    ErrorPolicyResumeProducer {}                -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {}  -> Error
    ErrorPolicyAcceptException {}               -> Error

namesForErrorPolicy :: WithAddr Socket.SockAddr ErrorPolicyTrace -> [Text]
namesForErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> ["SuspendPeer"]
    ErrorPolicySuspendConsumer {}               -> ["SuspendConsumer"]
    ErrorPolicyLocalNodeError {}                -> ["LocalNodeError"]
    ErrorPolicyResumePeer {}                    -> ["ResumePeer"]
    ErrorPolicyKeepSuspended {}                 -> ["KeepSuspended"]
    ErrorPolicyResumeConsumer {}                -> ["ResumeConsumer"]
    ErrorPolicyResumeProducer {}                -> ["ResumeProducer"]
    ErrorPolicyUnhandledApplicationException {} -> ["UnhandledApplicationException"]
    ErrorPolicyUnhandledConnectionException {}  -> ["UnhandledConnectionException"]
    ErrorPolicyAcceptException {}               -> ["AcceptException"]

instance Show addr => LogFormatting (NtN.WithAddr addr NtN.ErrorPolicyTrace) where
    forMachine _dtal (NtN.WithAddr addr ev) =
      mconcat [ "kind" .= String "ErrorPolicyTrace"
               , "address" .= show addr
               , "event" .= show ev ]
    forHuman (NtN.WithAddr addr ev) = "With address " <> showT addr <> ". " <> showT ev

-- WithDomainName has strict constructors
docErrorPolicy :: Documented (WithAddr Socket.SockAddr ErrorPolicyTrace)
docErrorPolicy = addDocumentedNamespace  [] docErrorPolicy'

--------------------------------------------------------------------------------
-- LocalErrorPolicy Tracer
--------------------------------------------------------------------------------

severityLocalErrorPolicy :: WithAddr NtC.LocalAddress ErrorPolicyTrace -> SeverityS
severityLocalErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {}               -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {}                -> Error
    ErrorPolicyResumePeer {}                    -> Debug
    ErrorPolicyKeepSuspended {}                 -> Debug
    ErrorPolicyResumeConsumer {}                -> Debug
    ErrorPolicyResumeProducer {}                -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {}  -> Error
    ErrorPolicyAcceptException {}               -> Error

namesForLocalErrorPolicy :: WithAddr NtC.LocalAddress ErrorPolicyTrace -> [Text]
namesForLocalErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> ["SuspendPeer"]
    ErrorPolicySuspendConsumer {}               -> ["SuspendConsumer"]
    ErrorPolicyLocalNodeError {}                -> ["LocalNodeError"]
    ErrorPolicyResumePeer {}                    -> ["ResumePeer"]
    ErrorPolicyKeepSuspended {}                 -> ["KeepSuspended"]
    ErrorPolicyResumeConsumer {}                -> ["ResumeConsumer"]
    ErrorPolicyResumeProducer {}                -> ["ResumeProducer"]
    ErrorPolicyUnhandledApplicationException {} -> ["UnhandledApplicationException"]
    ErrorPolicyUnhandledConnectionException {}  -> ["UnhandledConnectionException"]
    ErrorPolicyAcceptException {}               -> ["AcceptException"]


docLocalErrorPolicy :: Documented (WithAddr LocalAddress ErrorPolicyTrace)
docLocalErrorPolicy = addDocumentedNamespace  [] docErrorPolicy'

-- WithAddr has strict constructors

docErrorPolicy' :: Documented (WithAddr adr ErrorPolicyTrace)
docErrorPolicy' = Documented [
      DocMsg
        ["SuspendPeer"]
        []
        "Suspending peer with a given exception."
    , DocMsg
        ["SuspendConsumer"]
        []
        "Suspending consumer."
    , DocMsg
        ["LocalNodeError"]
        []
        "caught a local exception."
    , DocMsg
        ["ResumePeer"]
        []
        "Resume a peer (both consumer and producer)."
    , DocMsg
        ["KeepSuspended"]
        []
        "Consumer was suspended until producer will resume."
    , DocMsg
        ["ResumeConsumer"]
        []
        "Resume consumer."
    , DocMsg
        ["ResumeProducer"]
        []
        "Resume producer."
    , DocMsg
        ["UnhandledApplicationException"]
        []
        "An application threw an exception, which was not handled."
    , DocMsg
        ["UnhandledConnectionException"]
        []
        "'connect' threw an exception, which was not handled by any\
        \ 'ErrorPolicy'."
    , DocMsg
        ["AcceptException"]
        []
        "'accept' threw an exception."
    ]

--------------------------------------------------------------------------------
-- AcceptPolicy Tracer
--------------------------------------------------------------------------------

severityAcceptPolicy :: NtN.AcceptConnectionsPolicyTrace -> SeverityS
severityAcceptPolicy NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
severityAcceptPolicy NtN.ServerTraceAcceptConnectionHardLimit {}    = Warning
severityAcceptPolicy NtN.ServerTraceAcceptConnectionResume {}       = Info


namesForAcceptPolicy :: NtN.AcceptConnectionsPolicyTrace -> [Text]
namesForAcceptPolicy NtN.ServerTraceAcceptConnectionRateLimiting {} =
    ["ConnectionRateLimiting"]
namesForAcceptPolicy NtN.ServerTraceAcceptConnectionHardLimit {} =
    ["ConnectionHardLimit"]
namesForAcceptPolicy NtN.ServerTraceAcceptConnectionResume {} =
    ["ConnectionLimitResume"]

instance LogFormatting NtN.AcceptConnectionsPolicyTrace where
    forMachine _dtal (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
               , "delay" .= show delay
               , "numberOfConnection" .= show numOfConnections
               ]
    forMachine _dtal (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
               , "softLimit" .= show softLimit
               ]
    forMachine _dtal (NtN.ServerTraceAcceptConnectionResume numOfConnections) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionResume"
               , "numberOfConnection" .= show numOfConnections
               ]
    forHuman   = showT

docAcceptPolicy :: Documented NtN.AcceptConnectionsPolicyTrace
docAcceptPolicy = addDocumentedNamespace  [] docAcceptPolicy'

docAcceptPolicy' :: Documented NtN.AcceptConnectionsPolicyTrace
docAcceptPolicy' = Documented [
      DocMsg
        ["ConnectionRateLimiting"]
        []
        "Rate limiting accepting connections,\
        \ delaying next accept for given time, currently serving n connections."
      , DocMsg
        ["ConnectionHardLimit"]
        []
        "Hard rate limit reached,\
        \ waiting until the number of connections drops below n."
      , DocMsg
        ["ConnectionLimitResume"]
        []
        ""
  ]
