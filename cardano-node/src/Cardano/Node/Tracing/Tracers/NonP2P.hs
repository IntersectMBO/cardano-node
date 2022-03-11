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
namesForIPSubscription(WithIPList _ _ e) = "IP" : namesForSubscription e

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
      DocMsg (WithIPList anyProto [] v) nl ("IP Subscription: " <> comment)

--------------------------------------------------------------------------------
-- DNSSubscription Tracer
--------------------------------------------------------------------------------

namesForDNSSubscription ::
     NtN.WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForDNSSubscription(NtN.WithDomainName _ e) = "DNS" : namesForSubscription e

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
      DocMsg (WithDomainName anyProto v) nl ("DNS Subscription: " <> comment)


docSubscription :: Documented (SubscriptionTrace Socket.SockAddr)
docSubscription = Documented [
      DocMsg
        (SubscriptionTraceConnectStart anyProto)
        []
        "Connection Attempt Start with destination."
    , DocMsg
        (SubscriptionTraceConnectEnd anyProto ConnectSuccess)
        []
        "Connection Attempt end with destination and outcome."
    , DocMsg
        (SubscriptionTraceSocketAllocationException anyProto (anyProto :: SomeException))
        []
        "Socket Allocation Exception with destination and the exception."
    , DocMsg
        (SubscriptionTraceConnectException anyProto (anyProto :: SomeException))
        []
        "Connection Attempt Exception with destination and exception."
    , DocMsg
        (SubscriptionTraceTryConnectToPeer anyProto)
        []
        "Trying to connect to peer with address."
    , DocMsg
        (SubscriptionTraceSkippingPeer anyProto)
        []
        "Skipping peer with address."
    , DocMsg
        SubscriptionTraceSubscriptionRunning
        []
        "Required subscriptions started."
    , DocMsg
        (SubscriptionTraceSubscriptionWaiting 1)
        []
        "Waiting on address with active connections."
    , DocMsg
        SubscriptionTraceSubscriptionFailed
        []
        "Failed to start all required subscriptions."
    , DocMsg
        (SubscriptionTraceSubscriptionWaitingNewConnection anyProto)
        []
        "Waiting delay time before attempting a new connection."
    , DocMsg
        (SubscriptionTraceStart 1)
        []
        "Starting Subscription Worker with a valency."
    , DocMsg
        (SubscriptionTraceRestart anyProto 1 2)
        []
        "Restarting Subscription after duration with desired valency and\
        \ current valency."
    , DocMsg
        (SubscriptionTraceConnectionExist anyProto)
        []
        "Connection exists to destination."
    , DocMsg
        (SubscriptionTraceUnsupportedRemoteAddr anyProto)
        []
        "Unsupported remote target address."
    , DocMsg
        SubscriptionTraceMissingLocalAddress
        []
        "Missing local address."
    , DocMsg
        (SubscriptionTraceApplicationException anyProto (anyProto :: SomeException))
        []
        "Application Exception occurred."
    , DocMsg
        (SubscriptionTraceAllocateSocket anyProto)
        []
        "Allocate socket to address."
    , DocMsg
        (SubscriptionTraceCloseSocket anyProto)
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
docDNSResolver = Documented [
      DocMsg
        (WithDomainName anyProto
          (DnsTraceLookupException anyProto))
        []
        "A DNS lookup exception occurred."
    , DocMsg
        (WithDomainName anyProto
          (DnsTraceLookupAError anyProto))
        []
        "A lookup failed with an error."
    , DocMsg
        (WithDomainName anyProto
          (DnsTraceLookupAAAAError anyProto))
        []
        "AAAA lookup failed with an error."
    , DocMsg
        (WithDomainName anyProto
          DnsTraceLookupIPv4First)
        []
        "Returning IPv4 address first."
    , DocMsg
        (WithDomainName anyProto
          DnsTraceLookupIPv6First)
        []
        "Returning IPv6 address first."
    , DocMsg
        (WithDomainName anyProto
          DnsTraceLookupIPv6First)
        []
        "Returning IPv6 address first."
    , DocMsg
        (WithDomainName anyProto
          (DnsTraceLookupAResult [anyProto]))
        []
        "Lookup A result."
    , DocMsg
        (WithDomainName anyProto
          (DnsTraceLookupAAAAResult [anyProto]))
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
docErrorPolicy = docErrorPolicy' anyProto

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
docLocalErrorPolicy = docErrorPolicy' anyProto

-- WithAddr has strict constructors

docErrorPolicy' :: adr -> Documented (WithAddr adr ErrorPolicyTrace)
docErrorPolicy' adr = Documented [
      DocMsg
        (WithAddr adr
          (ErrorPolicySuspendPeer anyProto anyProto anyProto))
        []
        "Suspending peer with a given exception."
    , DocMsg
        (WithAddr adr
          (ErrorPolicySuspendConsumer anyProto anyProto))
        []
        "Suspending consumer."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyLocalNodeError anyProto))
        []
        "caught a local exception."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumePeer)
        []
        "Resume a peer (both consumer and producer)."
    , DocMsg
        (WithAddr adr
          ErrorPolicyKeepSuspended)
        []
        "Consumer was suspended until producer will resume."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumeConsumer)
        []
        "Resume consumer."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumeProducer)
        []
        "Resume producer."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyUnhandledApplicationException anyProto))
        []
        "An application threw an exception, which was not handled."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyUnhandledConnectionException anyProto))
        []
        "'connect' threw an exception, which was not handled by any\
        \ 'ErrorPolicy'."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyAcceptException anyProto))
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
docAcceptPolicy = Documented [
      DocMsg
        (NtN.ServerTraceAcceptConnectionRateLimiting anyProto 2)
        []
        "Rate limiting accepting connections,\
        \ delaying next accept for given time, currently serving n connections."
      , DocMsg
        (NtN.ServerTraceAcceptConnectionHardLimit 2)
        []
        "Hard rate limit reached,\
        \ waiting until the number of connections drops below n."
  ]
