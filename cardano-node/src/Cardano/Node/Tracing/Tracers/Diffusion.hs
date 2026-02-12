{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}



{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Diffusion
  ( txsMempoolTimeoutHardCounterName
  , impliesMempoolTimeoutHard
  ) where


import           Cardano.Logging
import           Cardano.Node.Configuration.TopologyP2P ()
import           Control.Exception (fromException)
import           Network.Mux.Tracing ()
import           Ouroboros.Consensus.Mempool.API (ExnMempoolTimeout)
import qualified Ouroboros.Network.Diffusion.Types as Diff
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..), PoolStake (..),
                   TraceLedgerPeers (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS

import           Data.Aeson (Value (String), (.=))
import qualified Data.List as List
import           Data.Text (Text, pack)
import           Data.Typeable
import           Formatting

import qualified Network.Mux as Mux
#ifdef linux_HOST_OS
import           Network.Mux.TCPInfo (StructTCPInfo (..))
#endif
import           Network.Mux.Types (SDUHeader (..), unRemoteClockModel)
import           Network.TypedProtocol.Codec (AnyMessage (..))

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
    "ProposeVersions" -> Just Debug
    "ReplyVersions"   -> Just Debug
    "QueryReply"      -> Just Debug
    "AcceptVersion"   -> Just Debug
    "Refuse"          -> Just Debug
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

    severityFor (Namespace _ ["PickedLedgerPeer"]) _ = Just Debug
    severityFor (Namespace _ ["PickedLedgerPeers"]) _ = Just Info
    severityFor (Namespace _ ["PickedBigLedgerPeer"]) _ = Just Debug
    severityFor (Namespace _ ["PickedBigLedgerPeers"]) _ = Just Info
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
    severityFor (Namespace _ ["UsingBigLedgerPeerSnapshot"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["PickedLedgerPeer"]) = Just
      "Trace for a peer picked with accumulated and relative stake of its pool."
    documentFor (Namespace _ ["PickedLedgerPeers"]) = Just
      "Trace for the number of peers we wanted to pick and the list of peers picked."
    documentFor (Namespace _ ["PickedBigLedgerPeer"]) = Just
      "Trace for a big ledger peer picked with accumulated and relative stake of its pool."
    documentFor (Namespace _ ["PickedBigLedgerPeers"]) = Just
      "Trace for the number of big ledger peers we wanted to pick and the list of peers picked."
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
    documentFor (Namespace _ ["UsingBigLedgerPeerSnapshot"]) = Just $ mconcat
      [ "Trace for when a request for big ledger peers is fulfilled from the snapshot file"
      , " specified in the topology file."]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["PickedLedgerPeer"]
      , Namespace [] ["PickedLedgerPeers"]
      , Namespace [] ["PickedBigLedgerPeer"]
      , Namespace [] ["PickedBigLedgerPeers"]
      , Namespace [] ["FetchingNewLedgerState"]
      , Namespace [] ["DisabledLedgerPeers"]
      , Namespace [] ["TraceUseLedgerAfter"]
      , Namespace [] ["WaitingOnRequest"]
      , Namespace [] ["RequestForPeers"]
      , Namespace [] ["ReusingLedgerState"]
      , Namespace [] ["FallingBackToPublicRootPeers"]
      , Namespace [] ["NotEnoughLedgerPeers"]
      , Namespace [] ["NotEnoughBigLedgerPeers"]
      , Namespace [] ["TraceLedgerPeersDomains"]
      , Namespace [] ["UsingBigLedgerPeerSnapshot"]
      ]
