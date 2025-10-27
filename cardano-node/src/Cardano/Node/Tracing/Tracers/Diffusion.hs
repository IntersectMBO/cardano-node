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

import           Ouroboros.Network.Logging ()
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import           Network.Mux.Logging ()
import           Network.TypedProtocol.Codec (AnyMessage (..))
import           Data.Aeson (Value (String), (.=))
import           Data.Text (Text, pack)

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

