{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Network.Formatting
  (
  ) where

import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Text.Show

import           Cardano.TraceDispatcher.Common.Formatting ()

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)

import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync


instance LogFormatting (AnyMessageAndAgency ps)
      => LogFormatting (TraceSendRecv ps) where
  forMachine dtal (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (TraceSendMsg m) = "Send: " <> forHuman m
  forHuman (TraceRecvMsg m) = "Receive: " <> forHuman m

  asMetrics (TraceSendMsg m) = asMetrics m
  asMetrics (TraceRecvMsg m) = asMetrics m

instance LogFormatting (AnyMessageAndAgency (ChainSync blk pt tip)) where
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]
