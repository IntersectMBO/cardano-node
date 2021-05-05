{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}


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
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import           Ouroboros.Consensus.Byron.Ledger (Query)


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

instance LogFormatting (AnyMessageAndAgency (LTS.LocalTxSubmission tx err)) where
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgSubmitTx{}) =
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]


instance (forall result. Show (Query blk result))
      => LogFormatting (AnyMessageAndAgency (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]
