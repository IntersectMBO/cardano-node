{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.Node.ToObjectOrphans
  (
    WithTip (..)
  , showTip
  , showWithTip
  ) where

import           Cardano.Prelude hiding (atomically, show)
import           Prelude (String, show, id)

import           Data.Aeson (Value (..), toJSON, (.=))
import           Data.Text (Text, pack)
import           Cardano.BM.Data.Tracer (ToObject (..),
                                         TracingVerbosity (..),
                                         Transformable (..),
                                         emptyObject, mkObject, trStructured)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB


-- | Tracing wrapper which includes current tip in the logs (thus it requires
-- it from the context).
--
-- TODO: this should be moved to `ouroboros-consensus`.  Running in a seprate
-- STM transaction we risk reporting  wrong tip.
--
data WithTip blk a =
    WithTip
      (Point blk)
      -- ^ current tip point
      a
      -- ^ data

showWithTip :: Condense (HeaderHash blk)
            => (a -> String)
            -> WithTip blk a
            -> String
showWithTip customShow (WithTip tip a) = "[" ++ (showTip MinimalVerbosity tip) ++ "] " ++ customShow a

showTip :: Condense (HeaderHash blk)
        => TracingVerbosity
        -> Point blk
        -> String
showTip verb tip =
    case pointHash tip of
        GenesisHash -> "genesis"
        BlockHash h -> trim $ condense h
  where
    trim :: [a] -> [a]
    trim = case verb of
        MinimalVerbosity -> take 7
        NormalVerbosity  -> take 7
        MaximalVerbosity -> id

instance ( Show a
         , Condense (HeaderHash blk)
         ) => Show (WithTip blk a) where

    show = showWithTip show



-- | instances of @ToObject@

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk)) where
    trTransformer = trStructured -- structure required, will call 'toObject'

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => ToObject (WithTip blk (ChainDB.TraceEvent blk)) where
    -- example: turn off any tracing of @TraceEvent@s when minimal verbosity level is set
    -- toObject MinimalVerbosity _ = emptyObject -- no output
    toObject verb (WithTip tip ev) =
        let evobj = toObject verb ev
        in
        if evobj == emptyObject
        then emptyObject
        else mkObject [ "kind" .= String "TraceEvent"
                      , "tip" .= showTip MinimalVerbosity tip
                      , "event" .= evobj
                      ]

instance ToObject SlotNo where
    toObject _verb slot =
        mkObject [ "kind" .= String "SlotNo"
                 , "slot" .= toJSON (unSlotNo slot) ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
    => ToObject (Point blk) where
    toObject MinimalVerbosity p = toObject NormalVerbosity p
    toObject verb p =
        mkObject [ "kind" .= String "Tip"
                 , "tip" .= showTip verb p ]

instance (Condense (HeaderHash blk), ProtocolLedgerView blk)
            => ToObject (ChainDB.TraceEvent blk) where

    toObject verb (ChainDB.TraceAddBlockEvent ev) = case ev of
        ChainDB.StoreButDontChange pt ->
            mkObject [ "kind" .= String "TraceAddBlockEvent.StoreButDontChange"
                     , "block" .= toObject verb pt ]
        ChainDB.TryAddToCurrentChain pt ->
            mkObject [ "kind" .= String "TraceAddBlockEvent.TryAddToCurrentChain"
                     , "block" .= toObject verb pt ]
        ChainDB.TrySwitchToAFork pt _ ->
            mkObject [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
                     , "block" .= toObject verb pt ]
        ChainDB.SwitchedToChain _ c ->
            mkObject [ "kind" .= String "TraceAddBlockEvent.SwitchedToChain"
                     , "newtip" .= showTip verb (AF.headPoint c) ]
        ChainDB.AddBlockValidation ev' -> case ev' of
            ChainDB.InvalidBlock err pt ->
                mkObject [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidBlock"
                         , "block" .= toObject verb pt
                         , "error" .= show err ]
            _ -> emptyObject
        _ -> emptyObject

    toObject MinimalVerbosity (ChainDB.TraceLedgerReplayEvent _ev) = emptyObject -- no output
    toObject verb (ChainDB.TraceLedgerReplayEvent ev) = case ev of
        LedgerDB.ReplayFromGenesis _replayTo ->
            mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromGenesis" ]
        LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
            mkObject [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromSnapshot"
                     , "snapshot" .= toObject verb snap
                     , "tip" .= show tip' ]
        _ -> emptyObject

    toObject MinimalVerbosity (ChainDB.TraceLedgerEvent _ev) = emptyObject -- no output
    toObject verb (ChainDB.TraceLedgerEvent ev) = case ev of
        LedgerDB.TookSnapshot snap pt ->
            mkObject [ "kind" .= String "TraceLedgerEvent.TookSnapshot"
                     , "snapshot" .= toObject verb snap
                     , "tip" .= show pt ]
        LedgerDB.DeletedSnapshot snap ->
            mkObject [ "kind" .= String "TraceLedgerEvent.DeletedSnapshot"
                     , "snapshot" .= toObject verb snap ]
        LedgerDB.InvalidSnapshot snap failure ->
            mkObject [ "kind" .= String "TraceLedgerEvent.InvalidSnapshot"
                     , "snapshot" .= toObject verb snap
                     , "failure" .= show failure ]

    toObject verb (ChainDB.TraceCopyToImmDBEvent ev) = case ev of
        ChainDB.CopiedBlockToImmDB pt ->
            mkObject [ "kind" .= String "TraceCopyToImmDBEvent.CopiedBlockToImmDB"
                     , "slot" .= toObject verb pt ]
        _ -> emptyObject

    toObject verb (ChainDB.TraceGCEvent ev) = case ev of
        ChainDB.PerformedGC slot ->
            mkObject [ "kind" .= String "TraceGCEvent.PerformedGC"
                     , "slot" .= toObject verb slot ]
        _ -> emptyObject

    toObject verb (ChainDB.TraceOpenEvent ev) = case ev of
        ChainDB.OpenedDB immTip tip' ->
            mkObject [ "kind" .= String "TraceOpenEvent.OpenedDB"
                     , "immtip" .= toObject verb immTip
                     , "tip" .= toObject verb tip' ]
        _ -> emptyObject

    toObject _ _ = emptyObject -- no output

instance ToObject LedgerDB.DiskSnapshot where
    toObject MinimalVerbosity snap = toObject NormalVerbosity snap
    toObject NormalVerbosity _ = mkObject [ "kind" .= String "snapshot" ]
    toObject MaximalVerbosity snap =
        mkObject [ "kind" .= String "snapshot"
                 , "snapshot" .= String (pack $ show snap) ]
