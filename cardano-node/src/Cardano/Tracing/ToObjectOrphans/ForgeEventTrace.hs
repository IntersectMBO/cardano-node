{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tracing.ToObjectOrphans.ForgeEventTrace () where

import           Cardano.Prelude hiding (show)
import           Prelude (show)

import           Data.Aeson hiding (Error)

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject(..))
import           Cardano.BM.Data.Tracer ( definePrivacyAnnotation
                                        , mkObject, trStructured)
import           Cardano.BM.Tracing ( DefinePrivacyAnnotation
                                    , DefineSeverity(..), ToObject(..)
                                    , Severity(..), Tracer(..)
                                    , TracingFormatting(..), Transformable(..)
                                    , traceWith, mkLOMeta)
import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView)
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Network.Block (SlotNo (..))

instance DefinePrivacyAnnotation (TraceForgeEvent blk tx)
instance DefineSeverity (TraceForgeEvent blk tx) where
  defineSeverity TraceForgedBlock {}            = Info
  defineSeverity TraceStartLeadershipCheck {}   = Info
  defineSeverity TraceNodeNotLeader {}          = Info
  defineSeverity TraceNodeIsLeader {}           = Info
  defineSeverity TraceNoLedgerState {}          = Warning
  defineSeverity TraceNoLedgerView {}           = Warning
  defineSeverity TraceBlockFromFuture {}        = Warning
  defineSeverity TraceAdoptedBlock {}           = Info
  defineSeverity TraceDidntAdoptBlock {}        = Warning
  defineSeverity TraceForgedInvalidBlock {}     = Alert

instance (Show blk, Show tx, ProtocolLedgerView blk) => Transformable Text IO (TraceForgeEvent blk tx) where
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer TextualRepresentation _verb tr = Tracer $ \s ->
    traceWith tr =<< LogObject <$> pure mempty
                               <*> mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
                               <*> pure (LogMessage . toS $ show s)
  trTransformer UserdefinedFormatting verb tr = trStructured verb tr

instance ProtocolLedgerView blk => ToObject (TraceForgeEvent blk tx) where
  toObject _verb (TraceAdoptedBlock slotNo _blk _txs) =
    mkObject
        [ "kind"    .= String "TraceAdoptedBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceBlockFromFuture currentSlot tip) =
    mkObject
        [ "kind" .= String "TraceBlockFromFuture"
        , "current slot" .= toJSON (unSlotNo currentSlot)
        , "tip" .= toJSON (unSlotNo tip)
        ]
  toObject _verb (TraceDidntAdoptBlock slotNo _) =
    mkObject
        [ "kind"    .= String "TraceDidntAdoptBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceForgedBlock slotNo _ _) =
    mkObject
        [ "kind"    .= String "TraceForgedBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceForgedInvalidBlock slotNo _ _) =
    mkObject
        [ "kind"    .= String "TraceForgedInvalidBlock"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNodeIsLeader slotNo) =
    mkObject
        [ "kind"    .= String "TraceNodeIsLeader"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNodeNotLeader slotNo) =
    mkObject
        [ "kind"    .= String "TraceNodeNotLeader"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNoLedgerState slotNo _blk) =
    mkObject
        [ "kind"    .= String "TraceNoLedgerState"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceNoLedgerView slotNo _) =
    mkObject
        [ "kind"    .= String "TraceNoLedgerView"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
  toObject _verb (TraceStartLeadershipCheck slotNo) =
    mkObject
        [ "kind"    .= String "TraceStartLeadershipCheck"
        , "slot"    .= toJSON (unSlotNo slotNo)
        ]
