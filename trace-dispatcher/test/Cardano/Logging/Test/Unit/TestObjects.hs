{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Logging.Test.Unit.TestObjects (

    TraceForgeEvent(..)
  , LogBlock(..)
  , SlotNo(..)
  , message1
  , message2
  , message3
  , message4
) where

import           Cardano.Logging

import qualified Data.Aeson as AE
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Kind (Type)
import           Data.Text (pack)
import           Data.Word (Word64)
import           GHC.Generics
import           Text.Printf (printf)

newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON SlotNo where
    toEncoding = AE.genericToEncoding AE.defaultOptions

newtype Point block = Point
    { getPoint :: WithOrigin (Block SlotNo (HeaderHash block))
    }
  deriving (Generic)


instance AE.ToJSON (Point LogBlock) where
    toEncoding = AE.genericToEncoding AE.defaultOptions

class ( Eq       (HeaderHash b)
      , Ord      (HeaderHash b)
      , Show     (HeaderHash b)
      ) => StandardHash b

deriving newtype instance StandardHash block => Eq   (Point block)
deriving newtype instance StandardHash block => Ord  (Point block)
deriving newtype instance StandardHash block => Show (Point block)

data Block slot hash = Block
  { blockPointSlot :: !slot
  , blockPointHash :: !hash
  }
  deriving (Eq, Ord, Show, Generic)

instance (AE.ToJSON h, AE.ToJSON s) => AE.ToJSON (Block s h) where
    toEncoding = AE.genericToEncoding AE.defaultOptions

data WithOrigin t = Origin | At !t
  deriving
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance AE.ToJSON a => AE.ToJSON (WithOrigin a) where
   toEncoding = AE.genericToEncoding AE.defaultOptions

newtype BlockNo = BlockNo {unBlockNo :: Word64}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (Enum, Bounded, Num)

instance AE.ToJSON BlockNo where
    toEncoding = AE.genericToEncoding AE.defaultOptions

data LogBlock = LogBlock
  deriving(Eq, Ord, Show, StandardHash)

type family HeaderHash b :: Type

type instance HeaderHash LogBlock = LogHash

newtype LogHash = LogHash { unLogHash :: Word64 }
  deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON LogHash where
    toEncoding = AE.genericToEncoding AE.defaultOptions

-- The actual test trace messages
data TraceForgeEvent blk
  = TraceStartLeadershipCheck SlotNo
  | TraceSlotIsImmutable SlotNo (Point blk) BlockNo
  | TraceBlockFromFuture SlotNo SlotNo
  deriving (Eq, Show, Generic)

instance MetaTrace (TraceForgeEvent blk) where
  namespaceFor TraceStartLeadershipCheck {} = Namespace [] ["StartLeadershipCheck"]
  namespaceFor TraceSlotIsImmutable {} = Namespace [] ["SlotIsImmutable"]
  namespaceFor TraceBlockFromFuture {} = Namespace  [] ["BlockFromFuture"]

  severityFor (Namespace _ ["StartLeadershipCheck"]) _ = Just Info
  severityFor (Namespace _ ["SlotIsImmutable"]) _ = Just Error
  severityFor (Namespace _ ["BlockFromFuture"]) _ = Just Error
  severityFor _ns _ = Nothing

  documentFor (Namespace _ ["StartLeadershipCheck"]) = Just
    "Start of the leadership check\n\
    \\n\
    \We record the current slot number."
  documentFor (Namespace _ ["SlotIsImmutable"]) = Just $ mconcat
    [ "Leadership check failed: the tip of the ImmutableDB inhabits the\n"
    , "current slot\n"
    , "\n"
    , "This might happen in two cases.\n"
    , "\n"
    , "1. the clock moved backwards, on restart we ignored everything from the\n"
    , "   VolatileDB since it's all in the future, and now the tip of the\n"
    , "   ImmutableDB points to a block produced in the same slot we're trying\n"
    , "   to produce a block in\n"
    , "\n"
    , "2. k = 0 and we already adopted a block from another leader of the same\n"
    , "   slot.\n"
    , "\n"
    , "We record both the current slot number as well as the tip of the\n"
    , "ImmutableDB.\n"
    , "\n"
    , "See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
    ]
  documentFor (Namespace _ ["BlockFromFuture"]) = Just $ mconcat
    [ "Leadership check failed: the current chain contains a block from a slot\n"
    , "/after/ the current slot\n"
    , "\n"
    , "This can only happen if the system is under heavy load.\n"
    , "\n"
    , "We record both the current slot number as well as the slot number of the\n"
    , "block at the tip of the chain.\n"
    , "\n"
    , "See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
    ]
  documentFor _ns = Nothing

  metricsDocFor (Namespace _ _) = []

  allNamespaces = [ Namespace [] ["StartLeadershipCheck"]
                  , Namespace [] ["SlotIsImmutable"]
                  , Namespace [] ["BlockFromFuture"]]

instance LogFormatting (TraceForgeEvent LogBlock) where
  forHuman (TraceStartLeadershipCheck slotNo) = pack $
    printf
      "Checking for leadership in slot %u"
      (unSlotNo slotNo)
  forHuman (TraceSlotIsImmutable slotNo immutableTipPoint immutableTipBlkNo) = pack $
    printf
      ( mconcat
        [ "Couldn't forge block because slot %u is immutable. "
        , " Immutable tip: %s, immutable tip block no: %i."
        ]
      )
      (unSlotNo slotNo)
      (show immutableTipPoint)
      (unBlockNo immutableTipBlkNo)
  forHuman (TraceBlockFromFuture currentSlot tipSlot) = pack $
    printf
      "Couldn't forge block because tip %u of slot %u is in the future."
      (unSlotNo tipSlot)
      (unSlotNo currentSlot)

  forMachine _verb (TraceStartLeadershipCheck slotNo) =
    KeyMap.fromList
      [ "kind" AE..= AE.String "TraceStartLeadershipCheck"
      , "slot" AE..= AE.toJSON (unSlotNo slotNo)
      ]
  forMachine _verb (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    KeyMap.fromList
      [ "kind" AE..= AE.String "TraceSlotIsImmutable"
      , "slot" AE..= AE.toJSON (unSlotNo slotNo)
      , "tip" AE..= showT tipPoint
      , "tipBlockNo" AE..= AE.toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _verb (TraceBlockFromFuture currentSlot tip) =
    KeyMap.fromList
      [ "kind" AE..= AE.String "TraceBlockFromFuture"
      , "current slot" AE..= AE.toJSON (unSlotNo currentSlot)
      , "tip" AE..= AE.toJSON (unSlotNo tip)
      ]

  asMetrics (TraceStartLeadershipCheck slotNo) =
    [IntM "cardano.node.aboutToLeadSlotLast" (fromIntegral $ unSlotNo slotNo)]
  asMetrics (TraceSlotIsImmutable slot _tipPoint _tipBlkNo) =
    [IntM "cardano.node.slotIsImmutable" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockFromFuture slot _slotNo) =
    [IntM "cardano.node.blockFromFuture" (fromIntegral $ unSlotNo slot)]



message1 :: TraceForgeEvent LogBlock
message1 = TraceStartLeadershipCheck (SlotNo 1001)

message2 :: TraceForgeEvent LogBlock
message2 = TraceSlotIsImmutable (SlotNo 3333) (Point Origin) (BlockNo 1)

message3 :: TraceForgeEvent LogBlock
message3 = TraceBlockFromFuture (SlotNo 4400) (SlotNo 300)

message4 :: TraceForgeEvent LogBlock
message4 = TraceStartLeadershipCheck (SlotNo 2002)
