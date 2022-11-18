{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Analysis.API.Field (module Cardano.Analysis.API.Field) where

import Cardano.Prelude          hiding (head, show)

import Data.CDF
import Data.String                     (fromString)
import Data.Text                       (unpack)

import Cardano.JSON
import Cardano.Util
import Cardano.Analysis.API.Ground
import Cardano.Analysis.API.Run


data Scale
  = Lin
  | Log
  deriving (Eq, Show)

data Range
  = Free   -- No range restriction
  | Z0 Int -- 1-based range
  | Z1 Int -- 1-based range
  deriving (Eq, Show)

data Unit
  = Sec  -- Second
  | Hz   -- Hertz
  | B    -- Byte
  | KB   -- Kibibyte: 2^10
  | MB   -- Mibibyte: 2^20
  | KBs  -- Kibibyte/s
  | MBs  -- Mibibyte/s
  | Epo  -- Epoch
  | Slo  -- Slots
  | Blk  -- Blocks
  | Hsh  -- Hash
  | Hos  -- Host
  | Sig  -- Sign: +/-
  | Pct  -- Unspecified ratio, percents
  | Ev   -- Events
  | Ix   -- Unspecified index
  | Len  -- Unspecified length
  | Rto  -- Unspecified ratio
  | Uni  -- Unspecified unit
  deriving (Eq, Show)

renderUnit :: Unit -> Text
renderUnit = \case
    Sec -> "sec"
    Hz  -> "Hz"
    B   -> "B"
    KB  -> "KB"
    MB  -> "MB"
    KBs -> "KB/s"
    MBs -> "MB/s"
    Epo -> "epoch"
    Slo -> "slots"
    Blk -> "blocks"
    Hsh -> "hash"
    Hos -> "host"
    Sig -> "+/-"
    Pct -> "%"
    Ev  -> ""
    Ix  -> ""
    Len -> ""
    Rto -> ""
    Uni -> ""

data Width
  = W0
  | W1
  | W2
  | W3
  | W4
  | W5
  | W6
  | W7
  | W8
  | W9
  | W10
  | W11
  | W12
  deriving (Eq, Enum, Ord, Show)

{-# INLINE width #-}
width :: Width -> Int
width = fromEnum

-- | Encapsulate all metadata about a metric (a projection) of
--   a certain projectible (a kind of analysis results):
--     - first parameter encapsulates the projection descriptor
--     - second parameter sets the arity (I vs. CDF I)
--     - third parameter is the projectible indexed by arity
data Field (s :: (Type -> Type) -> k -> Type) (p :: Type -> Type) (a :: k)
  = Field
  { fId          :: Text
  , fHead1       :: Text
  , fHead2       :: Text
  , fWidth       :: Width
  , fUnit        :: Unit
  , fScale       :: Scale
  , fRange       :: Range
  , fSelect      :: s p a
  , fShortDesc   :: Text
  , fDescription :: Text
  }

class CDFFields a p where
  cdfFields        :: [Field DSelect p a]
  fieldJSONOverlay :: Field DSelect p a -> Object -> [Maybe Object]

class TimelineFields a where
  data TimelineComments a :: Type
  timelineFields :: Run -> [Field ISelect I a]
  rtCommentary   :: a -> TimelineComments a -> [Text]
  rtCommentary _ _ = []

data DSelect p a
  = DInt    (a p -> CDF p Int)
  | DWord64 (a p -> CDF p Word64)
  | DFloat  (a p -> CDF p Double)
  | DDeltaT (a p -> CDF p NominalDiffTime)

data ISelect p a
  = IInt     (a -> Int)
  | IWord64  (a -> Word64)
  | IWord64M (a -> SMaybe Word64)
  | IFloat   (a -> Double)
  | IDeltaT  (a -> NominalDiffTime)
  | IDeltaTM (a -> SMaybe NominalDiffTime)
  | IText    (a -> Text)

filterFields :: CDFFields a p
             => (Field DSelect p a -> Bool) -> [Field DSelect p a]
filterFields f = filter f cdfFields

mapField :: a p -> (forall v. Divisible v => CDF p v -> b) -> Field DSelect p a -> b
mapField x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($x) ->r) -> r
    DWord64 (cdfProj . ($x) ->r) -> r
    DFloat  (cdfProj . ($x) ->r) -> r
    DDeltaT (cdfProj . ($x) ->r) -> r

tryOverlayFieldDescription :: Field DSelect p a -> Object -> Maybe Object
tryOverlayFieldDescription Field{..} =
  alterSubObject (Just . overlayJSON [ ("description", String fDescription)
                                     , ("shortDesc",   String fShortDesc)
                                     ])
                 (fromString $ unpack fId)

processFieldOverlays :: forall a p. CDFFields a p => a p -> Object -> Object
processFieldOverlays _ o =
  foldr' (\f -> handleMiss f . fieldJSONOverlay @a @p f) o cdfFields
 where
   handleMiss Field{..} =
     fromMaybe (error $ "fieldJSONOverlay:  failed to handle field " <> unpack fId)
     . getFirst
     . mconcat
     . fmap First


mapSomeFieldCDF :: forall p c a. (forall b. Divisible b => CDF p b -> c) -> a p -> DSelect p a -> c
mapSomeFieldCDF f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)

