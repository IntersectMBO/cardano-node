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


data Scale
  = Lin
  | Log
  deriving (Eq, Show)

data Range
  = Free   -- No range restriction
  | Z0 Int -- 1-based range
  | Z1 Int -- 1-based range
  | R01
  deriving (Eq, Show)

data Unit
  = Sec  -- Second
  | Hz   -- Hertz
  | B    -- Byte
  | KB   -- Kibibyte: 2^10
  | MB   -- Mibibyte: 2^20
  | KBs  -- Kibibyte/s
  | MBs  -- Mibibyte/s
  | Era  -- Era
  | Epo  -- Epoch
  | Slo  -- Slots
  | Blk  -- Blocks
  | Hsh  -- Hash
  | Hos  -- Host
  | Sig  -- Sign: +/-
  | Pct  -- Unspecified ratio, percents
  | Ev   -- Events
  | KEv  -- Events: 10^3
  | Dat  -- Date
  | Tim  -- Time
  | Ver  -- Version
  | Ix   -- Unspecified index
  | Len  -- Unspecified length
  | Cnt  -- Unspecified count
  | Rto  -- Unspecified ratio
  | Uni  -- Unspecified unit
  | Id   -- Unspefified identifier
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
    Era -> "era"
    Epo -> "epoch"
    Slo -> "slots"
    Blk -> "blocks"
    Hsh -> "hash"
    Hos -> "host"
    Sig -> "+/-"
    Pct -> "%"
    Ev  -> "#"
    KEv -> "#"
    Dat -> "on"
    Tim -> "at"
    Ver -> "v"
    Ix  -> "[]"
    Len -> "#"
    Cnt -> "#"
    Rto -> "/"
    Uni -> "#"
    Id  -> ""

data Width
  = Wno
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
  | W13
  | W14
  | W15
  | W16
  | W17
  | W18
  | W19
  | W20
  | W21
  deriving (Eq, Enum, Ord, Show)

data Precision
  = P0
  | P1
  | P2
  | P3
  deriving (Eq, Enum, Ord, Show)

{-# INLINE width #-}
width :: Width -> Int
width Wno = 80
width x   = fromEnum x

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
  , fPrecision   :: Precision
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
  timelineFields :: [Field ISelect I a]
  rtCommentary   :: a -> TimelineComments a -> [Text]
  rtCommentary _ _ = []

data FSelect where
  ISel :: TimelineFields a   => (Field ISelect I a -> Bool) -> FSelect
  DSel :: CDFFields      a p => (Field DSelect p a -> Bool) -> FSelect

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
  | IDate    (a -> UTCTime)
  | ITime    (a -> UTCTime)
  | IText    (a -> Text)

dFields :: [FieldName] -> Field DSelect p a -> Bool
dFields fs Field{fId} = FieldName fId `elem` fs

iFields :: [FieldName] -> Field ISelect I a -> Bool
iFields fs Field{fId} = FieldName fId `elem` fs

filterFields :: CDFFields a p
             => (Field DSelect p a -> Bool) -> [Field DSelect p a]
filterFields f = filter f cdfFields

mapField :: a p -> (forall v. Divisible v => CDF p v -> b) -> Field DSelect p a -> b
mapField x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($ x) ->r) -> r
    DWord64 (cdfProj . ($ x) ->r) -> r
    DFloat  (cdfProj . ($ x) ->r) -> r
    DDeltaT (cdfProj . ($ x) ->r) -> r

mapFieldWithKey :: a p -> (forall v. Divisible v => Field DSelect p a -> CDF p v -> b) -> Field DSelect p a -> b
mapFieldWithKey x cdfProj f@Field{..} =
  case fSelect of
    DInt    (cdfProj f . ($ x) ->r) -> r
    DWord64 (cdfProj f . ($ x) ->r) -> r
    DFloat  (cdfProj f . ($ x) ->r) -> r
    DDeltaT (cdfProj f . ($ x) ->r) -> r

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

