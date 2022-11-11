{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Analysis.Field (module Cardano.Analysis.Field) where

import Cardano.Prelude          hiding (head, show)

import Data.CDF

import Cardano.Analysis.Ground
import Cardano.Analysis.Run


-- | Encapsulate all metadata about a metric (a projection) of
--   a certain projectible (a kind of analysis results):
--     - first parameter encapsulates the projection descriptor
--     - second parameter sets the arity (I vs. CDF I)
--     - third parameter is the projectible indexed by arity
data Field (s :: (Type -> Type) -> k -> Type) (p :: Type -> Type) (a :: k)
  = Field
  { fWidth   :: Int
  , fLeftPad :: Int
  , fId      :: Text
  , fHead1   :: Text
  , fHead2   :: Text
  , fSelect  :: s p a
  , fDesc    :: Text
  }

class CDFFields a p where
  cdfFields :: [Field DSelect p a]

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
  = IInt    (a -> Int)
  | IWord64 (a -> Word64)
  | IFloat  (a -> Double)
  | IDeltaT (a -> NominalDiffTime)
  | IText   (a -> Text)


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

mapSomeFieldCDF :: forall p c a. (forall b. Divisible b => CDF p b -> c) -> a p -> DSelect p a -> c
mapSomeFieldCDF f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)
