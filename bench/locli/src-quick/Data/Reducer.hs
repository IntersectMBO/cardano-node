{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Reducer (module Data.Reducer) where

import qualified Data.Foldable as F (foldl')
import           Data.Function (on)
import           Data.Kind (Type)
import           Data.Maybe


class Reducer r where
  type family Elem  r   :: Type
  type family Result r  :: Type
  type family Accum r   :: Type

  initialOf :: r -> Accum r
  reducerOf :: r -> Accum r -> Elem r -> Accum r
  resultOf  :: r -> Accum r -> Result r


reduce :: (Foldable f, Reducer r) => r -> f (Elem r) -> Result r
reduce r = resultOf r . F.foldl' (reducerOf r) (initialOf r)


data Chain r1 r2 = Chain r1 r2

instance  ( Foldable f2
          , Reducer r1
          , Reducer r2
          , Result r1 ~ f2 (Elem r2)
          ) => Reducer (Chain r1 r2) where

  type instance Elem    (Chain r1 r2)  = Elem r1
  type instance Result  (Chain r1 r2)  = Result r2
  type instance Accum   (Chain r1 r2)  = Accum r1

  initialOf (Chain r1 _)  = initialOf r1
  reducerOf (Chain r1 _)  = reducerOf r1
  resultOf  (Chain r1 r2) = reduce r2 . resultOf r1

(<->) :: r1 -> r2 -> Chain r1 r2
r1 <-> r2 = Chain r1 r2

-- Explicitly chain reducers with a projection function that extracts a Foldable and
-- an initial accumulator value from the first reducer's reult value
chainWith ::
  ( Foldable f1
  , Foldable f2
  , Reducer r1
  , Reducer r2
  )
  => (Result r1 -> (Accum r2, f2 (Elem r2))) -> r1 -> r2 -> f1 (Elem r1) -> Result r2
chainWith f r1 r2 =
  resultOf r2 . uncurry (F.foldl' (reducerOf r2)) . f . reduce r1


data HigherOrder a where
  Threshold :: (a -> a -> Bool) -> HigherOrder a  -- a function that - given two data points - classifies them as passing a threshold, causing the reducer to emit a corresponding data point.
  Scan      :: (a -> a -> a)    -> HigherOrder a  -- prefix sum

instance Reducer (HigherOrder a) where
  type instance Elem   (HigherOrder a)  = a
  type instance Result (HigherOrder a)  = [a]
  type instance Accum  (HigherOrder a)  = (Result (HigherOrder a), Maybe a)

  initialOf _ = ([], Nothing)

  reducerOf (Threshold f) = go where
    -- still in intial state? always use the head of the original sequence as first data point
    go ([], _) h            = ([h], Just h)
    go (acc@(h:_), _) next
      | f h next            = (next:acc, Just next)
      | otherwise           = (acc, Just next)
  reducerOf (Scan f) = go where
    go (_, Nothing) h            = ([], Just h)
    go (acc, Just current) next  = (f current next : acc, Just next)

  resultOf Threshold{} accum = case accum of
    -- there where no data points in the original sequence
    ([], _)         -> []
    -- always use the last data point of the original sequence
    (acc, lastVal)  -> reverse $ maybeToList lastVal ++ acc
  resultOf Scan{} acc = case fst acc of
    [] -> []
    xs -> reverse xs

-- this extracts deltas from y[x[n]] to y[x[n-1]]; can be sensibly applied to a cumulative measurement like CPU ticks
deltas :: Num y => HigherOrder (x, y)
deltas = Scan $ \(_, y) (x', y') -> (x', y' - y)

-- this derives a steps plot from a curve, emitting a data point whenever there's a change on the y-axis
changes :: Eq y => HigherOrder (x, y)
changes = Threshold ((/=) `on` snd)
