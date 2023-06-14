{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Cardano.TxGenerator.Internal.Fifo
Description : FIFO/queue data structure.

This is a moderately simple implementation, discussed by e.g. Okasaki
in <https://www.cambridge.org/core/journals/journal-of-functional-programming/article/simple-and-efficient-purely-functional-queues-and-deques/7B3036772616B39E87BF7FBD119015AB>
in very close to identical form save for being in SML vs. Haskell in
section 2, and, somewhat earlier, by Hood and Melville (1981),
Gries (1981) and Burton (1982), albeit in other languages. This may
not become large enough for it to ever matter in the tx-generator,
but Tarjan's structure at <https://dl.acm.org/doi/10.1145/324133.324139>
might be worthwhile in that case, most likely to happen if someone
notices this existing and wants to use it somewhere where it might be
significantly more stressed.
-}
module  Cardano.TxGenerator.Internal.Fifo
        (module Cardano.TxGenerator.Internal.Fifo)
        where

-- This is to be used single threaded behind an MVar.

-- | This implements the section 2 __Queues as Paired Lists__
-- structure described by Okasaki.
data Fifo a = Fifo ![a] -- ^ the front, from where removals are taken
                   ![a] -- ^ the rear, where insertions are put


-- | The empty queue has no contents to sit in either the front or rear.
emptyFifo :: Fifo a
emptyFifo = Fifo [] []

-- | The arrivals inserted at the rear remain in reverse order until
-- there is a need to dequeue that can't be satisfied from the front.
-- Warning : bad complexity when used as a persistent data structure.
toList :: Fifo a -> [a]
toList (Fifo x y) = x ++ reverse y

-- | The rear's reverse ordering allows this to be a simple cons.
insert :: Fifo a -> a -> Fifo a
insert (Fifo x y) e = Fifo x $ e:y

-- | Potential failure demands the use of `Maybe`. Empty front
-- lists trigger reversals of the rear list and the Fifo part of
-- the result becomes a pure front list, namely the tail of that
-- reversal.
remove :: Fifo a -> Maybe (Fifo a, a)
remove fifo = case fifo of
  Fifo [] []    -> Nothing
  Fifo (h:t) y  -> Just (Fifo t y, h)
  Fifo [] y     -> let ~(h:t) = reverse y in Just (Fifo t [], h)

-- | Dequeueing /n/ items just iterates calling remove within the
-- `Maybe` monad. Removing n from a Fifo of length k when k < n is
-- regarded as a failure.
removeN :: Int -> Fifo a -> Maybe (Fifo a, [a])
removeN 0 f = return (f, [])
removeN n f = do
  (a, h) <- remove f
  (r, t) <- removeN (pred n) a
  return (r, h:t)
