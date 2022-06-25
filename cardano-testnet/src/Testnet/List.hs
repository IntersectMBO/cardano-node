module Testnet.List
  ( dropNth
  , zipWith8
  , zipWith10
  ) where

import           Data.Int
import           GHC.Num

-- | Drop the zero-based n-th element from the list.
dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth 0 (_:as) = as
dropNth i (a:as) = a:dropNth (i - 1) as

zipWith8 :: ()
  => (a -> b -> c -> d -> e -> f -> g -> h -> z)
  -> [a]
  -> [b]
  -> [c]
  -> [d]
  -> [e]
  -> [f]
  -> [g]
  -> [h]
  -> [z]
zipWith8 fun (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
  fun a b c d e f g h:zipWith8 fun as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ =
  []

zipWith10 :: ()
  => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z)
  -> [a]
  -> [b]
  -> [c]
  -> [d]
  -> [e]
  -> [f]
  -> [g]
  -> [h]
  -> [i]
  -> [j]
  -> [z]
zipWith10 fun (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) (j:js) =
  fun a b c d e f g h i j:zipWith10 fun as bs cs ds es fs gs hs is js
zipWith10 _ _ _ _ _ _ _ _ _ _ _ =
  []
