module Cardano.Logging.Utils (
  uncurry3
  ) where


-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c
