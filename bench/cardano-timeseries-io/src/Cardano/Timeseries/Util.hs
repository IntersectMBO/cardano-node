module Cardano.Timeseries.Util(toMaybe, maybeToEither, range, getTimeMs, diag) where

import           Prelude hiding (head)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word (Word64)


toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True x = Just x

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither err Nothing = Left err

-- | Return submap containing only keys in (lo, hi).
-- | Complexity: O(log(n)).
range :: Ord k => k -> k -> Map k v -> Map k v
range lo hi m =
    let (_, m1) = Map.split lo m  -- drop all =< lo
        (m2, _) = Map.split hi m1 -- drop all >=) hi
    in m2

getTimeMs :: IO Word64
getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

diag :: a -> (a, a)
diag x = (x, x)
