{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- HLINT ignore "Use list literal pattern" -}
module Cardano.Util
  ( module Prelude
  , module Util
  , module Data.Aeson
  , module Data.IntervalMap.FingerTree
  , module Data.SOP.Strict
  , module Data.List.Split
  , module Data.Time.Clock
  , module Data.Time.Clock.POSIX
  , module Data.Tuple.Extra
  , module Cardano.Ledger.BaseTypes
  , module Control.Arrow
  , module Control.Applicative
  , module Control.Concurrent.Async
  , module Control.Monad.Trans.Except.Extra
  , module Ouroboros.Consensus.Util.Time
  , module Text.Printf
  , module Cardano.Util
  )
where

import Prelude                          (String, error, head, last)
import Cardano.Prelude

#if __GLASGOW_HASKELL__ < 902
-- This is a GHC module ...
import Util
#else
-- that moved for the ghc-9.2 release.
import GHC.Utils.Misc                   as Util
#endif
                                 hiding (fst3, snd3, third3, uncurry3, firstM, secondM)

import Data.Aeson                       (FromJSON (..), ToJSON (..), Object, Value (..), (.:), (.:?), withObject, object)
import Data.Aeson                       qualified as AE
import Data.Tuple.Extra          hiding ((&&&), (***))
import Control.Arrow                    ((&&&), (***))
import Control.Applicative              ((<|>))
import Control.Concurrent.Async         (forConcurrently, forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.DeepSeq                  qualified as DS
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.IntervalMap.FingerTree      (Interval (..), low, high, point)
import Data.List                        (span)
import Data.List.Split                  (chunksOf)
import Data.Text                        qualified as T
import Data.SOP.Strict
import Data.Time.Clock                  (NominalDiffTime, UTCTime (..), diffUTCTime)
import Data.Time.Clock.POSIX
import Data.Vector                      (Vector)
import Data.Vector                      qualified as Vec
import GHC.Base                         (build)
import Text.Printf                      (printf)

import System.FilePath                  qualified as F

import Ouroboros.Consensus.Util.Time

import Cardano.Ledger.BaseTypes         (StrictMaybe (..), fromSMaybe)


-- * Data.IntervalMap.FingerTree.Interval
--
deriving instance FromJSON a => (FromJSON (Interval a))
deriving instance                 Functor  Interval
deriving instance   ToJSON a =>   (ToJSON (Interval a))
deriving instance   NFData a =>   (NFData (Interval a))

unionIntv, intersectIntv :: Ord a => [Interval a] -> Interval a
unionIntv     xs = Interval (low lo) (high hi)
  where lo = minimumBy (compare `on` low)  xs
        hi = maximumBy (compare `on` high) xs
intersectIntv xs = Interval (low lo) (high hi)
  where lo = maximumBy (compare `on` low)  xs
        hi = minimumBy (compare `on` high) xs

renderIntv :: (a -> Text) -> Interval a -> Text
renderIntv f (Interval lo hi) = f lo <> "-" <> f hi

intvDurationSec :: Interval UTCTime -> NominalDiffTime
intvDurationSec = uncurry diffUTCTime . (high &&& low)

-- * SMaybe
--
type SMaybe a = StrictMaybe a

smaybe :: b -> (a -> b) -> StrictMaybe a -> b
smaybe x _  SNothing = x
smaybe _ f (SJust x) = f x

isSJust :: SMaybe a -> Bool
isSJust = \case
  SNothing -> False
  SJust{}  -> True

isSNothing :: SMaybe a -> Bool
isSNothing = \case
  SNothing -> True
  SJust{}  -> False

{-# INLINE strictMaybe #-}
strictMaybe :: Maybe a -> SMaybe a
strictMaybe = \case
  Nothing -> SNothing
  Just a  -> SJust a

{-# INLINE lazySMaybe #-}
lazySMaybe :: SMaybe a -> Maybe a
lazySMaybe = \case
  SNothing -> Nothing
  SJust a  -> Just a

catSMaybes :: [SMaybe a] -> [a]
catSMaybes xs = [x | SJust x <- xs]

mapSMaybe          :: (a -> StrictMaybe b) -> [a] -> [b]
mapSMaybe _ []     = []
mapSMaybe f (x:xs) =
 let rs = mapSMaybe f xs in
 case f x of
  SNothing -> rs
  SJust r  -> r:rs
{-# NOINLINE [1] mapSMaybe #-}

{-# RULES
"mapSMaybe"     [~1] forall f xs. mapSMaybe f xs
                     = build (\c n -> foldr (mapSMaybeFB c f) n xs)
  #-}

{-# INLINE [0] mapSMaybeFB #-} -- See Note [Inline FB functions] in GHC.List
mapSMaybeFB :: (b -> r -> r) -> (a -> StrictMaybe b) -> a -> r -> r
mapSMaybeFB cons f x next = case f x of
  SNothing -> next
  SJust r -> cons r next

mapConcurrentlyPure :: NFData b => (a -> b) -> [a] -> IO [b]
mapConcurrentlyPure f =
  mapConcurrently
    (evaluate . DS.force . f)

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs) = f x:xs
mapHead _ [] = error "mapHead: partial"

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = error "mapHead: partial"
mapLast f xs' = reverse $ go [] xs'
 where go acc = \case
                   x:[] ->     f x:acc
                   x:xs -> go (  x:acc) xs

redistribute :: (a, (b, c)) -> ((a, b), (a, c))
redistribute    (a, (b, c))  = ((a, b), (a, c))

nChunksEachOf :: Int -> Int -> Text -> [Text]
nChunksEachOf chunks each center =
  T.chunksOf each (T.center (each * chunks) ' ' center)

toDouble :: forall a. Real a => a -> Double
toDouble = fromRational . toRational

data F
  = R String
  | Q String
  | L [String]
  | forall a. ToJSON a => J a

progress :: MonadIO m => String -> F -> m ()
progress key = putStr . T.pack . \case
  R x  -> printf "{ \"%s\":  %s }\n"    key x
  Q x  -> printf "{ \"%s\": \"%s\" }\n" key x
  L xs -> printf "{ \"%s\": \"%s\" }\n" key (Cardano.Prelude.intercalate "\", \"" xs)
  J x  -> printf "{ \"%s\": %s }\n" key (LBS.unpack $ AE.encode x)

-- Dumping to files
--
replaceExtension :: FilePath -> String -> FilePath
replaceExtension f new = F.dropExtension f <> "." <> new


spans :: forall a. (a -> Bool) -> [a] -> [Vector a]
spans f = go []
 where
   go :: [Vector a] -> [a] -> [Vector a]
   go acc [] = reverse acc
   go acc xs =
     case span f $ dropWhile (not . f) xs of
       ([], rest) -> go acc rest
       (ac, rest) ->
         go (Vec.fromList ac:acc) rest

{-# INLINE norm2Tuple #-}
norm2Tuple :: ((a, b), c) -> (a, (b, c))
norm2Tuple ((a, b), c) = (a, (b, c))

{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = T.pack . show

roundUTCTimeSec, roundUTCTimeDay :: UTCTime -> UTCTime
roundUTCTimeSec =
  posixSecondsToUTCTime . fromIntegral @Integer . truncate . utcTimeToPOSIXSeconds
roundUTCTimeDay (UTCTime day _) = UTCTime day 0

utcTimeDeltaSec :: UTCTime -> UTCTime -> Int
utcTimeDeltaSec x y = diffUTCTime x y & round

foldEmpty :: r -> ([a] -> r) -> [a] -> r
foldEmpty r _ [] = r
foldEmpty _ f l = f l
