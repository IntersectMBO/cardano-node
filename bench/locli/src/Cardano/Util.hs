{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Util
  ( module Prelude
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

import Prelude                          (String, error)
import Cardano.Prelude

import Data.Tuple.Extra          hiding ((&&&), (***))
import Control.Arrow                    ((&&&), (***))
import Control.Applicative              ((<|>))
import Control.Concurrent.Async         (forConcurrently, forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.DeepSeq                  qualified as DS
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.Aeson                       (ToJSON, encode)
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.List                        (span)
import Data.Text                        qualified as T
import Data.Vector                      (Vector)
import Data.Vector                      qualified as Vec
import GHC.Base                         (build)
import Text.Printf                      (printf)

import System.FilePath                  qualified as F

import Ouroboros.Consensus.Util.Time

import Cardano.Ledger.BaseTypes         (StrictMaybe (..), fromSMaybe)


type SMaybe a = StrictMaybe a

instance Alternative StrictMaybe where
  empty = SNothing
  (<|>) x y = case x of
                SNothing -> y
                _ -> x

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

mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])
mapAndUnzip _ [] = ([], [])
mapAndUnzip f (x:xs)
  = let (r1,  r2)  = f x
        (rs1, rs2) = mapAndUnzip f xs
    in
    (r1:rs1, r2:rs2)

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs) = f x:xs
mapHead _ [] = error "mapHead: partial"

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
  J x  -> printf "{ \"%s\": %s }\n" key (LBS.unpack $ encode x)

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
