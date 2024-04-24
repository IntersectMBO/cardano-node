{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.Filter
  (
    Filter (..)

  , Id (..), Compose (..), (<->), (<.>)

  -- Trace message validation.
  , ParseTrace (..), RightTrace (..), RightAt (..)

  -- Filter by `Namespace` and non-error decoded remainder.
  , Namespace (..), Aeson (..), AesonWithAt (..)
  -- TODO: Ideas!
--  , RemoveFirstNonTraces (..)
--  , AscendingAt

  ) where

--------------------------------------------------------------------------------

-- base.
import           Data.Kind (Type)
-- package: time.
import           Data.Time.Clock (UTCTime)
-- package: text.
import qualified Data.Text as Text
-- package: aeson.
import qualified Data.Aeson as Aeson
-- library.
import qualified Cardano.Tracer.Trace as Trace

--------------------------------------------------------------------------------

class Filter f where
  type family FilterInput  f :: Type
  type family FilterOutput f :: Type
  filterOf :: f -> FilterInput f -> Maybe (FilterOutput f)

-- Identity and composition.
--------------------------------------------------------------------------------

data Id = Id
  deriving Show

instance Filter Id where
  type instance FilterInput  Id = Text.Text
  type instance FilterOutput Id = Text.Text
  filterOf _ = Just

data Compose f1 f2 = Compose f1 f2

instance (Show f1, Show f2) => Show (Compose f1 f2) where
  show (Compose f1 f2) = "(" ++ show f1 ++ ") <-> (" ++ show f2 ++ ")"

instance ( Filter f1
         , Filter f2
         , FilterOutput f1 ~ FilterInput f2
         )
      => Filter (Compose f1 f2) where
  type instance FilterInput  (Compose f1 f2) = FilterInput  f1
  type instance FilterOutput (Compose f1 f2) = FilterOutput f2
  filterOf (Compose f1 f2) inputOfF1 =
    filterOf f1 inputOfF1 >>= filterOf f2

(<->) :: f1 -> f2 -> Compose f1 f2
f1 <-> f2 = Compose f1 f2

-- The same as `<->` but flipped, like function composition `(.)`.
(<.>) :: f1 -> f2 -> Compose f2 f1
f1 <.> f2 = Compose f2 f1

-- TODO: FIXME: This one is not a "filter", is a "map" function
--------------------------------------------------------------------------------

-- From a `Text` line to an `Either` `Trace`.
data ParseTrace = ParseTrace
  deriving Show

instance Filter ParseTrace where
  type instance FilterInput  ParseTrace = Text.Text
  type instance FilterOutput ParseTrace = Either Text.Text Trace.Trace
  filterOf _ text = Just $ Trace.fromJson text

-- Builtin trace messages filters.
--------------------------------------------------------------------------------

-- From an `Either` `Trace` to a `Trace`.
data RightTrace = RightTrace
  deriving Show

-- From `Trace` with an `Either` `UTCTime` to a `(UTCTime, Text)`.
data RightAt = RightAt
  deriving Show

-- Filter valid `Trace`s by namespace (`ns`).
newtype Namespace = Namespace Text.Text
  deriving Show

-- Convert the remainder.
data Aeson t = Aeson
  deriving Show

-- TODO: FIXME
data AesonWithAt t = AesonWithAt
  deriving Show

--------------------------------------------------------------------------------

-- To use after `ParseTrace`.
instance Filter RightTrace where
  type instance FilterInput  RightTrace = Either Text.Text Trace.Trace
  type instance FilterOutput RightTrace = Trace.Trace
  filterOf _ (Left _) = Nothing
  filterOf _ (Right trace) = Just trace

-- To use after `RightTrace`.
instance Filter RightAt where
  type instance FilterInput  RightAt = Trace.Trace
  type instance FilterOutput RightAt = (UTCTime, Text.Text) -- at and remainder
  filterOf RightAt (Trace.Trace eitherAt _ remainder) =
    case eitherAt of
      (Left _) -> Nothing
      (Right at) -> Just (at, remainder)

-- To use after `RightTrace`.
-- The most performant filter, to always use first when possible.
instance Filter Namespace where
  type instance FilterInput  Namespace = Trace.Trace
  type instance FilterOutput Namespace = Trace.Trace
  filterOf (Namespace ns) trace =
    if Trace.ns trace == ns
    then Just trace
    else Nothing

-- To use after `RightTrace`.
-- One of the slowest filter, use after `Namespace` when possible.
instance Aeson.FromJSON t => Filter (Aeson t) where
  type instance FilterInput  (Aeson t) = Trace.Trace
  type instance FilterOutput (Aeson t) = t -- Decoded remainder
  filterOf Aeson (Trace.Trace _ _ remainder) =
    case Aeson.eitherDecodeStrictText remainder of
      (Left _) -> Nothing
      -- TODO: Bench this strictness annotation!
      (Right !aeson) -> Just aeson

-- Slow x 2
instance Aeson.FromJSON t => Filter (AesonWithAt t) where
  type instance FilterInput  (AesonWithAt t) = (UTCTime, Text.Text)
  type instance FilterOutput (AesonWithAt t) = (UTCTime, t) -- Decoded remainder
  filterOf AesonWithAt (at, remainder) =
    case Aeson.eitherDecodeStrictText remainder of
      (Left _) -> Nothing
      -- TODO: Bench this strictness annotation!
      (Right !aeson) -> Just (at, aeson)
