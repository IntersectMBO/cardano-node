{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.Filter
  (
    Filter (..)

  , Id (..)

  -- Trace message validation.
  , ParseTrace (..)
  , RightTrace (..)
  , RightAt (..)

  , Namespace (..)
  -- TODO: Ideas!
--  , RemoveFirstNonTraces (..)
--  , AscendingAt

  -- Get a single data point with a timestamp.
  , Resource (..)
  , UtxoSize (..)

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

-- TODO: Show should not be here
class Show f => Filter f where
  type family FilterInput f :: Type
  type family FilterOutput f :: Type
  filterOf :: f -> FilterInput f -> Maybe (FilterOutput f)

--------------------------------------------------------------------------------

data Id = Id
  deriving Show

-- From a `Text` line to an `Either` `Trace`.
data ParseTrace = ParseTrace
  deriving Show

-- From an `Either` `Trace` to a `Trace`.
data RightTrace = RightTrace
  deriving Show

-- From `Trace` with an `Either` `UTCTime` to a `(UTCTime, Text)`.
data RightAt = RightAt
  deriving Show

-- Filter valid `Trace`s by namespace (`ns`).
data Namespace = Namespace Text.Text
  deriving Show

-- Get a `Resource` property (they are all `Integer`) from a `Trace`.
data Resource = Resource (Trace.DataResources -> Integer)

instance Show Resource where
  show _ = "Resource"

-- Get a `remainder`'s "utxoSize" property from a `Trace`.
data UtxoSize = UtxoSize
  deriving Show

--------------------------------------------------------------------------------

instance Filter Id where
  type instance FilterInput  Id = Text.Text
  type instance FilterOutput Id = Text.Text
  filterOf _ = Just

instance Filter ParseTrace where
  type instance FilterInput  ParseTrace = Text.Text
  type instance FilterOutput ParseTrace = Either Text.Text Trace.Trace
  filterOf _ text = Just $ Trace.fromJson text

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
-- For performance, first the `Namespace` and second the `RightAt` filter.
instance Filter Resource where
  type instance FilterInput  Resource = (UTCTime, Text.Text)
  type instance FilterOutput Resource = (UTCTime, Integer)
  filterOf (Resource f) (at, remainder) =
    case Aeson.eitherDecodeStrictText remainder of
      (Right !aeson) ->
        -- TODO: Use `unsnoc` when available
        let resource = f $ Trace.remainderData aeson
        in Just (at, resource)
      (Left _) -> Nothing

-- To use after `RightTrace`.
-- For performance, first the `Namespace` and second the `RightAt` filter.
instance Filter UtxoSize where
  type instance FilterInput  UtxoSize = (UTCTime, Text.Text)
  type instance FilterOutput UtxoSize = (UTCTime, Integer)
  filterOf UtxoSize (at, remainder) =
    case Aeson.eitherDecodeStrictText remainder of
      (Right !aeson) ->
        -- TODO: Use `unsnoc` when available
        let utxoSize = Trace.utxoSize $ Trace.remainderData aeson
        in Just (at, utxoSize)
      (Left _) -> Nothing
