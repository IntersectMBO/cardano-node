{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Single source of truth for monotonic time across the pull-fiction library.
--
-- Every module in the package must obtain timestamps through this module rather
-- than importing @System.Clock@ directly. This guarantees that all call sites
-- use the same clock ('Clock.MonotonicRaw') and prevents hard-to-diagnose bugs
-- caused by accidentally mixing different clocks (e.g. 'Clock.Monotonic' vs 
-- 'Clock.MonotonicRaw'), which can produce negative deltas or phantom drift on
-- systems where NTP adjusts the non-raw monotonic source.
--
-- 'TimeSpec' is a @newtype@ over 'Clock.TimeSpec' so that code importing
-- @System.Clock@ directly cannot accidentally pass its timestamps to functions
-- expecting this module's 'TimeSpec', and vice versa.

module Cardano.Benchmarking.PullFiction.Clock
  ( -- * Types.
    TimeSpec
    -- * Reading the clock.
  , getTime
    -- * Conversions.
  , toNanoSecs
  , fromNanoSecs
  ) where

--------------------------------------------------------------------------------

-----------
-- clock --
-----------
import System.Clock qualified as Clock

--------------------------------------------------------------------------------

-- | Opaque monotonic timestamp.
--
-- A @newtype@ wrapper that ensures only timestamps obtained via 'getTime'
-- (which always reads 'Clock.MonotonicRaw') are used in the core library.
--
-- Internally a 'Clock.TimeSpec' stores two fields: @sec@ (seconds) and
-- @nsec@ (nanoseconds within the current second, 0–999 999 999). The derived
-- 'Num' instance normalizes after every operation: carries and borrows
-- between @sec@ and @nsec@ are handled automatically, so @timeA - timeB@
-- always produces a correctly normalized result even when the nanoseconds
-- component underflows.
newtype TimeSpec = TimeSpec Clock.TimeSpec
  deriving (Eq, Ord, Show, Num)

-- | Read the monotonic raw clock. All timing in the package goes through this
-- function so a single clock source is used everywhere.
getTime :: IO TimeSpec
getTime = TimeSpec <$> Clock.getTime Clock.MonotonicRaw

-- | Convert a 'TimeSpec' to __total__ nanoseconds.
--
-- Returns @sec * 1 000 000 000 + nsec@, not just the @nsec@ field.
-- For example, @TimeSpec 2 500000000@ (2.5 s) yields @2 500 000 000@.
toNanoSecs :: TimeSpec -> Integer
toNanoSecs (TimeSpec ts) = Clock.toNanoSecs ts

-- | Convert total nanoseconds to a 'TimeSpec'.
--
-- Splits via @divMod@ into @sec@ and @nsec@ so the result is always
-- normalized (e.g. @fromNanoSecs 2500000000@ gives @TimeSpec 2 500000000@).
fromNanoSecs :: Integer -> TimeSpec
fromNanoSecs = TimeSpec . Clock.fromNanoSecs
