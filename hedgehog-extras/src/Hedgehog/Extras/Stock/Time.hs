{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Stock.Time
  ( showUTCTimeSeconds
  , formatIso8601
  ) where

import           Data.Int
import           Data.Maybe
import           Data.String
import           Data.Time.Clock (UTCTime)
import           Prelude (floor)
import           Text.Show

import qualified Data.Time.Clock.POSIX as DTC
import qualified Data.Time.Format as DT

-- | Show 'UTCTime' in seconds since epoch
showUTCTimeSeconds :: UTCTime -> String
showUTCTimeSeconds time = show @Int64 (floor (DTC.utcTimeToPOSIXSeconds time))

-- | Format the given time as an ISO 8601 date-time string
formatIso8601 :: UTCTime -> String
formatIso8601 = DT.formatTime DT.defaultTimeLocale (DT.iso8601DateFormat (Just "%H:%M:%SZ"))
