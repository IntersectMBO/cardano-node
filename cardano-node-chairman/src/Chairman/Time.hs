{-# LANGUAGE TypeApplications #-}

module Chairman.Time
  ( showUTCTimeSeconds
  ) where

import           Data.Int
import           Data.String
import           Data.Time.Clock (UTCTime)
import           Prelude (floor)
import           Text.Show

import qualified Data.Time.Clock.POSIX as DTC

-- | Show 'UTCTime' in seconds since epoch
showUTCTimeSeconds :: UTCTime -> String
showUTCTimeSeconds time = show @Int64 (floor (DTC.utcTimeToPOSIXSeconds time))
