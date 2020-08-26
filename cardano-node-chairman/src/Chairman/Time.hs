module Chairman.Time
  ( formatIso8601
  ) where

import           Data.Maybe
import           Data.String
import           Data.Time (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

formatIso8601 :: UTCTime -> String
formatIso8601 = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
