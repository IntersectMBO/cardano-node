module Cardano.Tracer.Time(getTimeMs) where
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (getPOSIXTime)

-- forkServer definition of `getTimeMs'. The ekg frontend relies
-- on the "ekg.server_timestamp_ms" metric being in every
-- store. While forkServer adds that that automatically we must
-- manually add it.
-- url
--  + https://github.com/tvh/ekg-wai/blob/master/System/Remote/Monitoring/Wai.hs#L237-L238
getTimeMs :: IO Int64
getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime
