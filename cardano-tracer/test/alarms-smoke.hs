-- | Cabal wrapper for the alarm-subsystem smoke test. Locates
-- @test/alarms-smoke.sh@ via 'getDataFileName' and runs it under @bash@;
-- non-zero exit propagates through 'callProcess' as an exception, which
-- the test framework reports as a test failure.
module Main where

import           Paths_cardano_tracer (getDataFileName)
import           System.Process (callProcess)

main :: IO ()
main = do
  script <- getDataFileName "test/alarms-smoke.sh"
  callProcess "bash" [script]
