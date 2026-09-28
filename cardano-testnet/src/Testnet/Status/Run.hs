module Testnet.Status.Run
  ( runCheckStatusOptions
  ) where

import           Prelude

import           Testnet.Status.Types (CheckStatusOptions)

runCheckStatusOptions :: CheckStatusOptions -> IO ()
runCheckStatusOptions _ = pure ()
