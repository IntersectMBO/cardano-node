{-# OPTIONS_GHC -Wno-orphans #-}

module Testnet.Orphans () where

import           RIO (RIO (..), throwString)

instance MonadFail (RIO env) where
  fail = throwString
