{-# OPTIONS_GHC -Wno-orphans #-}

module Testnet.Orphans () where

import           RIO (RIO(..), liftIO)

instance MonadFail (RIO env) where 
  fail = liftIO . fail