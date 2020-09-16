module Hedgehog.Extras.Stock.OS
  ( isWin32
  ) where

import           Data.Bool
import           Data.Eq
import           System.Info

-- | Determine if the operating system is Windows.
isWin32 :: Bool
isWin32 = os == "mingw32"
{-# INLINE isWin32 #-}
