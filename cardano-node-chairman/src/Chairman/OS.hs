module Chairman.OS
  ( isWin32
  ) where

import           Data.Bool
import           Data.Eq
import           System.Info

isWin32 :: Bool
isWin32 = os == "mingw32"
{-# INLINE isWin32 #-}
