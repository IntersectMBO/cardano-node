{-# LANGUAGE CPP #-}

module Chairman.IO.Constants
  ( isWin32
  ) where

import           Data.Bool

isWin32 :: Bool
isWin32 =
#ifdef mingw32_HOST_OS
  True
#else
  False
#endif
{-# INLINE isWin32 #-}
