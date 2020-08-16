{-# LANGUAGE CPP #-}

module Cardano.Node.Platform.Constants
  ( isPosix
  ) where

import           Data.Bool

isPosix :: Bool
isPosix =
#if !defined(mingw32_HOST_OS)
  True
#else
  False
#endif
{-# INLINE isPosix #-}
