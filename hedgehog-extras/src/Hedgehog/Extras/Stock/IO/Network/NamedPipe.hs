{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hedgehog.Extras.Stock.IO.Network.NamedPipe
  ( doesNamedPipeExist
  ) where

import           Data.Bool
import           Prelude (error)
import           System.IO (FilePath, IO)

#ifdef mingw32_HOST_OS
import qualified System.Win32.NamedPipes as W32
#endif

doesNamedPipeExist :: FilePath -> IO Bool
doesNamedPipeExist path =
#ifdef mingw32_HOST_OS
  W32.waitNamedPipe path 1
#else
  error "doesNamedPipeExist may only be called on Windows"
#endif
