module Chairman.IO.Network.Posix
  ( doesSocketExist
  , adjustSocketPath
  , maxSocketNameLength
  ) where

import           Data.Bool
import           Data.Function
import           Data.Int
import           System.IO (FilePath, IO)

import qualified System.Directory as IO

doesSocketExist :: FilePath -> IO Bool
doesSocketExist = IO.doesFileExist
{-# INLINE doesSocketExist #-}

-- | Adjust the socket name to be Win32 friendly
adjustSocketPath :: FilePath -> FilePath
adjustSocketPath = id

maxSocketNameLength :: Int
maxSocketNameLength = 104
