{-# LANGUAGE CPP #-}

module Chairman.IO.Network
  ( doesSocketExist
  , maxSocketNameLength
  , adjustSocketPath
  , isPortOpen
  , canConnect
  , listenOn
  ) where

import           Data.Bool
import           Data.Int
import           Network.Socket (SockAddr, Socket)
import           System.IO (FilePath, IO)

#ifdef mingw32_HOST_OS
import qualified Chairman.IO.Network.Win32 as OS
#else
import qualified Chairman.IO.Network.Posix as OS
#endif

import qualified Chairman.IO.Network.Base as OS

doesSocketExist :: FilePath -> IO Bool
doesSocketExist = OS.doesSocketExist

maxSocketNameLength :: Int
maxSocketNameLength = OS.maxSocketNameLength

adjustSocketPath :: FilePath -> FilePath
adjustSocketPath = OS.adjustSocketPath

-- | Check if a TCP port is open
isPortOpen :: Int -> IO Bool
isPortOpen = OS.isPortOpen

-- | Check if it is possible to connect to a socket address
canConnect :: SockAddr -> IO Bool
canConnect = OS.canConnect

-- | Open a socket at the specified port for listening
listenOn :: Int -> IO Socket
listenOn = OS.listenOn
