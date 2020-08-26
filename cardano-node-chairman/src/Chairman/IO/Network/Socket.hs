{-# LANGUAGE CPP #-}

{-|
Module      : Chairman.IO.Network.Socket
Description : Network functions

OS specific functions are found in submodules.
-}
module Chairman.IO.Network.Socket
  ( doesSocketExist
  , isPortOpen
  , canConnect
  , listenOn
  ) where

import           Control.Monad
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Foreign.C.Error (Errno (..), eCONNREFUSED)
import           Network.Socket (Family (AF_INET), SockAddr (..), Socket, SocketType (Stream))
import           System.IO (FilePath, IO)
import           Text.Show (show)

import qualified Data.List as L
import qualified GHC.IO.Exception as IO
import qualified Network.Socket as IO
import qualified System.Directory as IO
import qualified UnliftIO.Exception as IO

-- | Check if a TCP port is open
isPortOpen :: Int -> IO Bool
isPortOpen port = do
  socketAddressInfo:_ <- IO.getAddrInfo Nothing (Just "127.0.0.1") (Just (show port))
  canConnect (IO.addrAddress socketAddressInfo)

-- | Check if it is possible to connect to a socket address
canConnect :: SockAddr -> IO Bool
canConnect sockAddr = IO.bracket (IO.socket AF_INET Stream 6) IO.close' $ \sock -> do
  result <- IO.try $ IO.connect sock sockAddr
  case result of
    Right () -> return True
    Left e
      | (Errno <$> IO.ioe_errno e) == Just eCONNREFUSED -> return False
      | "WSAECONNREFUSED" `L.isInfixOf` show e -> return False
      | "WSAECONNRESET" `L.isInfixOf` show e -> return False
      | otherwise -> IO.throwIO e

-- | Open a socket at the specified port for listening
listenOn :: Int -> IO Socket
listenOn n = do
  sock <- IO.socket AF_INET Stream 0
  sockAddrInfo:_ <- IO.getAddrInfo Nothing (Just "127.0.0.1") (Just (show n))
  IO.setSocketOption sock IO.ReuseAddr 1
  IO.bind sock (IO.addrAddress sockAddrInfo)
  IO.listen sock 2
  return sock

doesSocketExist :: FilePath -> IO Bool
doesSocketExist = IO.doesFileExist
{-# INLINE doesSocketExist #-}
