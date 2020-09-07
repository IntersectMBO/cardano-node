{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chairman.IO.Network.Socket
  ( doesSocketExist
  , isPortOpen
  , canConnect
  , listenOn
  ) where

import           Control.Exception (IOException, handle)
import           Control.Monad
import           Data.Bool
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Network.Socket (Family (AF_INET), SockAddr (..), Socket, SocketType (Stream))
import           System.IO (FilePath, IO)
import           Text.Show (show)

import qualified Network.Socket as IO
import qualified System.Directory as IO
import qualified UnliftIO.Exception as IO

-- | Check if a TCP port is open
isPortOpen :: Int -> IO Bool
isPortOpen port = do
  socketAddressInfos <- IO.getAddrInfo Nothing (Just "127.0.0.1") (Just (show port))
  case socketAddressInfos of
    socketAddressInfo:_ ->
      handle (return . const @Bool @IOException False) $
        canConnect (IO.addrAddress socketAddressInfo) $> True
    [] -> return False

-- | Check if it is possible to connect to a socket address
canConnect :: SockAddr -> IO ()
canConnect sockAddr = IO.bracket (IO.socket AF_INET Stream 6) IO.close' $ \sock -> do
  IO.connect sock sockAddr

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
