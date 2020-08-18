module Chairman.Network
  ( isPortOpen
  , listenOn
  ) where

import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Foreign.C.Error (Errno (..))
import           Network.Socket (Family (AF_INET), SockAddr (..), Socket, SocketType (Stream))
import           System.IO (IO)
import           Text.Show (show)

import qualified Data.List as L
import qualified Foreign.C.Error as E
import qualified GHC.IO.Exception as IO
import qualified Network.Socket as IO
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
      | matchingErrNo (Errno <$> IO.ioe_errno e) -> return False
      | "WSAECONNREFUSED" `L.isInfixOf` show e -> return False
      | "WSAECONNRESET" `L.isInfixOf` show e -> return False
      | otherwise -> IO.throwIO e
  where matchingErrNo :: Maybe Errno -> Bool
        matchingErrNo = maybe False (`L.elem` [E.eCONNRESET, E.eCONNREFUSED])

-- | Open a socket at the specified port for listening
listenOn :: (MonadIO m, MonadFail m) => Int -> m Socket
listenOn n = do
  sock <- liftIO $ IO.socket AF_INET Stream 0
  sockAddrInfo:_ <- liftIO $ IO.getAddrInfo Nothing (Just "127.0.0.1") (Just (show n))
  liftIO $ IO.setSocketOption sock IO.ReuseAddr 1
  liftIO $ IO.bind sock (IO.addrAddress sockAddrInfo)
  liftIO $ IO.listen sock 2
  return sock
