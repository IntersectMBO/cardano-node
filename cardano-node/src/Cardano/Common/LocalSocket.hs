module Cardano.Common.LocalSocket
  ( localSocketAddrInfo
  , removeStaleLocalSocket
  )
where

import           Cardano.Prelude

import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath (takeDirectory)
import           System.IO.Error (isDoesNotExistError)
import           Network.Socket as Socket

import           Cardano.Config.Types (SocketPath(..))


-- | Provide an AF_UNIX address for a socket situated in 'socketDir', with its name
--   derived from the node ID.  When 'mkdir' is 'MkdirIfMissing', the directory is created.
localSocketAddrInfo :: SocketPath -> IO Socket.AddrInfo
localSocketAddrInfo (SocketFile fp) = do
  createDirectoryIfMissing True $ takeDirectory fp
  pure $
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix fp)
      Nothing

-- TODO: Convert to ExceptT
-- | Remove the socket established with 'localSocketAddrInfo'.
removeStaleLocalSocket :: SocketPath -> IO ()
removeStaleLocalSocket (SocketFile socketFp) = do
  removeFile socketFp
    `catch` \e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e
