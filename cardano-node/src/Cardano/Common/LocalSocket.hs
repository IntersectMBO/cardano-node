module Cardano.Common.LocalSocket
  ( MkdirIfMissing(..)
  , localSocketAddrInfo
  , removeStaleLocalSocket
  )
where

import           Cardano.Prelude

import           System.Directory ( canonicalizePath, createDirectoryIfMissing
                                  , makeAbsolute, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)
import           Network.Socket as Socket

import           Ouroboros.Consensus.NodeId (NodeId(..))

data MkdirIfMissing
  = MkdirIfMissing
  | NoMkdirIfMissing
  deriving (Eq, Show)

localSocketFilePath :: NodeId -> FilePath
localSocketFilePath (CoreId  n) = "node-core-" ++ show n ++ ".socket"
localSocketFilePath (RelayId n) = "node-relay-" ++ show n ++ ".socket"

-- | Provide an AF_UNIX address for a socket situated in 'socketDir', with its name
--   derived from the node ID.  When 'mkdir' is 'MkdirIfMissing', the directory is created.
localSocketAddrInfo :: NodeId -> FilePath -> MkdirIfMissing -> IO Socket.AddrInfo
localSocketAddrInfo nodeId socketDir mkdir = do
  dir <- canonicalizePath =<< makeAbsolute socketDir
  when (mkdir == MkdirIfMissing)
    $ createDirectoryIfMissing True dir
  pure $
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix $ dir </> localSocketFilePath nodeId)
      Nothing

-- | Remove the socket established with 'localSocketAddrInfo'.
removeStaleLocalSocket :: NodeId -> FilePath -> IO ()
removeStaleLocalSocket nodeId socketDir = do
  dir <- canonicalizePath =<< makeAbsolute socketDir
  removeFile (dir </> localSocketFilePath nodeId)
    `catch` \e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e
