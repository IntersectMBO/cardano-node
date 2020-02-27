module Cardano.Common.LocalSocket
  ( localSocketPath
  , nodeLocalSocketAddrInfo
  , removeStaleLocalSocket
  )
where

import           Cardano.Prelude

import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath (takeDirectory)
import           System.IO.Error (isDoesNotExistError)

import           Cardano.Config.Types ( MiscellaneousFilepaths(..), NodeCLI(..)
                                      , NodeMockCLI(..), NodeProtocolMode(..)
                                      , SocketPath(..))


nodeLocalSocketAddrInfo :: NodeProtocolMode -> IO FilePath
nodeLocalSocketAddrInfo npm =
  case npm of
    MockProtocolMode (NodeMockCLI mscFp' _ _ _ _) -> localSocketPath $ socketFile mscFp'
    RealProtocolMode (NodeCLI mscFp' _ _ _ _) -> localSocketPath $ socketFile mscFp'

-- | Provide an filepath intended for a socket situated in 'socketDir'.
-- When 'mkdir' is 'MkdirIfMissing', the directory is created.
localSocketPath :: SocketPath -> IO FilePath
localSocketPath (SocketFile fp) = do
  createDirectoryIfMissing True $ takeDirectory fp
  return fp

-- TODO: Convert to ExceptT
-- | Remove the socket established with 'localSocketAddrInfo'.
removeStaleLocalSocket :: NodeProtocolMode -> IO ()
removeStaleLocalSocket npm = do
  SocketFile socketFp <-
    case npm of
      MockProtocolMode (NodeMockCLI mscFp' _ _ _ _) -> pure $ socketFile mscFp'
      RealProtocolMode (NodeCLI mscFp' _ _ _ _) -> pure $ socketFile mscFp'

  removeFile socketFp
    `catch` \e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e
