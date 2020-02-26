{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Common.LocalSocket
  ( chooseSocketPath
  , localSocketPath
  , nodeLocalSocketAddrInfo
  , removeStaleLocalSocket
  )
where

import           Cardano.Prelude

import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath (takeDirectory)
import           System.IO.Error (isDoesNotExistError)

import           Cardano.Config.Types

-- | This lets us override the socket path specified in the node configuration yaml file
-- if required.
chooseSocketPath :: Maybe YamlSocketPath -> Maybe CLISocketPath -> SocketPath
chooseSocketPath Nothing Nothing = panic $ "Cardano.Common.LocalSocket.chooseSocketPath: "
                                         <> "Please specify a socket path either in the config yaml "
                                         <> "file or on the command line."
chooseSocketPath (Just yamlSockPath) Nothing = unYamlSocketPath yamlSockPath
chooseSocketPath Nothing (Just cliSockPath) = unCLISocketPath cliSockPath
chooseSocketPath _ (Just cliSockPath) = unCLISocketPath cliSockPath

nodeLocalSocketAddrInfo :: NodeConfiguration -> NodeProtocolMode -> IO FilePath
nodeLocalSocketAddrInfo nc npm = do
  mCliSockPath <- case npm of
                    MockProtocolMode (NodeMockCLI {mockMscFp}) -> pure $ socketFile mockMscFp
                    RealProtocolMode (NodeCLI {mscFp}) -> pure $ socketFile mscFp

  localSocketPath $ chooseSocketPath (ncSocketPath nc) mCliSockPath

-- | Provide an filepath intended for a socket situated in 'socketDir'.
-- When 'mkdir' is 'MkdirIfMissing', the directory is created.
localSocketPath :: SocketPath -> IO FilePath
localSocketPath (SocketFile fp) = do
  createDirectoryIfMissing True $ takeDirectory fp
  return fp

-- TODO: Convert to ExceptT
-- | Remove the socket established with 'localSocketAddrInfo'.
removeStaleLocalSocket :: NodeConfiguration -> NodeProtocolMode -> IO ()
removeStaleLocalSocket nc npm = do
  mCliSockPath <- case npm of
                    MockProtocolMode (NodeMockCLI {mockMscFp}) -> pure $ socketFile mockMscFp
                    RealProtocolMode (NodeCLI {mscFp}) -> pure $ socketFile mscFp

  (SocketFile socketFp) <- pure $ chooseSocketPath (ncSocketPath nc) mCliSockPath

  removeFile socketFp
    `catch` \e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e
