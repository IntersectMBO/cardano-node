{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Common.LocalSocket
  ( chooseSocketPath
  , nodeLocalSocketAddrInfo
  , removeStaleLocalSocket
  )
where

import           Prelude (show)
import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (newExceptT)
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath (takeDirectory)
import           System.IO.Error (isDoesNotExistError)

import           Cardano.Config.Types

-- | Errors for the current module.
data SocketError
    = SocketError !FilePath !IOException

-- | Instance for showing the @SocketError@.
instance Show SocketError where
    show (SocketError fp ex) =
      "Socket '" <> fp <> "': " <> Prelude.show ex

-- | This lets us override the socket path specified in the node configuration yaml file
-- if required.
chooseSocketPath :: Maybe YamlSocketPath -> Maybe CLISocketPath -> SocketPath
chooseSocketPath Nothing Nothing = panic $ "Cardano.Common.LocalSocket.chooseSocketPath: "
                                         <> "Please specify a socket path either in the config yaml "
                                         <> "file or on the command line."
chooseSocketPath (Just yamlSockPath) Nothing = unYamlSocketPath yamlSockPath
chooseSocketPath Nothing (Just cliSockPath) = unCLISocketPath cliSockPath
chooseSocketPath _ (Just cliSockPath) = unCLISocketPath cliSockPath

nodeLocalSocketAddrInfo :: NodeConfiguration -> NodeCLI -> FilePath
nodeLocalSocketAddrInfo nc NodeCLI {socketFile} =
    path
  where
    SocketFile path = chooseSocketPath (ncSocketPath nc) socketFile

-- | Remove the socket established with 'localSocketAddrInfo'.
removeStaleLocalSocket :: NodeConfiguration -> NodeCLI -> ExceptT SocketError IO ()
removeStaleLocalSocket nc NodeCLI{socketFile} = do
    SocketFile socketFp <- pure $ chooseSocketPath (ncSocketPath nc) socketFile

    -- Removal of the socket file may fail if it has already been cleaned up.
    newExceptT $ (Right <$> removeFile socketFp) `catch` handler socketFp
  where
    handler :: FilePath -> IOException -> IO (Either SocketError ())
    handler fpath ex
      | isDoesNotExistError ex = pure $ Right ()
      | otherwise = pure $ Left (SocketError fpath ex)
