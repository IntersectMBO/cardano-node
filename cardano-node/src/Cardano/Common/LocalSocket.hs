module Cardano.Common.LocalSocket
  ( eitherThrow
  , localSocketFilePath
  , localSocketAddrInfo
  )
where

import           Cardano.Prelude

import           Network.Socket as Socket
import           Ouroboros.Consensus.NodeId


localSocketFilePath :: NodeId -> FilePath
localSocketFilePath (CoreId  n) = "node-core-" ++ show n ++ ".socket"
localSocketFilePath (RelayId n) = "node-relay-" ++ show n ++ ".socket"

localSocketAddrInfo :: FilePath -> Socket.AddrInfo
localSocketAddrInfo socketPath =
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix socketPath)
      Nothing

eitherThrow :: Exception e => (b -> e) -> Either b a ->  IO a
eitherThrow masker (Left e) = throwIO $ masker e
eitherThrow _ (Right x) = pure x
