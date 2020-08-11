{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Configuration.Socket
  ( gatherConfiguredSockets
  , SocketOrSocketInfo(..)
  , SocketConfigError(..)
  , renderSocketConfigError
  )
where

import           Cardano.Prelude hiding (local)
import           Prelude (String)
import qualified Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Socket, SocketType (..),
                     defaultHints, getAddrInfo)

import           Cardano.Node.Types

#if defined(mingw32_HOST_OS)
#else
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)
#endif

#ifdef SYSTEMD
import           System.Systemd.Daemon (getActivatedSockets)
#endif




-- | Since we support systemd socket activation, we have to handle being
-- given actual already-constructed sockets, or the info needed to make new
-- sockets later.
--
data SocketOrSocketInfo socket info =
       ActualSocket socket
     | SocketInfo   info

-- | Errors for the current module.
data SocketConfigError
    = NoPublicSocketGiven
    | NoLocalSocketGiven
    | ClashingPublicSocketGiven
    | ClashingLocalSocketGiven
    | LocalSocketError FilePath IOException
    | GetAddrInfoError NodeAddress IOException
  deriving Show

instance Exception SocketConfigError where
  displayException = renderSocketConfigError

renderSocketConfigError :: SocketConfigError -> String
renderSocketConfigError NoPublicSocketGiven =
    "No configuration for the node's public socket. Please specify a socket "
 <> "path either in the config file, on the command line or via systemd socket "
 <> "activation."

renderSocketConfigError NoLocalSocketGiven =
    "No configuration for the node's local socket. Please specify a socket "
 <> "path either in the config file, on the command line or via systemd socket "
 <> "activation."

renderSocketConfigError ClashingPublicSocketGiven =
    "Configuration for the node's public socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError ClashingLocalSocketGiven =
    "Configuration for the node's local socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError (LocalSocketError fp ex) =
    "Failure while attempting to remove the stale local socket: "
 <> fp <> " : " <> displayException ex

renderSocketConfigError (GetAddrInfoError addr ex) =
    "Failure while getting address information for the public listening "
 <> "address: " <> show addr <> " : " <> displayException ex


-- | Gather from the various sources of configuration which sockets we will use
-- for the public node-to-node and the local node-to-client IPC.
--
-- We get such configuration from:
--
-- * node config file
-- * node cli
-- * systemd socket activation
--
gatherConfiguredSockets :: NodeConfiguration
                        -> NodeCLI
                        -> ExceptT SocketConfigError IO
                                   (SocketOrSocketInfo [Socket] [AddrInfo],
                                    SocketOrSocketInfo Socket SocketPath)
gatherConfiguredSockets config cli = do

    mbAllSocketsFromSystemD          <- liftIO getSystemdSockets

    -- Select the sockets or address for public node-to-node comms
    --
    let mbPublicSocketsAddrFromConfigOrCLI :: Maybe NodeAddress
        mbPublicSocketsAddrFromConfigOrCLI = nodeAddr cli
                                             --TODO: add config file support
        mbPublicSocketsFromSystemD         = snd <$> mbAllSocketsFromSystemD

    public <- case (mbPublicSocketsAddrFromConfigOrCLI,
                    mbPublicSocketsFromSystemD) of
                (Nothing, Just [])    -> throwError NoPublicSocketGiven
                (Nothing, Just socks) -> return (ActualSocket socks)
                (Just addr, Nothing)  -> SocketInfo <$> nodeAddressInfo addr
                (Just addr, Just [])  -> SocketInfo <$> nodeAddressInfo addr
                (Nothing, Nothing)    -> throwError NoPublicSocketGiven
                (Just{}, Just{})      -> throwError ClashingPublicSocketGiven

    -- Select the socket or path for local node-to-client comms
    --
    let mbLocalSocketFileConfigOrCLI  = socketFile cli `mplus`
                                        ncSocketPath config
        mbLocalSocketFromSystemD      = fst <$> mbAllSocketsFromSystemD

    local  <- case (mbLocalSocketFileConfigOrCLI,
                    mbLocalSocketFromSystemD) of
                (Nothing, Just sock) -> return (ActualSocket sock)
                (Just path, Nothing) -> do removeStaleLocalSocket path
                                           return (SocketInfo path)
                (Nothing, Nothing)   -> throwError NoLocalSocketGiven
                (Just{}, Just{})     -> throwError ClashingLocalSocketGiven

    return (public, local)


-- | Binding a local unix domain socket always expects to create it, and fails
-- if it exists already. So we delete it first if it exists. But only on unix.
--
removeStaleLocalSocket :: SocketPath -> ExceptT SocketConfigError IO ()
#if defined(mingw32_HOST_OS)
removeStaleLocalSocket _ =
    return ()
#else
removeStaleLocalSocket (SocketPath path) =
    handleIOExceptT (LocalSocketError path) $
      removeFile path `catch` \e ->
        if isDoesNotExistError e then return ()
                                 else throwIO e
#endif

nodeAddressInfo :: NodeAddress -> ExceptT SocketConfigError IO [AddrInfo]
nodeAddressInfo addr@(NodeAddress hostAddr port) =
    handleIOExceptT (GetAddrInfoError addr) $
      getAddrInfo (Just hints)
                  (fmap Prelude.show $ unNodeHostAddress hostAddr)
                  (Just $ Prelude.show port)
  where
    hints = defaultHints {
              addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
            , addrSocketType = Stream
            }


-- | Possibly return a SOCKET_UNIX socket for NodeToClient communication
-- and a list of SOCKET_STREAM sockets for NodeToNode communication.
-- The SOCKET_UNIX socket should be defined first in the `.socket` systemd file.
--
getSystemdSockets :: IO (Maybe (Socket, [Socket]))
#ifdef SYSTEMD
getSystemdSockets = do
  sds_m <- getActivatedSockets
  case sds_m of
       Nothing       -> return Nothing
       Just []       -> return Nothing
       Just (sd:sds) -> return $ Just (sd, sds)
#else
getSystemdSockets = return Nothing
#endif
