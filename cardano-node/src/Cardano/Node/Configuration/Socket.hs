{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Configuration.Socket
  ( SocketConfig (..)
  , gatherConfiguredSockets
  , SocketOrSocketInfo' (..)
  , SocketOrSocketInfo
  , LocalSocketOrSocketInfo
  , getSocketOrSocketInfoAddr
  , SocketConfigError(..)
  , renderSocketConfigError
  )
where

import           Cardano.Prelude hiding (local)
import           Prelude (String)
import qualified Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Generic.Data.Orphans ()
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (AF_INET, AF_INET6),
                   Socket, SocketType (..))
import qualified Network.Socket as Socket

import           Cardano.Node.Configuration.NodeAddress

import           Ouroboros.Network.NodeToClient (LocalAddress (..), LocalSocket (..))

#if !defined(mingw32_HOST_OS)
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)
#endif

#ifdef SYSTEMD
import           System.Systemd.Daemon (getActivatedSockets)
#endif

{- HLINT ignore "Use =<<" -}


-- | Since we support systemd socket activation, we have to handle being
-- given actual already-constructed sockets, or the info needed to make new
-- sockets later.
--
data SocketOrSocketInfo' socket info =
       ActualSocket socket
     | SocketInfo   info
  deriving Show

type SocketOrSocketInfo = SocketOrSocketInfo' Socket Socket.SockAddr
type LocalSocketOrSocketInfo = SocketOrSocketInfo' LocalSocket LocalAddress


getSocketOrSocketInfoAddr :: SocketOrSocketInfo
                          -> IO Socket.SockAddr
getSocketOrSocketInfoAddr (ActualSocket sock) =
    Socket.getSocketName sock
getSocketOrSocketInfoAddr (SocketInfo sockAddr)  =
    return sockAddr


-- | Errors for the current module.
data SocketConfigError
    = NoPublicSocketGiven
    | NoLocalSocketGiven
    | ClashingPublicIpv4SocketGiven
    | ClashingPublicIpv6SocketGiven
    | ClashingLocalSocketGiven
    | LocalSocketError SocketPath IOException
    | GetAddrInfoError (Maybe NodeHostIPAddress) (Maybe PortNumber) IOException
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

renderSocketConfigError ClashingPublicIpv4SocketGiven =
    "Configuration for the node's public IPv4 socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError ClashingPublicIpv6SocketGiven =
    "Configuration for the node's public IPv6 socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError ClashingLocalSocketGiven =
    "Configuration for the node's local socket supplied both by config/cli and "
 <> "via systemd socket activation. Please use one or the other but not both."

renderSocketConfigError (LocalSocketError fp ex) =
    "Failure while attempting to remove the stale local socket: "
 <> unFile fp <> " : " <> displayException ex

renderSocketConfigError (GetAddrInfoError addr port ex) =
    "Failure while getting address information for the public listening "
 <> "address: " <> show addr <> " " <> show port <> " : " <> displayException ex

data SocketConfig
  = SocketConfig
    { ncNodeIPv4Addr    :: !(Last NodeHostIPv4Address)
    , ncNodeIPv6Addr    :: !(Last NodeHostIPv6Address)
    , ncNodePortNumber  :: !(Last PortNumber)
    , ncSocketPath      :: !(Last SocketPath)
    }
    deriving (Eq, Show)

-- | Gather from the various sources of configuration which sockets we will use
-- for the public node-to-node and the local node-to-client IPC.  It returns
-- 'SocketOrSocketInfo' for @ipv4@, @ipv6@ and local socket.
--
-- We get such configuration from:
--
-- * node config file
-- * node cli
-- * systemd socket activation
--
gatherConfiguredSockets :: SocketConfig
                        -> ExceptT SocketConfigError IO
                                   (Maybe SocketOrSocketInfo,
                                    Maybe SocketOrSocketInfo,
                                    Maybe LocalSocketOrSocketInfo)
gatherConfiguredSockets SocketConfig { ncNodeIPv4Addr,
                                       ncNodeIPv6Addr,
                                       ncNodePortNumber,
                                       ncSocketPath } = do

    systemDSockets <- liftIO getSystemdSockets

    -- Select the sockets or address for public node-to-node comms
    -- TODO: add config file support
    let -- The first systemd IPv4 socket if it exists
        firstIpv4Socket :: Maybe Socket
        firstIpv4Socket = join $ listToMaybe . (\(a, _, _) -> a) <$> systemDSockets

        -- The first systemd IPv6 socket if it exists
        firstIpv6Socket :: Maybe Socket
        firstIpv6Socket = join $ listToMaybe . (\(_, a, _) -> a) <$> systemDSockets

    -- only when 'ncNodeIPv4Addr' is specified or an ipv4 socket is passed
    -- through socket activation
    ipv4 <- case (getLast ncNodeIPv4Addr, firstIpv4Socket) of
      (Nothing, Nothing)    -> pure Nothing
      (Nothing, Just sock)  -> return (Just (ActualSocket sock))
      (Just _, Just _)      -> throwError ClashingPublicIpv4SocketGiven
      (Just addr, Nothing)  ->
            fmap (SocketInfo . addrAddress) . head
        <$> nodeAddressInfo
              (Just $ nodeHostIPv4AddressToIPAddress addr)
              (getLast ncNodePortNumber)

    -- only when 'ncNodeIPv6Addr' is specified or an ipv6 socket is passed
    -- through socket activation
    ipv6 <- case (getLast ncNodeIPv6Addr, firstIpv6Socket) of
      (Nothing, Nothing)    -> pure Nothing
      (Nothing, Just sock)  -> return (Just (ActualSocket sock))
      (Just _, Just _)      -> throwError ClashingPublicIpv6SocketGiven
      (Just addr, Nothing)  ->
              fmap (SocketInfo . addrAddress) . head
          <$> nodeAddressInfo
                (Just $ nodeHostIPv6AddressToIPAddress addr)
                (getLast ncNodePortNumber)

    -- When none of the addresses was given. We try resolve address passing
    -- only 'ncNodePortNumber'.
    (ipv4', ipv6')
      <- case (ipv4, ipv6) of
            (Nothing, Nothing) -> do

              info <- nodeAddressInfo Nothing $ getLast ncNodePortNumber
              let ipv4' = SocketInfo . addrAddress
                      <$> find ((== AF_INET)  . addrFamily) info
                  ipv6' = SocketInfo . addrAddress
                      <$> find ((== AF_INET6) . addrFamily) info
              when (isNothing $ ipv4' <|> ipv6') $
                throwError NoPublicSocketGiven

              pure (ipv4', ipv6')

            _ -> pure (ipv4, ipv6)


    -- Select the socket or path for local node-to-client comms
    --
    let firstUnixSocket :: Maybe LocalSocket
        firstUnixSocket = join $ listToMaybe . (\(_, _, a) -> a) <$> systemDSockets

    -- only when 'ncSocketpath' is specified or a unix socket is passed through
    -- socket activation
    local <- case (getLast ncSocketPath, firstUnixSocket) of
      (Nothing, Nothing)    -> return Nothing
      (Just _, Just _)      -> throwError ClashingLocalSocketGiven
      (Nothing, Just sock)  -> return . Just $ ActualSocket sock
      (Just socketPath, Nothing)
                            -> removeStaleLocalSocket socketPath
                            $> Just (SocketInfo (LocalAddress (unFile socketPath)))

    return (ipv4', ipv6', local)


-- | Binding a local unix domain socket always expects to create it, and fails
-- if it exists already. So we delete it first if it exists. But only on unix.
--
removeStaleLocalSocket :: SocketPath -> ExceptT SocketConfigError IO ()
#if defined(mingw32_HOST_OS)
removeStaleLocalSocket _ =
    return ()
#else
removeStaleLocalSocket path =
    handleIOExceptT (LocalSocketError path) $
      removeFile (unFile path) `catch` \e ->
        if isDoesNotExistError e then return ()
                                 else throwIO e
#endif

nodeAddressInfo :: Maybe NodeHostIPAddress
                -> Maybe PortNumber
                -> ExceptT SocketConfigError IO [AddrInfo]
nodeAddressInfo mbHostAddr mbPort =
    handleIOExceptT (GetAddrInfoError mbHostAddr mbPort) $
      Socket.getAddrInfo
        (Just hints)
        (Prelude.show <$> mbHostAddr)
        (Prelude.show <$> mbPort)
  where
    hints = Socket.defaultHints {
                addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
              , addrSocketType = Stream
              }


-- | Possibly return systemd-activated sockets.  Splits the sockets into three
-- groups:'AF_INET' and 'AF_INET6', 'AF_UNIX'.
--
getSystemdSockets :: IO (Maybe ([Socket], [Socket], [LocalSocket]))
#ifdef SYSTEMD
getSystemdSockets = do
  sds_m <- getActivatedSockets
  case sds_m of
       Nothing    -> return Nothing
       Just socks ->
         Just <$>
          foldM (\(ipv4s, ipv6s, unixs) sock -> do
                  addr <- Socket.getSocketName sock
                  return $ case addr of
                    Socket.SockAddrInet {}  ->
                      (sock : ipv4s,        ipv6s,                    unixs)
                    Socket.SockAddrInet6 {} ->
                      (       ipv4s, sock : ipv6s,                    unixs)
                    Socket.SockAddrUnix {}  ->
                      (       ipv4s,        ipv6s, LocalSocket sock : unixs))
                ([], [], [])
                socks
#else
getSystemdSockets = return Nothing
#endif
