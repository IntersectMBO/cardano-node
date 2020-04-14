{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Topology
  ( NetworkTopology(..)
  , NodeHostAddress(..)
  , NodeSetup(..)
  , NodeSockets(..)
  , RemoteAddress(..)
  , chooseSocketPath
  , localSocketPath
  , nodeAddressInfo
  , nodeAddressToSockAddr
  , nodeLocalSocketAddrInfo
  , readTopologyFile
  , remoteAddressToNodeAddress
  , removeStaleLocalSocket
  )
where

import           Cardano.Prelude hiding (toS)
import           Prelude (String)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Tracer (Tracer, traceWith)
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.IP as IP
import           Data.String.Conv (toS)
import qualified Data.Text as T
import           Text.Read (readMaybe)
import           Network.Socket
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath (takeDirectory)
import           System.IO.Error (isDoesNotExistError)
import           System.Systemd.Daemon (getActivatedSocketsWithNames)


import           Cardano.Config.Types

import           Ouroboros.Consensus.Util.Condense (Condense (..))


data NodeAddressError = ActivatedSocketsNotFound
                      | IncorrectNumberOfActivatedSockets Text
                      deriving Show

type TCPSockets = [AddrInfo]

data NodeSockets = ActivatedSockets TCPSockets FilePath
                 | NonActivatedSockets TCPSockets FilePath

-- | This lets us override the socket path specified in the node configuration yaml file
-- if required.
chooseSocketPath :: Maybe YamlSocketPath -> Maybe CLISocketPath -> SocketPath
chooseSocketPath Nothing Nothing = panic $ "Cardano.Config.Topology.chooseSocketPath: "
                                         <> "Please specify a socket path either in the config yaml "
                                         <> "file or on the command line."
chooseSocketPath (Just yamlSockPath) Nothing = unYamlSocketPath yamlSockPath
chooseSocketPath Nothing (Just cliSockPath) = unCLISocketPath cliSockPath
chooseSocketPath _ (Just cliSockPath) = unCLISocketPath cliSockPath

-- | Provide an filepath intended for a socket situated in 'socketDir'.
-- When 'mkdir' is 'MkdirIfMissing', the directory is created.
localSocketPath :: SocketPath -> IO FilePath
localSocketPath (SocketFile fp) = do
  createDirectoryIfMissing True $ takeDirectory fp
  return fp

nodeAddressToSockAddr :: NodeAddress -> ExceptT NodeAddressError IO SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case getAddress addr of
    Just (IP.IPv4 ipv4) -> pure $ SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> pure $ SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing -> pure $ SockAddrInet port 0 -- Could also be any IPv6 addr
nodeAddressToSockAddr _ = pure $ SockAddrInet 9009 98989898


-- | Get TCP/IP and UNIX sockets for local node: (Activated TCP/IP sockets, Activated UNIX socket, Maybe Filepath for non-activated socket)
nodeAddressInfo :: Tracer IO String -> NodeProtocolMode -> NodeConfiguration -> ExceptT NodeAddressError IO NodeSockets
nodeAddressInfo trace' npm nc = do
    mActivatedSockets <- useActivatedSockets npm
    liftIO $ traceWith trace' $ "Activated socket names: " <> show mActivatedSockets
    case mActivatedSockets of
      Nothing -> do let NodeAddress hostAddr port = extractNodeAddress
                    localUnixSocketFp <- liftIO $ nodeLocalSocketAddrInfo nc npm
                    tcpIpSocket <- liftIO $ getAddrInfo (Just tcpIpHints) (show <$> getAddress hostAddr) (Just $ show port)
                    pure $ NonActivatedSockets tcpIpSocket localUnixSocketFp

      Just (_:(ipv4skt,_):(ipv6skt,_):(localUnixSocket,_) : []) -> do
                    -- TODO: Handle these exceptions via ExceptT
                    addrInfo4 <- liftIO $ getSocketAddr ipv4skt
                    addrInfo6 <- liftIO $ getSocketAddr ipv6skt
                    name <- liftIO $ getSocketName localUnixSocket
                    liftIO $ traceWith trace' $ "UNIX Socket name: " <> show name
                    pure $ ActivatedSockets (addrInfo4 ++ addrInfo6) (show name)

      _ ->  left . IncorrectNumberOfActivatedSockets
                 $ "Cardano.Config.Topology.nodeAddressInfo: Was expecting IPv4 socket, IPv6 socket and a UNIX socket."
                 <> "Make sure your .socket file has only these sockets, anymore or less will result in this failure."
 where
  getSocketAddr :: Socket -> IO [AddrInfo]
  getSocketAddr skt = do name <- getSocketName skt
                         (ip, port) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True name
                         getAddrInfo (Just tcpIpHints) ip port
  --getUNIXSocketAddr :: Socket -> IO [AddrInfo]
  --getUNIXSocketAddr skt = do name <- getSocketName skt
  --                           (hostname, servicename) <- getNameInfo [] True True name
  --                           getAddrInfo (Just unixHints) hostname servicename

  tcpIpHints :: AddrInfo
  tcpIpHints = defaultHints { addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
                            , addrSocketType = Stream
                            }
  --unixHints :: AddrInfo
  --unixHints = defaultHints { addrFlags = [AI_CANONNAME]
  --                         , addrSocketType = SeqPacket
  --                         , addrFamily = AF_UNIX
  --                         }
  extractNodeAddress :: NodeAddress
  extractNodeAddress = case npm of
                         (RealProtocolMode (NodeCLI {nodeAddr})) -> nodeAddr
                         (MockProtocolMode (NodeMockCLI {mockNodeAddr})) -> mockNodeAddr


nodeLocalSocketAddrInfo :: NodeConfiguration -> NodeProtocolMode -> IO FilePath
nodeLocalSocketAddrInfo nc npm = do
  mCliSockPath <- case npm of
                    MockProtocolMode (NodeMockCLI {mockMscFp}) -> pure $ socketFile mockMscFp
                    RealProtocolMode (NodeCLI {mscFp}) -> pure $ socketFile mscFp

  localSocketPath $ chooseSocketPath (ncSocketPath nc) mCliSockPath

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress :: !String
  -- ^ either a dns address or ip address
  , raPort :: !PortNumber
  -- ^ port number of the destination
  , raValency :: !Int
  -- ^ if a dns address is given valency governs
  -- to how many resolved ip addresses
  -- should we maintain acctive (hot) connection;
  -- if an ip address is given valency is used as
  -- a boolean value, @0@ means to ignore the address;
  } deriving (Eq, Ord, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress :: RemoteAddress-> Maybe NodeAddress
remoteAddressToNodeAddress (RemoteAddress addrStr port val) =
  case readMaybe addrStr of
    Nothing -> Nothing
    Just addr -> if val /= 0
                 then Just $ NodeAddress (NodeHostAddress $ Just addr) port
                 else Nothing

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

instance Condense RemoteAddress where
  condense (RemoteAddress addr port val) =
    addr ++ ":" ++ show port ++ " (" ++ show val ++ ")"

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> (v .: "valency")

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeAddress :: !NodeAddress
  , producers :: ![RemoteAddress]
  } deriving Show

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .: "nodeId"
                  <*> o .: "nodeAddress"
                  <*> o .: "producers"

data NetworkTopology = MockNodeTopology ![NodeSetup]
                     | RealNodeTopology ![RemoteAddress]
                     deriving Show

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o -> asum
                [ MockNodeTopology <$> o .: "MockProducers"
                , RealNodeTopology <$> o .: "Producers"
                ]

-- | Read the `NetworkTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readTopologyFile :: NodeProtocolMode -> IO (Either Text NetworkTopology)
readTopologyFile npm = do
  topo  <- case npm of
             (RealProtocolMode (NodeCLI {mscFp})) -> pure . unTopology $ topFile mscFp
             (MockProtocolMode (NodeMockCLI {mockMscFp})) -> pure . unTopology $ topFile mockMscFp

  eBs <- Exception.try $ BS.readFile topo

  case eBs of
    Left e -> pure . Left $ handler e
    Right bs -> pure . first T.pack . eitherDecode $ toS bs

 where
  handler :: IOException -> Text
  handler e = T.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                     ++ displayException e

useActivatedSockets :: NodeProtocolMode -> ExceptT NodeAddressError IO (Maybe [(Socket,String)])
useActivatedSockets (MockProtocolMode (NodeMockCLI {mockNodeAddr})) =
  case mockNodeAddr of
    ActSocks sActivation -> case sActivation of
                              UseActivatedSockets -> do mSkts <- liftIO getActivatedSocketsWithNames
                                                        if isNothing mSkts then left ActivatedSocketsNotFound else pure mSkts
                              UseNormalSockets -> pure Nothing
    _ -> pure Nothing
useActivatedSockets (RealProtocolMode (NodeCLI {nodeAddr})) =
  case nodeAddr of
     ActSocks sActivation -> case sActivation of
                               UseActivatedSockets -> do mSkts <- liftIO getActivatedSocketsWithNames
                                                         if isNothing mSkts then left ActivatedSocketsNotFound else pure mSkts
                               UseNormalSockets -> pure Nothing
     _ -> pure Nothing
