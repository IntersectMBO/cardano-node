{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Tracing (tests) where

import           Cardano.Prelude

import           Hedgehog (Property, checkParallel, discover, property, withTests, (===))
import           Network.Socket (SockAddr (SockAddrInet, SockAddrInet6), tupleToHostAddress,
                   tupleToHostAddress6)

import           Ouroboros.Network.NodeToNode (ConnectionId (..), RemoteConnectionId)

import           Cardano.Tracing.Peer (ppCid)

prop_ppCid_IPv4 :: Property
prop_ppCid_IPv4 = withTests 1 $ property $ ppCid connIpV4 === "0.1.2.3"

connIpV4 :: RemoteConnectionId
connIpV4 =
  ConnectionId
    { localAddress = SockAddrInet 7 8
    , remoteAddress = SockAddrInet 9 $ tupleToHostAddress (0, 1, 2, 3)
    }

prop_ppCid_IPv6 :: Property
prop_ppCid_IPv6 = withTests 1 $ property $ ppCid connIpV6 === "[::5]"

connIpV6 :: RemoteConnectionId
connIpV6 =
  ConnectionId
    { localAddress =
        SockAddrInet6 1 2 (tupleToHostAddress6 (3, 4, 5, 6, 7, 8, 9, 0)) 1
    , remoteAddress =
        SockAddrInet6 2 3 (tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 5)) 6
    }

tests :: IO Bool
tests = checkParallel $$discover
