{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Query
  ( runGetLocalNodeTip
  ) where

import           Cardano.Api

import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text


{- HLINT ignore "Reduce duplication" -}

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip
  :: SocketPath
  -> NetworkId
  -> IO ()
runGetLocalNodeTip socketPath networkId = do
  let connctInfo =
        LocalNodeConnectInfo
          { localNodeSocketPath = socketPath
          , localNodeNetworkId = networkId
          , localConsensusModeParams = ByronModeParams (EpochSlots 21600)
          }

  tip <- getLocalChainTip connctInfo
  Text.putStrLn . Text.decodeUtf8 . LB.toStrict $ encodePretty tip


