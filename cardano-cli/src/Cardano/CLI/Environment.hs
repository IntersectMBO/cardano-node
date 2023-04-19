{-# LANGUAGE TypeApplications #-}

-- | This module defines constants derived from the environment.
module Cardano.CLI.Environment
  ( EnvCli(..)
  , getEnvCli
  , getEnvNetworkId
  ) where

import           Cardano.Api (NetworkId (..), NetworkMagic (..))

import           Data.Word (Word32)
import qualified System.Environment as IO
import qualified System.IO as IO
import           Text.Read (readMaybe)

newtype EnvCli = EnvCli
  { cliEnvNetworkId :: Maybe NetworkId
  }

getEnvCli :: IO EnvCli
getEnvCli = EnvCli <$> getEnvNetworkId

-- | If the environment variable @CARDANO_NODE_NETWORK_ID@ is set, then return the network id therein.
-- Otherwise, return 'Nothing'.
getEnvNetworkId :: IO (Maybe NetworkId)
getEnvNetworkId = do
  mNetworkIdString <- IO.lookupEnv "CARDANO_NODE_NETWORK_ID"

  case mNetworkIdString of
    Nothing -> pure Nothing
    Just networkIdString -> do
      case networkIdString of
        "mainnet" -> pure $ Just Mainnet
        _ ->
          case readMaybe @Word32 networkIdString of
            Just networkId -> pure $ Just $ Testnet $ NetworkMagic networkId
            Nothing -> do
              IO.hPutStrLn IO.stderr $ mconcat
                [ "The network id specified in CARDANO_NODE_NETWORK_ID invalid: " <> networkIdString
                , " It should be either 'mainnet' or a number."
                ]
              pure Nothing
