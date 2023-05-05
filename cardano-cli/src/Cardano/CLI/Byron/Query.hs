{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Query
  ( ByronQueryError(..)
  , renderByronQueryError
  , runGetLocalNodeTip
  ) where

import           Cardano.Api

import           Control.Monad.IO.Unlift (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text


{- HLINT ignore "Reduce duplication" -}

newtype ByronQueryError = ByronQueryEnvVarSocketErr EnvSocketError
  deriving Show

renderByronQueryError :: ByronQueryError -> Text
renderByronQueryError err =
  case err of
    ByronQueryEnvVarSocketErr sockEnvErr -> renderEnvSocketError sockEnvErr

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip
  :: SocketPath
  -> NetworkId
  -> ExceptT ByronQueryError IO ()
runGetLocalNodeTip (SocketPath sockPath) networkId = do
  let connctInfo =
        LocalNodeConnectInfo {
          localNodeSocketPath    = sockPath,
          localNodeNetworkId     = networkId,
          localConsensusModeParams = ByronModeParams (EpochSlots 21600)
        }

  tip <- liftIO $ getLocalChainTip connctInfo
  liftIO . Text.putStrLn . Text.decodeUtf8 . LB.toStrict $ encodePretty tip


