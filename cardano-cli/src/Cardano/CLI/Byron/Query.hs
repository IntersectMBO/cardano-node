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
import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as Text


{- HLINT ignore "Reduce duplication" -}

newtype ByronQueryError = ByronQueryErr EnvSocketError
  deriving Show

renderByronQueryError :: ByronQueryError -> Text
renderByronQueryError err =
  case err of
    ByronQueryErr sockEnvErr -> renderEnvSocketError sockEnvErr

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip :: NetworkId -> ExceptT ByronQueryError IO ()
runGetLocalNodeTip networkId = do
    SocketPath sockPath <- firstExceptT ByronQueryErr
                             $ newExceptT readEnvSocketPath
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath    = sockPath,
            localNodeNetworkId     = networkId,
            localConsensusModeParams = ByronModeParams (EpochSlots 21600)
          }

    tip <- liftIO $ getLocalChainTip connctInfo
    liftIO . putTextLn . Text.decodeUtf8 . LB.toStrict $ encodePretty tip


