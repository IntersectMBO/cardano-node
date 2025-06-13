{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Rpc.Query
  ( hprop_rpc_query
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import qualified Cardano.Api.Network as Net.Tx
import           Cardano.Api.Shelley
import qualified Cardano.Api.Tx.UTxO as Utxo

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Proxy
import           Data.Set (Set)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro

import           Testnet.Components.Query
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_rpc_query :: Property
hprop_rpc_query = integrationRetryWorkspace 2 "rpc-query" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do

  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  tr@TestnetRuntime
    { configurationFile
    , testnetNodes = node0 : _
    , wallets = wallet0@(PaymentKeyInfo _ addrTxt0) : wallet1 : _
    } <-
    cardanoTestnetDefault options def conf

  systemStart <- H.noteShowM $ getStartTime tempAbsPath' tr
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  connectionInfo <- nodeConnectionInfo tr 0
  pparams <- getProtocolParams epochStateView ceo
  H.failure
