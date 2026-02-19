{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Spec.Chairman.Cardano where

import           Cardano.Testnet

import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

import           Spec.Chairman.Chairman (chairmanOver)

hprop_chairman :: H.Property
hprop_chairman = integrationRetryWorkspace 2 "cardano-chairman" $ \tempAbsPath' -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tempAbsPath'

  let testnetOptions = def{ cardanoNodes = SpoNodeOptions [] :| [RelayNodeOptions [], RelayNodeOptions []] }
  allNodes <- testnetNodes <$> createAndRunTestnet testnetOptions def conf

  chairmanOver 120 50 conf allNodes
