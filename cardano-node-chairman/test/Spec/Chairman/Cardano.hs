{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Spec.Chairman.Cardano where

import           Cardano.Testnet (allNodes, cardanoTestnetDefault, mkConf)

import           Data.Default.Class

import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H

import           Spec.Chairman.Chairman (chairmanOver)

hprop_chairman :: H.Property
hprop_chairman = integrationRetryWorkspace 2 "cardano-chairman" $ \tempAbsPath' -> do
  conf <- mkConf tempAbsPath'

  allNodes' <- allNodes <$> cardanoTestnetDefault def def conf

  chairmanOver 120 50 conf allNodes'
