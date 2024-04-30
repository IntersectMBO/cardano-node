{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Spec.Chairman.Cardano where

import           Cardano.Testnet (NodeRuntime (nodeName), allNodes, cardanoDefaultTestnetOptions,
                   cardanoTestnetDefault, mkConf)

import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H

import           Spec.Chairman.Chairman (chairmanOver)

-- TODO: Conway broken in conway
hprop_chairman :: H.Property
hprop_chairman = integrationRetryWorkspace 0 "cardano-chairman" $ \tempAbsPath' -> do
  conf <- mkConf tempAbsPath'

  allNodes' <- fmap nodeName . allNodes <$> cardanoTestnetDefault cardanoDefaultTestnetOptions conf

  chairmanOver 120 50 conf allNodes'
