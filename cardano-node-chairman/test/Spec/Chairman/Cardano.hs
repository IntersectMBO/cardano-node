{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Spec.Chairman.Cardano where

import qualified Cardano.Testnet as H

import qualified Testnet.Property.Utils as H

import qualified Hedgehog as H

import           Spec.Chairman.Chairman (chairmanOver)

-- TODO: Conway broken in conway
hprop_chairman :: H.Property
hprop_chairman = H.integrationRetryWorkspace 2 "cardano-chairman" $ \tempAbsPath' -> do
  conf <- H.mkConf tempAbsPath'

  allNodes <- fmap H.nodeName . H.allNodes <$> H.cardanoTestnetDefault H.cardanoDefaultTestnetOptions conf

  chairmanOver 120 50 conf allNodes
