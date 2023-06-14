{-# LANGUAGE OverloadedStrings #-}

module Spec.Chairman.Cardano
  ( hprop_chairman
  ) where

import           Spec.Chairman.Chairman (chairmanOver)

import qualified Hedgehog as H

import qualified Cardano.Testnet as H
import qualified Testnet.Property.Utils as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

hprop_chairman :: H.Property
hprop_chairman = H.integrationRetryWorkspace 2 "cardano-chairman" $ \tempAbsPath' -> do
  conf <- H.mkConf tempAbsPath'

  allNodes <- fmap H.nodeName . H.allNodes <$> H.testnet (H.CardanoOnlyTestnetOptions H.cardanoDefaultTestnetOptions) conf

  chairmanOver 120 50 conf allNodes
