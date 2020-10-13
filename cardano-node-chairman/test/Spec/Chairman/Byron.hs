{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Chairman.Byron
  ( hprop_chairman
  ) where

import           Data.Function
import           Spec.Chairman.Chairman (chairmanOver)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Test.Base as H
import qualified Testnet.Byron as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

hprop_chairman :: H.Property
hprop_chairman = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsPath' -> do
  conf@H.Conf {..} <- H.mkConf tempAbsPath' 42
  allNodes <- H.testnet conf

  chairmanOver conf allNodes
