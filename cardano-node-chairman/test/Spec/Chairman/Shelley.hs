{-# LANGUAGE OverloadedStrings #-}

module Spec.Chairman.Shelley
  ( hprop_chairman
  ) where

import           Prelude
import           Spec.Chairman.Chairman (chairmanOver)
import           System.FilePath ((</>))

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified Test.Base as H
import qualified Test.Runtime as H
import qualified Testnet.Conf as H
import qualified Testnet.Shelley as H

hprop_chairman :: H.Property
hprop_chairman = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsPath' -> do
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsPath' Nothing

  allNodes <- fmap H.nodeName . H.allNodes <$> H.shelleyTestnet H.defaultTestnetOptions conf

  chairmanOver 120 21 conf allNodes
