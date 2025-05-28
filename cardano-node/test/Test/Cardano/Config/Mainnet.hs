{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Config.Mainnet
  ( tests
  ) where

import           Cardano.Api (File (..), initialLedgerState)
import           Cardano.Api.Error (displayError)

import           Control.Monad.Trans.Except
import qualified Data.Aeson as J
import qualified Data.Yaml as Y
import qualified GHC.Stack as GHC
import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H

hprop_configMainnetHash :: Property
hprop_configMainnetHash = H.propertyOnce $ do
  base <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  result <- H.evalIO $ runExceptT $ initialLedgerState $ File $ base </> "configuration/cardano/mainnet-config.json"
  case result of
    Right (_, _) -> return ()
    Left e -> H.failWithCustom GHC.callStack Nothing (displayError e)

hprop_configMainnetYaml :: Property
hprop_configMainnetYaml = H.propertyOnce $ do
  base <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  yamlResult <- H.evalIO . Y.decodeFileEither $ base </> "configuration/cardano/mainnet-config.yaml"
  yaml :: J.Value <- case yamlResult of
    Right v -> return v
    Left e -> H.failWithCustom GHC.callStack Nothing (Y.prettyPrintParseException e)
  jsonResult <- H.evalIO . J.eitherDecodeFileStrict $ base </> "configuration/cardano/mainnet-config.json"
  json  :: J.Value <- case jsonResult of
    Right v -> return v
    Left e -> H.failWithCustom GHC.callStack Nothing (show e)
  yaml === json

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Test.Config.Mainnet"
        [ ("hprop_configMainnetHash", hprop_configMainnetHash)
        , ("hprop_configMainnetYaml", hprop_configMainnetYaml)
        ]
