{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Config.Mainnet
  ( tests
  ) where

import           Cardano.Api (initialLedgerState, renderInitialLedgerStateError)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Bool (Bool(..))
import           Data.Either (Either(..))
import           Data.Function
import           Data.Maybe
import           Hedgehog (Property)
import           System.FilePath ((</>))
import           System.IO (IO)

import qualified Data.Text as T
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO

hprop_configMainnet :: Property
hprop_configMainnet = H.propertyOnce $ do
  projectBase <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  result <- H.evalIO $ runExceptT $ initialLedgerState $ projectBase </> "configuration/cardano/mainnet-config.json"
  case result of
    Right (_, _) -> return ()
    Left e -> H.failWithCustom GHC.callStack Nothing (T.unpack (renderInitialLedgerStateError e))

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Test.Config.Mainnet"
        [ ("hprop_configMainnet", hprop_configMainnet)
        ]
