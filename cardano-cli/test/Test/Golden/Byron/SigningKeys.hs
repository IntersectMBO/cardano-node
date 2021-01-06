{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Golden.Byron.SigningKeys
  ( tests
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.ByteString.Lazy as LB

import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.CLI.Byron.Legacy (decodeLegacyDelegateKey)

import           Hedgehog (Property, checkParallel, discover, success)
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

prop_deserialise_Legacy_Signing_Key :: Property
prop_deserialise_Legacy_Signing_Key = propertyOnce $ do
  legSkeyBs <- liftIO $ LB.readFile "test/data/golden/byron/keys/legacy.skey"
  case deserialiseFromBytes decodeLegacyDelegateKey legSkeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

prop_deserialise_NonLegacy_Signing_Key :: Property
prop_deserialise_NonLegacy_Signing_Key = propertyOnce $ do
  skeyBs <- liftIO $ LB.readFile "test/data/golden/byron/keys/byron.skey"
  case deserialiseFromBytes Crypto.fromCBORXPrv skeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkParallel $$discover
