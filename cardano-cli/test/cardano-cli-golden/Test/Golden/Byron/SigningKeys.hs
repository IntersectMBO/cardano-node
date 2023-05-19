{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.SigningKeys
  ( tests
  ) where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Monad (void)
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy as LB

import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Byron

import           Cardano.CLI.Byron.Key (readByronSigningKey)
import           Cardano.CLI.Byron.Legacy (decodeLegacyDelegateKey)
import           Cardano.CLI.Shelley.Commands

import           Hedgehog (Group (..), Property, checkSequential, property, success)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import           Hedgehog.Internal.Property (failWith)
import           Test.Cardano.CLI.Util

prop_deserialise_legacy_signing_Key :: Property
prop_deserialise_legacy_signing_Key = propertyOnce $ do
  legSkeyBs <- H.evalIO $ LB.readFile "test/cardano-cli-golden/files/golden/byron/keys/legacy.skey"
  case deserialiseFromBytes decodeLegacyDelegateKey legSkeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

prop_deserialise_nonLegacy_signing_Key :: Property
prop_deserialise_nonLegacy_signing_Key = propertyOnce $ do
  skeyBs <- H.evalIO $ LB.readFile "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"
  case deserialiseFromBytes Crypto.fromCBORXPrv skeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

prop_print_legacy_signing_key_address :: Property
prop_print_legacy_signing_key_address = propertyOnce $ do
  let legKeyFp = "test/cardano-cli-golden/files/golden/byron/keys/legacy.skey"

  void $ execCardanoCLI
   [ "signing-key-address", "--byron-legacy-formats"
   , "--testnet-magic", "42"
   , "--secret", legKeyFp
   ]

  void $ execCardanoCLI
   [ "signing-key-address", "--byron-legacy-formats"
   , "--mainnet"
   , "--secret", legKeyFp
   ]

prop_print_nonLegacy_signing_key_address :: Property
prop_print_nonLegacy_signing_key_address = propertyOnce $ do
  let nonLegKeyFp = "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"

  void $ execCardanoCLI
   [ "signing-key-address", "--byron-formats"
   , "--testnet-magic", "42"
   , "--secret", nonLegKeyFp
   ]

  void $ execCardanoCLI
   [ "signing-key-address", "--byron-formats"
   , "--mainnet"
   , "--secret", nonLegKeyFp
   ]

prop_generate_and_read_nonlegacy_signingkeys :: Property
prop_generate_and_read_nonlegacy_signingkeys = property $ do
  byronSkey <- H.evalIO $ generateSigningKey AsByronKey
  case deserialiseFromRawBytes (AsSigningKey AsByronKey) (serialiseToRawBytes byronSkey) of
    Left _ -> failWith Nothing "Failed to deserialise non-legacy Byron signing key. "
    Right _ -> success

prop_migrate_legacy_to_nonlegacy_signingkeys :: Property
prop_migrate_legacy_to_nonlegacy_signingkeys =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let legKeyFp = "test/cardano-cli-golden/files/golden/byron/keys/legacy.skey"
    nonLegacyKeyFp <- noteTempFile tempDir "nonlegacy.skey"

    void $ execCardanoCLI
     [ "migrate-delegate-key-from"
     , "--from", legKeyFp
     , "--to", nonLegacyKeyFp
     ]

    eSignKey <- H.evalIO . runExceptT . readByronSigningKey NonLegacyByronKeyFormat
                  $ File nonLegacyKeyFp

    case eSignKey of
      Left err -> failWith Nothing $ show err
      Right _ -> success

prop_deserialise_NonLegacy_Signing_Key_API :: Property
prop_deserialise_NonLegacy_Signing_Key_API = propertyOnce $ do
  eFailOrWit <- H.evalIO . runExceptT $ readByronSigningKey NonLegacyByronKeyFormat "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"
  case eFailOrWit of
    Left keyFailure -> failWith Nothing $ show keyFailure
    Right _ -> success

prop_deserialiseLegacy_Signing_Key_API :: Property
prop_deserialiseLegacy_Signing_Key_API = propertyOnce $ do
  eFailOrWit <- H.evalIO . runExceptT $ readByronSigningKey LegacyByronKeyFormat "test/cardano-cli-golden/files/golden/byron/keys/legacy.skey"
  case eFailOrWit of
    Left keyFailure -> failWith Nothing $ show keyFailure
    Right _ -> success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential
    $ Group "Byron Signing Key Serialisation"
        [ ("prop_deserialise_legacy_signing_Key", prop_deserialise_legacy_signing_Key)
        , ("prop_print_legacy_signing_key_address", prop_print_legacy_signing_key_address)
        , ("prop_deserialise_nonLegacy_signing_Key", prop_deserialise_nonLegacy_signing_Key)
        , ("prop_print_nonLegacy_signing_key_address", prop_print_nonLegacy_signing_key_address)
        , ("prop_generate_and_read_nonlegacy_signingkeys", prop_generate_and_read_nonlegacy_signingkeys)
        , ("prop_migrate_legacy_to_nonlegacy_signingkeys", prop_migrate_legacy_to_nonlegacy_signingkeys)
        , ("prop_deserialise_NonLegacy_Signing_Key_API", prop_deserialise_NonLegacy_Signing_Key_API)
        , ("prop_deserialiseLegacy_Signing_Key_API", prop_deserialiseLegacy_Signing_Key_API)
        ]

