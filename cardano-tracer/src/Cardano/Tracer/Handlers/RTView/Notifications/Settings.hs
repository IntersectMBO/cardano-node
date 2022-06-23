{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Settings
  ( incompleteEmailSettings
  , readSavedEmailSettings
  , readSavedEventsSettings
  , saveEmailSettingsOnDisk
  , saveEventsSettingsOnDisk
  ) where

import           Control.Exception.Extra (ignore, try_)
import           Crypto.Cipher.AES ()
import           Crypto.Cipher.Types ()
import           Crypto.Error ()
-- import           Crypto.Cipher.AES (AES256)
-- import           Crypto.Cipher.Types (BlockCipher (..), cipherInit, ctrCombine, nullIV)
-- import           Crypto.Error (CryptoError, eitherCryptoError)
import           Data.Aeson (decodeStrict', encode, encodeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.System

readSavedEmailSettings :: IO EmailSettings
readSavedEmailSettings = do
  (pathToEmailSettings, _) <- getPathsToNotificationsSettings
  try_ (BS.readFile pathToEmailSettings) >>= \case
    Left _ -> return defaultSettings
    Right jsonSettings ->
      case decodeStrict' jsonSettings of
        Nothing -> return defaultSettings
        Just (settings :: EmailSettings) -> return settings
    -- Right encryptedSettings ->
    --   case decryptJSON encryptedSettings of
    --     Left _ -> return defaultSettings
    --     Right jsonSettings ->
    --       case decodeStrict' jsonSettings of
    --         Nothing -> return defaultSettings
    --         Just (settings :: EmailSettings) -> return settings
 where
  defaultSettings = EmailSettings
    { esSMTPHost  = ""
    , esSMTPPort  = -1
    , esUsername  = ""
    , esPassword  = ""
    , esSSL       = TLS
    , esEmailFrom = ""
    , esEmailTo   = ""
    , esSubject   = ""
    }

  -- decryptJSON :: BS.ByteString -> Either CryptoError BS.ByteString
  -- decryptJSON = encryptJSON -- Encryption/decryption is symmetric.

incompleteEmailSettings :: EmailSettings -> Bool
incompleteEmailSettings emailSettings = T.null $ esSMTPHost emailSettings 

-- encryptJSON :: BS.ByteString -> Either CryptoError BS.ByteString
-- encryptJSON plainJSON = ctrCombine
--   <$> cInit
--   <*> pure nullIV
--   <*> pure plainJSON
--  where
--   cInit :: Either CryptoError AES256
--   cInit = eitherCryptoError $ cipherInit key
--
--   -- The length must be exactly 32 bytes (256 bits).
--   key :: BS.ByteString
--   key = "n3+d6^jrodGe$1Ljwt;iBtsi_mxzp-47"

readSavedEventsSettings :: IO EventsSettings
readSavedEventsSettings = do
  (_, pathToEventsSettings) <- getPathsToNotificationsSettings
  try_ (BS.readFile pathToEventsSettings) >>= \case
    Left _ -> return defaultSettings
    Right jsonSettings ->
      case decodeStrict' jsonSettings of
        Nothing -> return defaultSettings
        Just (settings :: EventsSettings) -> return settings
 where
  defaultSettings = EventsSettings
    { evsWarnings         = defaultState
    , evsErrors           = defaultState
    , evsCriticals        = defaultState
    , evsAlerts           = defaultState
    , evsEmergencies      = defaultState
    , evsNodeDisconnected = defaultState
    }
  defaultState = (False, 1800)

saveEmailSettingsOnDisk :: EmailSettings -> IO ()
saveEmailSettingsOnDisk settings = ignore $ do
  (pathToEmailSettings, _) <- getPathsToNotificationsSettings
  LBS.writeFile pathToEmailSettings $ encode settings
  -- Encrypt JSON-content to avoid saving user's private data in "plain mode".
  -- case encryptJSON . LBS.toStrict . encode $ settings of
  --   Right encryptedJSON -> BS.writeFile pathToEmailSettings encryptedJSON
  --   Left _ -> return ()

saveEventsSettingsOnDisk :: EventsSettings -> IO ()
saveEventsSettingsOnDisk settings = ignore $ do
  (_, pathToEventsSettings) <- getPathsToNotificationsSettings
  encodeFile pathToEventsSettings settings
