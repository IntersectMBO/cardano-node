{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Settings
  ( incompleteEmailSettings
  , readSavedEmailSettings
  , readSavedEventsSettings
  , saveEmailSettingsOnDisk
  , saveEventsSettingsOnDisk
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.System

import           Control.Exception.Extra (ignore, try_)
import           Data.Aeson (decodeStrict', encode, encodeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import           Crypto.Cipher.AES ()
import           Crypto.Cipher.Types ()
import           Crypto.Error ()

readSavedEmailSettings :: Maybe FilePath -> IO EmailSettings
readSavedEmailSettings rtvSD = do
  (pathToEmailSettings, _) <- getPathsToNotificationsSettings rtvSD
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

readSavedEventsSettings :: Maybe FilePath -> IO EventsSettings
readSavedEventsSettings rtvSD = do
  (_, pathToEventsSettings) <- getPathsToNotificationsSettings rtvSD
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

saveEmailSettingsOnDisk :: TracerEnv -> EmailSettings -> IO ()
saveEmailSettingsOnDisk TracerEnv{teRTViewStateDir} settings = ignore $ do
  (pathToEmailSettings, _) <- getPathsToNotificationsSettings teRTViewStateDir
  LBS.writeFile pathToEmailSettings $ encode settings
  -- Encrypt JSON-content to avoid saving user's private data in "plain mode".
  -- case encryptJSON . LBS.toStrict . encode $ settings of
  --   Right encryptedJSON -> BS.writeFile pathToEmailSettings encryptedJSON
  --   Left _ -> return ()

saveEventsSettingsOnDisk :: TracerEnv -> EventsSettings -> IO ()
saveEventsSettingsOnDisk TracerEnv{teRTViewStateDir} settings = ignore $ do
  (_, pathToEventsSettings) <- getPathsToNotificationsSettings teRTViewStateDir
  encodeFile pathToEventsSettings settings
