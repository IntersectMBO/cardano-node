{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Email
  ( StatusMessage
  , createAndSendEmail
  , createAndSendTestEmail
  , statusIsOK
  ) where

import           Cardano.Tracer.Handlers.RTView.Notifications.Types

import           Control.Concurrent.Async (race)
import           Control.Exception.Extra (try_)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Network.Mail.Mime (Address (..), Mail (..), simpleMail')
import qualified Network.Mail.SMTP as SMTP
import           System.Time.Extra (sleep)

type StatusMessage = Text

createAndSendEmail
  :: EmailSettings
  -> Text
  -> IO StatusMessage
createAndSendEmail settings@EmailSettings {esEmailTo, esEmailFrom, esSubject} bodyMessage =
  sendEmail settings $ simpleMail' to from esSubject body
 where
  to   = Address Nothing esEmailTo
  from = Address (Just "Cardano RTView") esEmailFrom
  body = LT.fromStrict bodyMessage

createAndSendTestEmail
  :: EmailSettings
  -> IO StatusMessage
createAndSendTestEmail settings = createAndSendEmail settings body
 where
  body = "This is a test notification from Cardano RTView. Congrats: your email settings are correct!"

sendEmail
  :: EmailSettings
  -> Mail
  -> IO StatusMessage
sendEmail EmailSettings {esSMTPHost, esSMTPPort, esUsername, esPassword, esSSL} mail =
  -- Unfortunately, 'sender' function may freeze because of problem with email settings
  -- (for example, wrong SMTP port). In this case we need a watchdog-pattern to avoid
  -- too long execution time.
  runIOWithWatchdog 10.0 "✗ Unable to send: timeout" $
    try_ (sender host port user pass mail) >>= \case
      Left e  -> return $ "✗ Unable to send: " <> explanation e
      Right _ -> return "✓ Yay! Notification is sent."
 where
  sender = case esSSL of
             TLS      -> SMTP.sendMailWithLoginTLS'
             STARTTLS -> SMTP.sendMailWithLoginSTARTTLS'
             NoSSL    -> SMTP.sendMailWithLogin'
  host = T.unpack esSMTPHost
  port = fromIntegral esSMTPPort
  user = T.unpack esUsername
  pass = T.unpack esPassword

  explanation e
    | "getAddrInfo" `T.isInfixOf` msg = "check SMTP host"
    | "user error"  `T.isInfixOf` msg = "check your name, password or SSL"
    | otherwise = msg
   where
    msg = T.pack (show e)

statusIsOK :: StatusMessage -> Bool
statusIsOK msg = "Yay" `T.isInfixOf` msg

runIOWithWatchdog
  :: Double
  -> a
  -> IO a
  -> IO a
runIOWithWatchdog howLongToWaitInS timeIsUpMsg ioAction =
  race (sleep howLongToWaitInS) ioAction >>= \case
    Left _             -> return timeIsUpMsg
    Right actionResult -> return actionResult
