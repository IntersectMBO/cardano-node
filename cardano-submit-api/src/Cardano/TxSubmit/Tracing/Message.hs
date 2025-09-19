{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Tracing.Message (Message(..)) where

import           Cardano.Api (TxId (TxId), TxValidationErrorInCardanoMode (..))
import           Cardano.Api.Pretty (textShow)

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Logging.Types
import           Cardano.TxSubmit.Types (TxCmdError (..))

import           Prelude hiding (take)

import           Data.Aeson (toJSON)
import           Data.Aeson.KeyMap (singleton)
import           Data.Text (Text, pack, take)
import           Data.Text.Encoding (decodeLatin1)
import           GHC.Exception.Type (SomeException, displayException)
import           GHC.IO.Exception (IOException)
import           Network.Socket (SockAddr)

-- TODO: (@russoul) Evaluate the Message structure.
data Message = ServerStopped
             | ServerListeningOnPort SockAddr
             | AppExiting
             | MetricsServerStarted Int
             | MetricsServerError IOException
             | MetricsServerPortOccupied Int
             | MetricsServerPortNotBound Int {- tried ports until that one -}
             | Exception Text SomeException
             | FailedToSubmitTransaction TxCmdError
             | SubmittedTransaction TxId

-- TODO: (@russoul) Implement this.
instance LogFormatting Message where
  -- TODO (@russoul) why json object is required instead of arbitrary json value?
  forMachine _ x = singleton "log" (toJSON (forHuman x))

  forHuman (MetricsServerStarted port) = "Starting metrics server on port " <> textShow port
  forHuman (Exception title except) = title <> textShow except
  forHuman (MetricsServerError txt) = pack $ "Metrics server error: " <> displayException txt
  forHuman (MetricsServerPortOccupied port) =
    "Could not allocate metrics server port " <> textShow port <> " - trying next available..."
  forHuman (MetricsServerPortNotBound untilPort) =
    "Could not allocate any metrics port until " <> textShow untilPort <> " - metrics endpoint disabled"
  forHuman ServerStopped = "runTxSubmitWebapi: Stopping TxSubmit API"
  forHuman (ServerListeningOnPort port) = "Web API listening on port " <> textShow port
  forHuman AppExiting = "txSubmitApp: exiting"
  forHuman (FailedToSubmitTransaction err) =
    "txSubmitPost: failed to submit transaction: " <> renderTxCmdError where

      renderTxCmdError :: Text
      renderTxCmdError = case err of
        TxCmdSocketEnvError socketError ->
          "socket env error " <> textShow socketError
        TxCmdTxReadError envelopeError ->
          "transaction read error " <> textShow envelopeError
        TxCmdTxSubmitValidationError e ->
          case e of
            TxValidationErrorInCardanoMode validationErr -> "transaction submit error " <> textShow validationErr
            TxValidationEraMismatch eraMismatch -> "transaction submit era mismatch" <> textShow eraMismatch
  forHuman (SubmittedTransaction txId) =
    "txSubmitPost: successfully submitted transaction " <> renderMediumTxId txId where

      -- | Render the first 16 characters of a transaction ID.
      renderMediumTxId :: TxId -> Text
      renderMediumTxId (TxId hash) = renderMediumHash hash

      -- | Render the first 16 characters of a hex-encoded hash.
      renderMediumHash :: Crypto.Hash crypto a -> Text
      renderMediumHash = take 16 . decodeLatin1 . Crypto.hashToBytesAsHex

-- TODO: @russoul
instance MetaTrace Message where
  namespaceFor ServerStopped = Namespace [] ["TxSubmit", "ServerStopped"]
  namespaceFor (ServerListeningOnPort _) = Namespace [] ["TxSubmit", "ServerListeningOnPort"]
  namespaceFor (Exception _ _) = Namespace [] ["TxSubmit", "Exception"]
  namespaceFor (MetricsServerStarted _) = Namespace [] ["TxSubmit", "Metrics", "Started"]
  namespaceFor (MetricsServerError _) = Namespace [] ["TxSubmit", "Metrics", "Error"]
  namespaceFor (MetricsServerPortOccupied _) = Namespace [] ["TxSubmit", "Metrics", "PortOccupied"]
  namespaceFor (MetricsServerPortNotBound _) = Namespace [] ["TxSubmit", "Metrics", "PortNotBound"]
  namespaceFor AppExiting = Namespace [] ["TxSubmit", "AppExiting"]
  namespaceFor (FailedToSubmitTransaction _) = Namespace [] ["TxSubmit", "FailedToSubmitTransaction"]
  namespaceFor (SubmittedTransaction _) = Namespace [] ["TxSubmit", "SubmittedTransaction"]
  severityFor (Namespace _ ["TxSubmit", "ServerStopped"]) _ = Just Info
  severityFor (Namespace _ ["TxSubmit", "ServerListeningOnPort"]) _ = Just Info
  severityFor (Namespace _ ["TxSubmit", "Exception"]) _ = Just Error
  severityFor (Namespace _ ["TxSubmit", "Metrics", "Started"]) _ = Just Info
  severityFor (Namespace _ ["TxSubmit", "Metrics", "Error"]) _ = Just Warning
  severityFor (Namespace _ ["TxSubmit", "Metrics", "PortOccupied"]) _ = Just Warning
  severityFor (Namespace _ ["TxSubmit", "Metrics", "PortNotBound"]) _ = Just Error
  severityFor (Namespace _ ["TxSubmit", "AppExiting"]) _ = Just Info
  severityFor (Namespace _ ["TxSubmit", "FailedToSubmitTransaction"]) _ = Just Info
  severityFor (Namespace _ ["TxSubmit", "SubmittedTransaction"]) _ = Just Info
  severityFor _ _ = Nothing
  metricsDocFor _ = []
  documentFor _ = Nothing
  allNamespaces = [
    Namespace [] ["TxSubmit", "ServerStopped"],
    Namespace [] ["TxSubmit", "ServerListeningOnPort"],
    Namespace [] ["TxSubmit", "Exception"],
    Namespace [] ["TxSubmit", "Metrics", "Started"],
    Namespace [] ["TxSubmit", "Metrics", "Error"],
    Namespace [] ["TxSubmit", "Metrics", "PortOccupied"],
    Namespace [] ["TxSubmit", "Metrics", "PortNotBound"],
    Namespace [] ["TxSubmit", "AppExiting"],
    Namespace [] ["TxSubmit", "FailedToSubmitTransaction"],
    Namespace [] ["TxSubmit", "SubmittedTransaction"]
   ]

