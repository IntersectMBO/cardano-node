{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Tracing.Message (TraceSubmitApi(..)) where

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

data TraceSubmitApi = ApplicationStopping
                    | EndpointListeningOnPort SockAddr -- TODO addr as field name
                    | EndpointException Text SomeException
                    | EndpointFailedToSubmitTransaction TxCmdError
                    | EndpointSubmittedTransaction TxId
                    | EndpointExiting
                    | MetricsServerStarted Int
                    | MetricsServerError IOException
                    | MetricsServerPortOccupied Int
                    | MetricsServerPortNotBound Int {- tried ports until that one -}

instance LogFormatting TraceSubmitApi where
  -- TODO (from: @russoul, to: @jutaro) why json object is required instead of, more flexible, arbitrary json value?
  -- TODO do as in tx-generator. Don't include `kind` field
  forMachine _ x = singleton "log" (toJSON (forHuman x))

  forHuman (MetricsServerStarted port) =
    "Starting metrics server on port " <> textShow port
  forHuman (EndpointException title except) =
    title <> textShow except
  forHuman (MetricsServerError txt) =
    pack $ "Metrics server error: " <> displayException txt
  forHuman (MetricsServerPortOccupied port) =
    "Could not allocate metrics server port " <> textShow port <> " - trying next available..."
  forHuman (MetricsServerPortNotBound untilPort) =
    "Could not allocate any metrics port until " <> textShow untilPort <> " - metrics endpoint disabled"
  forHuman ApplicationStopping =
    "runTxSubmitWebapi: Stopping TxSubmit API"
  forHuman (EndpointListeningOnPort port) =
    "Web API listening on port " <> textShow port
  forHuman EndpointExiting =
    "txSubmitApp: exiting"
  forHuman (EndpointFailedToSubmitTransaction err) =
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
  forHuman (EndpointSubmittedTransaction txId) =
    "txSubmitPost: successfully submitted transaction " <> renderMediumTxId txId where

      -- | Render the first 16 characters of a transaction ID.
      renderMediumTxId :: TxId -> Text
      renderMediumTxId (TxId hash) = renderMediumHash hash

      -- | Render the first 16 characters of a hex-encoded hash.
      renderMediumHash :: Crypto.Hash crypto a -> Text
      renderMediumHash = take 16 . decodeLatin1 . Crypto.hashToBytesAsHex

instance MetaTrace TraceSubmitApi where
  allNamespaces = [
      Namespace [] ["Application", "Stopping"],

      Namespace [] ["Endpoint", "ListeningOnPort"],
      Namespace [] ["Endpoint", "Exception"],
      Namespace [] ["Endpoint", "FailedToSubmitTransaction"],
      Namespace [] ["Endpoint", "SubmittedTransaction"],
      Namespace [] ["Endpoint", "Exiting"],

      Namespace [] ["Metrics", "Started"],
      Namespace [] ["Metrics", "Error"],
      Namespace [] ["Metrics", "PortOccupied"],
      Namespace [] ["Metrics", "PortNotBound"]

    ]

  namespaceFor ApplicationStopping                   = Namespace [] ["Application", "Stopping"]
  namespaceFor (EndpointListeningOnPort _)           = Namespace [] ["Endpoint", "ListeningOnPort"]
  namespaceFor (EndpointException _ _)               = Namespace [] ["Endpoint", "Exception"]
  namespaceFor (EndpointFailedToSubmitTransaction _) = Namespace [] ["Endpoint", "FailedToSubmitTransaction"]
  namespaceFor (EndpointSubmittedTransaction _)      = Namespace [] ["Endpoint", "SubmittedTransaction"]
  namespaceFor EndpointExiting                       = Namespace [] ["Endpoint", "Exiting"]
  namespaceFor (MetricsServerStarted _)              = Namespace [] ["Metrics", "Started"]
  namespaceFor (MetricsServerError _)                = Namespace [] ["Metrics", "Error"]
  namespaceFor (MetricsServerPortOccupied _)         = Namespace [] ["Metrics", "PortOccupied"]
  namespaceFor (MetricsServerPortNotBound _)         = Namespace [] ["Metrics", "PortNotBound"]

  severityFor (Namespace _ ["Application", "Stopping"]) _               = Just Info
  severityFor (Namespace _ ["Endpoint", "ListeningOnPort"]) _           = Just Info
  severityFor (Namespace _ ["Endpoint", "Exception"]) _                 = Just Error
  severityFor (Namespace _ ["Endpoint", "Exiting"]) _                   = Just Info
  severityFor (Namespace _ ["Endpoint", "FailedToSubmitTransaction"]) _ = Just Info
  severityFor (Namespace _ ["Endpoint", "SubmittedTransaction"]) _      = Just Info
  severityFor (Namespace _ ["Metrics", "Started"]) _                    = Just Info
  severityFor (Namespace _ ["Metrics", "Error"]) _                      = Just Warning
  severityFor (Namespace _ ["Metrics", "PortOccupied"]) _               = Just Warning
  severityFor (Namespace _ ["Metrics", "PortNotBound"]) _               = Just Error
  severityFor _ _                                                       = Nothing

  metricsDocFor _ = []

  documentFor _ = Nothing
