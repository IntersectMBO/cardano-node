{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi(..)) where

import           Cardano.Api (TxId (TxId), TxValidationErrorInCardanoMode (..))
import           Cardano.Api.Pretty (textShow)

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Logging
import           Cardano.TxSubmit.Types (TxCmdError (..))

import           Prelude hiding (take)

import           Data.Aeson (Value (String), toJSON)
import           Data.Aeson.KeyMap (singleton)
import           Data.Aeson.Types ((.=))
import           Data.Text (Text, pack, take)
import           Data.Text.Encoding (decodeLatin1)
import           GHC.Exception.Type (SomeException, displayException)
import           GHC.IO.Exception (IOException)
import           Network.Socket (SockAddr)

data TraceSubmitApi = ApplicationStopping
                    | ApplicationInitializeMetrics
                    | EndpointListeningOnPort SockAddr
                    | EndpointException Text SomeException
                    | EndpointFailedToSubmitTransaction TxCmdError
                    | EndpointSubmittedTransaction TxId
                    | EndpointExiting
                    | MetricsServerStarted Int
                    | MetricsServerError IOException
                    | MetricsServerPortOccupied Int
                    | MetricsServerPortNotBound Int {- tried ports until that one -}


-- | Render the first 16 characters of a transaction ID.
renderMediumTxId :: TxId -> Text
renderMediumTxId (TxId hash) = renderMediumHash hash

-- | Render the first 16 characters of a hex-encoded hash.
renderMediumHash :: Crypto.Hash crypto a -> Text
renderMediumHash = take 16 . decodeLatin1 . Crypto.hashToBytesAsHex

renderTxCmdError :: TxCmdError -> Text
renderTxCmdError (TxCmdSocketEnvError socketError) =
  "socket env error " <> textShow socketError
renderTxCmdError (TxCmdTxReadError envelopeError) =
  "transaction read error " <> textShow envelopeError
renderTxCmdError (TxCmdTxSubmitValidationError e) =
  case e of
    TxValidationErrorInCardanoMode validationErr ->
      "transaction submit error " <> textShow validationErr
    TxValidationEraMismatch eraMismatch ->
      "transaction submit era mismatch" <> textShow eraMismatch

instance LogFormatting TraceSubmitApi where
  -- TODO (from: @russoul, to: @jutaro) why json object is required instead of, more flexible, arbitrary json value?
  forMachine _ ApplicationStopping = mempty
  forMachine _ (EndpointListeningOnPort addr) =
    singleton "addr" (toJSON (textShow addr))
  forMachine _ (EndpointException txt except) = mconcat
    [
      "txt" .= String txt,
      "exception" .= String (textShow except)
    ]
  forMachine _ (EndpointFailedToSubmitTransaction txCmdError) =
    singleton "error" (toJSON (renderTxCmdError txCmdError))
  forMachine _ (EndpointSubmittedTransaction txId) =
    singleton "txId" (String $ renderMediumTxId txId)
  forMachine _ EndpointExiting = mempty
  forMachine _ (MetricsServerStarted port) =
    singleton "port" (toJSON port)
  forMachine _ (MetricsServerError except) =
    singleton "exception" (toJSON $ displayException except)
  forMachine _ (MetricsServerPortOccupied port) =
    singleton "port" (toJSON port)
  forMachine _ (MetricsServerPortNotBound port) =
    singleton "port" (toJSON port)
  forMachine _ ApplicationInitializeMetrics = mempty

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
  forHuman ApplicationInitializeMetrics = "Metrics initialized"
  forHuman (EndpointListeningOnPort port) =
    "Web API listening on port " <> textShow port
  forHuman EndpointExiting =
    "txSubmitApp: exiting"
  forHuman (EndpointFailedToSubmitTransaction err) =
    "txSubmitPost: failed to submit transaction: " <> renderTxCmdError err
  forHuman (EndpointSubmittedTransaction txId) =
    "txSubmitPost: successfully submitted transaction " <> renderMediumTxId txId

  asMetrics (EndpointFailedToSubmitTransaction _) = [CounterM "tx_submit_fail" Nothing]
  asMetrics (EndpointSubmittedTransaction      _) = [CounterM "tx_submit"      Nothing]
  asMetrics ApplicationInitializeMetrics          = [CounterM "tx_submit_fail" (Just 0), CounterM "tx_submit" (Just 0)]
  asMetrics _                                     = []

instance MetaTrace TraceSubmitApi where
  allNamespaces = [
      Namespace [] ["Application", "Stopping"],
      Namespace [] ["Application", "InitializeMetrics"],

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
  namespaceFor ApplicationInitializeMetrics          = Namespace [] ["Application", "InitializeMetrics"]
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
  severityFor (Namespace _ ["Application", "InitializeMetrics"]) _      = Just Debug
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

  -- TODO (@russoul) This seems to be necessary for metrics to work at all, why?
  metricsDocFor (Namespace _ ["Endpoint", "FailedToSubmitTransaction"]) = [ ("tx_submit_fail", "Number of failed tx submissions") ]
  metricsDocFor (Namespace _ ["Endpoint", "SubmittedTransaction"]) = [ ("tx_submit", "Number of successful tx submissions") ]
  metricsDocFor (Namespace _ ["Application", "InitializeMetrics"]) =
    [
      ("tx_submit_fail", "Initialize and set the number of successful tx submissions to 0"),
      ("tx_submit", "Initialize and set the number of successful tx submissions to 0")
    ]
  metricsDocFor _ = []

  documentFor _ = Nothing
