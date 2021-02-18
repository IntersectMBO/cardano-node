{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Web
  ( runTxSubmitServer
  ) where

import Cardano.TxSubmit.Rest.Web as Web
import Cardano.TxSubmit.CLI.Types
import Cardano.TxSubmit.JsonOrphans
    ()
import Cardano.TxSubmit.Metrics
    ( TxSubmitMetrics (..) )
import Cardano.TxSubmit.Tx
import Cardano.TxSubmit.Types
import Cardano.TxSubmit.Util

import Cardano.Api.Protocol
    ( Protocol (..), withlocalNodeConnectInfo )
import Cardano.Api.TxBody ( TxId(..) )
import Cardano.Api
    ( AsType(AsShelleyTx, AsByronTx),
      ByronEra,
      ShelleyEra,
      SerialiseAsCBOR(deserialiseFromCBOR),
      NetworkId,
      LocalNodeConnectInfo(localNodeConsensusMode),
      Tx )
import Cardano.Api.Typed
    ( NodeConsensusMode (..)
    )
import Cardano.Binary
    ( DecoderError )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.TxSubmit.Rest.Types
    ( WebserverConfig (..), toWarpSettings )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( ToJSON (..) )
import Data.ByteString.Char8
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Servant
    ( Application, Handler, ServerError (..), err400, throwError )
import Servant.API.Generic
    ( toServant )
import Servant.Server.Generic
    ( AsServerT )

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Servant
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

runTxSubmitServer
  :: Trace IO Text
  -> TxSubmitMetrics
  -> WebserverConfig
  -> Protocol
  -> NetworkId
  -> SocketPath
  -> IO ()
runTxSubmitServer trce metrics webserverConfig protocol networkId (SocketPath sockPath) = do
  logException trce "tx-submit-webapi." $
    Web.runSettings (toWarpSettings webserverConfig) (withlocalNodeConnectInfo protocol networkId sockPath $ txSubmitApp trce metrics)
  logInfo trce "txSubmitApp: exiting"

txSubmitApp
  :: Trace IO Text
  -> TxSubmitMetrics
  -> LocalNodeConnectInfo mode blk
  -> Application
txSubmitApp trce metrics connectInfo =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost trce metrics connectInfo
      }

txSubmitPost
  :: Trace IO Text
  -> TxSubmitMetrics
  -> LocalNodeConnectInfo mode blk
  -> ByteString
  -> Handler TxId
txSubmitPost trce metrics connectInfo txBytes = do
  liftIO $ logInfo trce $
    "txSubmitPost: received "
      <> textShow (B8.length txBytes)
      <> " bytes"
  case localNodeConsensusMode connectInfo of
    ByronMode{} ->
      case deserialiseFromCBOR AsByronTx txBytes of
        Left err -> handleDeserialiseErr err
        Right tx -> do
          resp <- liftIO $ submitTx connectInfo (Left tx)
          handleSubmitResult resp

    ShelleyMode{} ->
      case deserialiseFromCBOR AsShelleyTx txBytes of
        Left err -> handleDeserialiseErr err
        Right tx -> do
          resp <- liftIO $ submitTx connectInfo (Right tx)
          handleSubmitResult resp

    CardanoMode{} ->
      case tryDeserialiseByronOrShelleyTx txBytes of
        Left err -> handleDeserialiseErr err
        Right byronOrShelleyTx -> do
          resp <- liftIO $ submitTx connectInfo byronOrShelleyTx
          handleSubmitResult resp
  where
    errorResponse :: ToJSON e => e -> Handler a
    errorResponse e = throwError $ err400 { errBody = Aeson.encode e }

    -- Log relevant information and return an appropriate HTTP 400 response to
    -- the client.
    handleDeserialiseErr :: DecoderError -> Handler a
    handleDeserialiseErr err = do
      let serr = if
                  | B8.length txBytes == 0 -> TxSubmitEmpty
                  | B8.all isHexDigitOrSpace txBytes -> TxSubmitDecodeHex
                  | otherwise -> TxSubmitDecodeFail err
      liftIO $ logInfo trce $
        "txSubmitPost: failed to deserialise transaction: "
          <> renderTxSubmitWebApiError serr
      errorResponse serr

    -- Log relevant information, update the metrics, and return the result to
    -- the client.
    handleSubmitResult :: Either TxSubmitError TxId -> Handler TxId
    handleSubmitResult res =
      case res of
        Left err -> do
          liftIO $ logInfo trce $
            "txSubmitPost: failed to submit transaction: "
              <> renderTxSubmitError err
          errorResponse (TxSubmitFail err)
        Right txid -> do
          liftIO $ logInfo trce $
            "txSubmitPost: successfully submitted transaction "
              <> renderMediumTxId txid
          liftIO $ Gauge.inc (tsmCount metrics)
          pure txid

    -- Attempt to deserialise either a Byron or Shelley transaction.
    -- Note that, if this fails, the 'DecoderError' from the Shelley
    -- transaction will be returned and the one from the Byron transaction
    -- will be discarded.
    tryDeserialiseByronOrShelleyTx
      :: ByteString
      -> Either DecoderError (Either (Tx ByronEra) (Tx ShelleyEra))
    tryDeserialiseByronOrShelleyTx bs =
      case deserialiseFromCBOR AsByronTx bs of
        Left _ -> Right <$> deserialiseFromCBOR AsShelleyTx bs
        Right byronTx -> Right $ Left byronTx

-- | Whether the provided 'Char' is a hex character or a space.
isHexDigitOrSpace :: Char -> Bool
isHexDigitOrSpace c = Char.isHexDigit c || Char.isSpace c

-- | Render the first 16 characters of a transaction ID.
renderMediumTxId :: TxId -> Text
renderMediumTxId (TxId hash) = renderMediumHash hash

-- | Render the first 16 characters of a hex-encoded hash.
renderMediumHash :: Crypto.Hash crypto a -> Text
renderMediumHash = T.take 16 . T.decodeLatin1 . Crypto.hashToBytesAsHex
