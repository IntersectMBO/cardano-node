{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cardano.TxSubmit.Web
  ( runTxSubmitServer
  ) where

import           Cardano.Api (AllegraEra, AnyCardanoEra (AnyCardanoEra), AsType (..),
                   CardanoEra (..), ConsensusModeParams (..), Error (..), FromSomeType (..),
                   HasTypeProxy (AsType), InAnyCardanoEra (..), InAnyShelleyBasedEra (..),
                   IsCardanoEra (..),
                   LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                   NetworkId, SerialiseAsCBOR (..), ShelleyBasedEra (..), ShelleyEra, SocketPath,
                   ToJSON, Tx, TxId (..), TxInMode (TxInMode), TxValidationErrorInCardanoMode (..),
                   getTxBody, getTxId, submitTxToNodeLocal)

import           Cardano.Binary (DecoderError (..))
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Logging.Trace (traceWith)
import qualified Cardano.Logging.Types as TraceD
import           Cardano.TxSubmit.Rest.Types (WebserverConfig (..), toWarpSettings)
import qualified Cardano.TxSubmit.Rest.Web as Web
import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi (..))
import           Cardano.TxSubmit.Types (EnvSocketError (..), RawCborDecodeError (..),
                   TxCmdError (..), TxSubmitApi, TxSubmitApiRecord (..),
                   TxSubmitWebApiError (TxSubmitFail), renderTxCmdError)
import           Cardano.TxSubmit.Util (logException)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Monad (Functor (fmap), Monad (return), (=<<))
import           Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT)
import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import           Data.Either (Either (..), partitionEithers)
import           Data.Function (($), (.))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (listToMaybe, maybe)
import           Data.Proxy (Proxy (..))
import           Data.Semigroup (Semigroup ((<>)))
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           System.Environment (lookupEnv)
import qualified System.IO as IO
import           System.IO (IO)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import           Text.Show (Show (show))

import qualified Servant
import           Servant (Application, Handler, ServerError (..), err400, throwError)
import           Servant.API.Generic (toServant)
import           Servant.Server.Generic (AsServerT)

runTxSubmitServer
  :: TraceD.Trace IO TraceSubmitApi
  -> WebserverConfig
  -> ConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> IO ()
runTxSubmitServer trace  webserverConfig protocol networkId socketPath = do
  logException trace "TxSubmit WebAPI: " $
    Web.runSettings trace  (toWarpSettings webserverConfig) $ txSubmitApp trace protocol networkId socketPath
  traceWith trace EndpointExiting

txSubmitApp
  :: TraceD.Trace IO TraceSubmitApi
  -> ConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> Application
txSubmitApp trace connectInfo networkId socketPath =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost trace connectInfo networkId socketPath
      }

deserialiseOne :: forall b. ()
  => FromSomeType SerialiseAsCBOR b
  -> ByteString
  -> Either DecoderError b
deserialiseOne (FromSomeType ttoken f) bs = f <$> deserialiseFromCBOR ttoken bs

deserialiseAnyOf :: forall b. ()
  => [FromSomeType SerialiseAsCBOR b]
  -> ByteString
  -> Either RawCborDecodeError b
deserialiseAnyOf ts te = getResult . partitionEithers $ fmap (`deserialiseOne` te) ts
  where
    getResult :: ([DecoderError], [b]) -> Either RawCborDecodeError b
    getResult (dErrors, []) = Left $ RawCborDecodeError dErrors
    getResult (_, result:_) = Right result -- take the first successful decode

readByteStringTx :: ByteString -> ExceptT TxCmdError IO (InAnyShelleyBasedEra Tx)
readByteStringTx = firstExceptT TxCmdTxReadError . hoistEither . deserialiseAnyOf
  [ FromSomeType (AsTx AsShelleyEra) (InAnyShelleyBasedEra ShelleyBasedEraShelley)
  , FromSomeType (AsTx AsAllegraEra) (InAnyShelleyBasedEra ShelleyBasedEraAllegra)
  , FromSomeType (AsTx AsMaryEra)    (InAnyShelleyBasedEra ShelleyBasedEraMary)
  , FromSomeType (AsTx AsAlonzoEra)  (InAnyShelleyBasedEra ShelleyBasedEraAlonzo)
  , FromSomeType (AsTx AsBabbageEra) (InAnyShelleyBasedEra ShelleyBasedEraBabbage)
  , FromSomeType (AsTx AsConwayEra)  (InAnyShelleyBasedEra ShelleyBasedEraConway)
  ]

txSubmitPost
  :: TraceD.Trace IO TraceSubmitApi
  -> ConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> ByteString
  -> Handler TxId
txSubmitPost trace p@(CardanoModeParams cModeParams) networkId socketPath txBytes =
  handle $ do
    InAnyShelleyBasedEra sbe tx <- readByteStringTx txBytes
    let txInMode = TxInMode sbe tx
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = p
                              , localNodeNetworkId = networkId
                              , localNodeSocketPath = socketPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> do
        liftIO $ T.putStrLn "Transaction successfully submitted."
        return $ getTxId (getTxBody tx)
      Net.Tx.SubmitFail e ->
        left $ TxCmdTxSubmitValidationError e
    where
      handle :: ExceptT TxCmdError IO TxId -> Handler TxId
      handle f = do
        result <- liftIO $ runExceptT f
        handleSubmitResult result

      errorResponse :: ToJSON e => e -> Handler a
      errorResponse e = throwError $ err400 { errBody = Aeson.encode e }

      -- Log relevant information, update the metrics, and return the result to
      -- the client.
      handleSubmitResult :: Either TxCmdError TxId -> Handler TxId
      handleSubmitResult res =
        case res of
          Left err -> do
            liftIO $ traceWith trace $ EndpointFailedToSubmitTransaction err
            errorResponse (TxSubmitFail err)
          Right txid -> do
            liftIO $ traceWith trace $ EndpointSubmittedTransaction txid
            pure txid
