{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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

-- import Cardano.Api.Protocol
--     ( Protocol (..), withlocalNodeConnectInfo )
import Cardano.Api.TxBody ( TxId(..) )
import Cardano.Api
-- import Cardano.Api.Typed
--     ( NodeConsensusMode (..)
--     )
import           Data.Bifunctor (first)
import           Cardano.Prelude (putTextLn)
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
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                  hoistMaybe, left, newExceptT)
import Control.Monad.Except
import           System.Environment (lookupEnv)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Servant
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified Data.ByteString as BS
import qualified System.IO as IO

runTxSubmitServer
  :: Trace IO Text
  -> TxSubmitMetrics
  -> WebserverConfig
  -> AnyConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> IO ()
runTxSubmitServer trce metrics webserverConfig protocol networkId socketPath = do
  logException trce "tx-submit-webapi." $
    Web.runSettings (toWarpSettings webserverConfig) $ txSubmitApp trce metrics protocol networkId socketPath
  logInfo trce "txSubmitApp: exiting"

txSubmitApp
  :: Trace IO Text
  -> TxSubmitMetrics
  -> AnyConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> Application
txSubmitApp trce metrics connectInfo networkId socketPath =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost trce metrics connectInfo networkId socketPath
      }

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: ExceptT EnvSocketError IO SocketPath
readEnvSocketPath =
    maybe (left $ CliEnvVarLookup (T.pack envName)) (pure . SocketPath)
      =<< liftIO (lookupEnv envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"


readFileTextEnvelopeAnyOf2 :: [FromSomeType HasTextEnvelope b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf2 types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeAnyOf types te

readFileInAnyCardanoEra
  :: ( HasTextEnvelope (thing ByronEra)
     , HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> ExceptT TxCmdError IO (InAnyCardanoEra thing)
readFileInAnyCardanoEra asThing file =
    firstExceptT TxCmdReadTextViewFileError
  . newExceptT
  $ readFileTextEnvelopeAnyOf2
      [ FromSomeType (asThing AsByronEra)   (InAnyCardanoEra ByronEra)
      , FromSomeType (asThing AsShelleyEra) (InAnyCardanoEra ShelleyEra)
      , FromSomeType (asThing AsAllegraEra) (InAnyCardanoEra AllegraEra)
      , FromSomeType (asThing AsMaryEra)    (InAnyCardanoEra MaryEra)
      ]
      file

readFileTx :: FilePath -> ExceptT TxCmdError IO (InAnyCardanoEra Tx)
readFileTx = readFileInAnyCardanoEra AsTx

txSubmitPost
  :: Trace IO Text
  -> TxSubmitMetrics
  -> AnyConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> ByteString
  -> Handler TxId
txSubmitPost trce metrics (AnyConsensusModeParams cModeParams) networkId (SocketPath socketPath) txBytes = handle $ do
    let txFile = "blah" -- TODO fix this

    InAnyCardanoEra era tx <- readFileTx txFile -- TODO fix this
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (TxCmdEraConsensusModeMismatch txFile cMode (AnyCardanoEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = networkId
                              , localNodeSocketPath = socketPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> do
        liftIO $ putTextLn "Transaction successfully submitted."
        return $ getTxId (getTxBody tx) -- TODO return actual transaction id
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . TxCmdTxSubmitError . T.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatch mismatchErr
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
            -- liftIO $ logInfo trce $
            --   "txSubmitPost: failed to submit transaction: "
            --     <> renderTxSubmitError err
            errorResponse (TxSubmitFail err)
          Right txid -> do
            liftIO $ logInfo trce $
              "txSubmitPost: successfully submitted transaction "
                <> renderMediumTxId txid
            liftIO $ Gauge.inc (tsmCount metrics)
            pure txid

  -- liftIO $ logInfo trce $
  --   "txSubmitPost: received "
  --     <> textShow (B8.length txBytes)
  --     <> " bytes"
  -- case localNodeConsensusMode connectInfo of
  --   0 {- ByronMode{} -} ->
  --     case deserialiseFromCBOR AsByronTx txBytes of
  --       Left err -> handleDeserialiseErr err
  --       Right tx -> do
  --         resp <- liftIO $ submitTx connectInfo (Left tx)
  --         handleSubmitResult resp

  --   ShelleyMode{} ->
  --     case deserialiseFromCBOR AsShelleyTx txBytes of
  --       Left err -> handleDeserialiseErr err
  --       Right tx -> do
  --         resp <- liftIO $ submitTx connectInfo (Right tx)
  --         handleSubmitResult resp

  --   CardanoMode{} ->
  --     case tryDeserialiseByronOrShelleyTx txBytes of
  --       Left err -> handleDeserialiseErr err
  --       Right byronOrShelleyTx -> do
  --         resp <- liftIO $ submitTx connectInfo byronOrShelleyTx
  --         handleSubmitResult resp
  -- where

  --   -- Log relevant information and return an appropriate HTTP 400 response to
  --   -- the client.
  --   handleDeserialiseErr :: DecoderError -> Handler a
  --   handleDeserialiseErr err = do
  --     let serr = if
  --                 | B8.length txBytes == 0 -> TxSubmitEmpty
  --                 | B8.all isHexDigitOrSpace txBytes -> TxSubmitDecodeHex
  --                 | otherwise -> TxSubmitDecodeFail err
  --     liftIO $ logInfo trce $
  --       "txSubmitPost: failed to deserialise transaction: "
  --         <> renderTxSubmitWebApiError serr
  --     errorResponse serr


  --   -- Attempt to deserialise either a Byron or Shelley transaction.
  --   -- Note that, if this fails, the 'DecoderError' from the Shelley
  --   -- transaction will be returned and the one from the Byron transaction
  --   -- will be discarded.
  --   tryDeserialiseByronOrShelleyTx
  --     :: ByteString
  --     -> Either DecoderError (Either (Tx ByronEra) (Tx ShelleyEra))
  --   tryDeserialiseByronOrShelleyTx bs =
  --     case deserialiseFromCBOR AsByronTx bs of
  --       Left _ -> Right <$> deserialiseFromCBOR AsShelleyTx bs
  --       Right byronTx -> Right $ Left byronTx

-- -- | Whether the provided 'Char' is a hex character or a space.
-- isHexDigitOrSpace :: Char -> Bool
-- isHexDigitOrSpace c = Char.isHexDigit c || Char.isSpace c

-- | Render the first 16 characters of a transaction ID.
renderMediumTxId :: TxId -> Text
renderMediumTxId (TxId hash) = renderMediumHash hash

-- | Render the first 16 characters of a hex-encoded hash.
renderMediumHash :: Crypto.Hash crypto a -> Text
renderMediumHash = T.take 16 . T.decodeLatin1 . Crypto.hashToBytesAsHex
