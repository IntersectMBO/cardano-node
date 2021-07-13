{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.SubmitApi
  ( submitApi
  , submitApiSubmitTransaction
  ) where

import           Control.Monad
import           Data.Aeson (FromJSON (..), (.:))
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text)
import           GHC.Generics
import           System.IO (FilePath)
import           Text.Show

import qualified Data.Aeson as J
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use head" -}

submitApi :: H.SubmitApiConfig -> H.Integration Int
submitApi H.SubmitApiConfig {..} = do
  hStdout <- H.openFile stdoutFile IO.WriteMode
  hStderr <- H.openFile stderrFile IO.WriteMode

  H.note_ tempBaseAbsPath

  [port] <- H.noteShowIO $ IO.allocateRandomPorts 1

  void $ H.createProcess =<<
    ( H.procSubmitApi
      [ "--config", configFile
      , "--socket-path", IO.sprocketArgumentName sprocket
      , "--port", show @Int port
      , "--testnet-magic", show @Int testnetMagic
      ] <&>
      ( \cp -> cp
        { IO.std_in = IO.CreatePipe
        , IO.std_out = IO.UseHandle hStdout
        , IO.std_err = IO.UseHandle hStderr
        , IO.cwd = Just tempBaseAbsPath
        }
      )
    )

  return port

newtype ObjectWithCBorHexField = ObjectWithCBorHexField
  { cborHex :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ObjectWithCBorHexField where
  parseJSON = J.withObject "ObjectWithCBorHexField" $ \obj -> ObjectWithCBorHexField
    <$> obj .: "cborHex"

submitApiSubmitTransaction :: Int -> FilePath -> H.Integration LBS.ByteString
submitApiSubmitTransaction port txFile = GHC.withFrozenCallStack $ do
  txJson <- H.readJsonFile txFile & H.leftFailM

  objectWithCBorHexField <- J.fromJSON @ObjectWithCBorHexField txJson & H.jsonErrorFail

  bs <- Base16.decode (Text.encodeUtf8 (cborHex objectWithCBorHexField)) & H.leftFail

  let url = "http://localhost:" <> show @Int port <> "/api/submit/tx"

  manager <- H.evalIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- H.evalIO $ HTTP.parseRequest url <&> \r -> r
    { HTTP.method = "POST"
    , HTTP.requestBody = HTTP.RequestBodyLBS (LBS.fromStrict bs)
    , HTTP.requestHeaders =
      [ ("Content-Type", "application/cbor")
      ]
    }
  response <- H.evalIO $ HTTP.httpLbs request manager

  let status = HTTP.responseStatus response

  unless (HTTP.statusIsSuccessful status) $ do
    H.failWithCustom GHC.callStack Nothing (show status)

  H.note_ $ "Transaction ID: " <> Text.unpack (Text.decodeUtf8 (LBS.toStrict (HTTP.responseBody response)))

  return $ HTTP.responseBody response
