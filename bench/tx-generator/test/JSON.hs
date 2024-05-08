{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import Control.Monad

import Data.Aeson                     as AE
import Data.Aeson.Decoding            as AED
import Data.Aeson.Decoding.Tokens     as AED
import Data.Aeson.Decoding.ByteString as AEB
import Data.Aeson.Types               as AET
import Data.ByteString                as BS
import Data.Functor.Adjunction        as Adj
import Data.String                    as Str
import Data.Tuple.Extra               as Tup

import Cardano.Node.Configuration.NodeAddress
import Cardano.TxGenerator.Setup.NixService

main :: IO ()
main = pure ()

valueToNDParser :: Value -> Parser NodeDescription
valueToNDParser = withObject "NodeDescription" \v -> do
  unNodeHostIPv4Address <- v .: "addr" <?> Key "addr"
  naPort <- fmap toEnum $ v .: "port" <?> Key "port"
  let naHostAddress = NodeHostIPv4Address {..}
      ndAddr        = NodeAddress {..}
  ndName <- v .:? "name" <?> Key "name" .!= show ndAddr
  pure $ NodeDescription {..}

tokens :: Tokens ByteString String
tokens = bsToTokens nodeDescStr

nodeDescStr :: IsString s => s
nodeDescStr = "{ \"name\": \"node-0\",\
               \ \"addr\": \"127.0.0.1\",\
               \ \"port\": 7878 }"

eitherValue :: Either String (Value, ByteString)
eitherValue = toEitherValue tokens

runJSONParser, runJSONParser' :: ByteString -> (Value -> Parser t) -> Either String (t, ByteString)
runJSONParser jsonBS valueToParser = join .
  flip parseEither valueToParser . (uncozipL .) $
                  \f -> (pure +++ firstM f) jsonEitherValue
  where
    jsonEitherValue :: Either String (Value, ByteString)
    jsonEitherValue = toEitherValue $ bsToTokens jsonBS

runJSONParser' jsonBS valueToParser = do
  (jsonValue, restBS) <- toEitherValue $ bsToTokens jsonBS
  marshalledValue <- parseEither valueToParser jsonValue
  pure (marshalledValue, restBS)

caf2 :: Either String (NodeDescription, ByteString)
caf2 = join . flip parseEither valueToNDParser $
           \f -> uncozipL $ (pure +++ firstM f) eitherValue

caf1 :: Either String (NodeDescription, ByteString)
caf1 = join $ parseEither ((\f -> uncozipL $ (pure +++ (firstM f)) (toEitherValue $ bsToTokens "{ \"name\": \"node-0\", \"addr\": \"127.0.0.1\", \"port\": 7878 }"))) (withObject "NodeDescription" \v -> do { unNodeHostIPv4Address <- v .: "addr" <?> Key "addr" ; naPort <- fmap toEnum $ v .: "port" <?> Key "port" ; let { naHostAddress = NodeHostIPv4Address {..} ; ndAddr = NodeAddress {..} } ; ndName <- v .:? "name" <?> Key "name" .!= show ndAddr ; pure $ NodeDescription {..} })

caf3 :: Either String (NodeDescription, ByteString)
caf3 = join . flip parseEither valueToNDParser $ uncozipL . flip (pure +++) eitherValue . firstM

caf4 :: Either String (NodeDescription, ByteString)
caf4 = join . flip parseEither valueToNDParser . (uncozipL .) $ flip (pure +++) eitherValue . firstM

caf5 :: Either String (NodeDescription, ByteString)
caf5 = join . flip parseEither valueToNDParser . (uncozipL .) $ \f -> (flip (pure +++) eitherValue) (firstM f)

caf6 :: Either String (NodeDescription, ByteString)
caf6 = join . flip parseEither valueToNDParser . (uncozipL .) $ \f -> (pure +++ firstM f) eitherValue
