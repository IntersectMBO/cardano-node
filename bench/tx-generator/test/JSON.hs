{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wno-unused-imports #-}

module Main where

import           Cardano.Node.Configuration.NodeAddress
import           Cardano.TxGenerator.Setup.NixService

-- It would have been nice if stylish-haskell could have kept the
-- aliases aligned so "as" happens in the same column for each import.
import           Control.Applicative            as App
import           Control.Arrow                  as Arr
import           Control.Monad                  as Mon
import           Data.Aeson                     as AE
import           Data.Aeson.Decoding            as AED
import           Data.Aeson.Decoding.ByteString as AEB
import           Data.Aeson.Decoding.Tokens     as AED
import           Data.Aeson.Types               as AET
import           Data.ByteString                as BS
import           Data.Functor.Adjunction        as Adj
import           Data.String                    as Str
import           Data.Tuple.Extra               as Tup

main :: IO ()
main = pure ()

nodeDescTokens :: Tokens ByteString String
nodeDescTokens = bsToTokens nodeDescStr

-- An example of a NodeDescription these things were being used
-- to prototype parsing.
nodeDescStr :: IsString s => s
nodeDescStr = "{ \"name\": \"node-0\",\
               \ \"addr\": \"127.0.0.1\",\
               \ \"port\": 7878 }"

eitherValue :: Either String (Value, ByteString)
eitherValue = toEitherValue nodeDescTokens

runJSONParser, runJSONParser', runJSONParser'' ::
  ByteString -> (Value -> Parser t) -> Either String (t, ByteString)

-- This adapts caf2 to make the escaping variables into function
-- parameters. I still wasn't happy with it, so I went the complete
-- opposite direction for the next attempt.
runJSONParser jsonBS valueToParser = join .
  flip parseEither valueToParser . (uncozipL .) $
                  \f -> (pure +++ firstM f) jsonEitherValue
  where
    jsonEitherValue :: Either String (Value, ByteString)
    jsonEitherValue = toEitherValue $ bsToTokens jsonBS

-- This is something of an attempt to be very, very basic in the hopes
-- of maximum maintainability.
runJSONParser' jsonBS valueToParser = do
  (jsonValue, restBS) <- toEitherValue $ bsToTokens jsonBS
  marshalledValue <- parseEither valueToParser jsonValue
  pure (marshalledValue, restBS)

-- Baldur had by far the best golfing of this, though he wrote it
-- flipped. The verbatim original was:
-- runPar :: (Value -> Parser a) -> BS.ByteString -> Either String (a, BS.ByteString)
-- runPar a b = join $ flip parseEither a \f ->
--   uncozipL do pure +++ firstM f $ toEitherValue (bsToTokens b)
runJSONParser'' jsonBS valueToParser =
  join $ flip parseEither valueToParser \f -> uncozipL do
    pure +++ firstM f $ toEitherValue $ bsToTokens jsonBS

{-
-- This stuff is the case where my efforts to write a FromJSON
-- instance originally came from and maybe some of the development
-- process. It might be illustrative of how to use this, in a way.

valueToNDParser :: Value -> Parser NodeDescription
valueToNDParser = withObject "NodeDescription" \v -> do
  unNodeHostIPv4Address <- v .: "addr" <?> Key "addr"
  naPort <- fmap toEnum $ v .: "port" <?> Key "port"
  let naHostAddress = NodeHostIPv4Address {..}
      ndAddr        = NodeAddress {..}
  ndName <- v .:? "name" <?> Key "name" .!= show ndAddr
  pure $ NodeDescription {..}

-- Most of the experimentation further down the line ended up not
-- improving the comprehensibility, so this was kept as the very most
-- basic pulling out of the JSON string and lambda creating the parser.
-- That said, Baldur's above in runJSONParser'' is both more
-- comprehensible and more stylish.
caf2 :: Either String (NodeDescription, ByteString)
caf2 = join . flip parseEither valueToNDParser $
           \f -> uncozipL $ (pure +++ firstM f) eitherValue

-- This was a reconstruction of the first cut, cut and pasted directly
-- from the repl.
caf1 :: Either String (NodeDescription, ByteString)
caf1 = join $ parseEither ((\f -> uncozipL $ (pure +++ (firstM f)) (toEitherValue $ bsToTokens "{ \"name\": \"node-0\", \"addr\": \"127.0.0.1\", \"port\": 7878 }"))) (withObject "NodeDescription" \v -> do { unNodeHostIPv4Address <- v .: "addr" <?> Key "addr" ; naPort <- fmap toEnum $ v .: "port" <?> Key "port" ; let { naHostAddress = NodeHostIPv4Address {..} ; ndAddr = NodeAddress {..} } ; ndName <- v .:? "name" <?> Key "name" .!= show ndAddr ; pure $ NodeDescription {..} })

-- This pulls out the JSON string to parse and the lambda yielding the
-- parser to attempt to run. It's also pointfree. It's also hard for
-- people not immersed in it all to understand.
caf3 :: Either String (NodeDescription, ByteString)
caf3 = join . flip parseEither valueToNDParser $ uncozipL . flip (pure +++) eitherValue . firstM

-- This pulls uncozipL out in front of the $ in an attempt to
-- incrementally make the pointfree version more comprehensible.
caf4 :: Either String (NodeDescription, ByteString)
caf4 = join . flip parseEither valueToNDParser . (uncozipL .) $ flip (pure +++) eitherValue . firstM

-- This makes a lambda variable for more comprehensibility vs. being
-- totally pointfree.
caf5 :: Either String (NodeDescription, ByteString)
caf5 = join . flip parseEither valueToNDParser . (uncozipL .) $ \f -> (flip (pure +++) eitherValue) (firstM f)

-- This got rid of the flip in the hopes of more comprehensibility.
-- When the smoke cleared, I thought (uncozipL .) was too fundamentally
-- incomprehensible and went back to caf2, which Baldur then either
-- improved upon or did better than ab initio.
caf6 :: Either String (NodeDescription, ByteString)
caf6 = join . flip parseEither valueToNDParser . (uncozipL .) $ \f -> (pure +++ firstM f) eitherValue
-}
