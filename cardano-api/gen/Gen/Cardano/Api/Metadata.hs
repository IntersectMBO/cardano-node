{-# LANGUAGE OverloadedStrings #-}

module Gen.Cardano.Api.Metadata
  ( genTxMetadata
  , genTxMetadataValue
  , genJsonForTxMetadata
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Data.Aeson (ToJSON (..))
import           Hedgehog (Gen)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range

-- ----------------------------------------------------------------------------
-- Generators
--

genJsonForTxMetadata :: TxMetadataJsonSchema -> Gen Aeson.Value
genJsonForTxMetadata mapping =
    Gen.sized $ \sz ->
      Aeson.object <$>
      Gen.list (Range.linear 0 (fromIntegral sz))
               ((,) <$> (Aeson.fromString . show <$> Gen.word64 Range.constantBounded)
                    <*> genJsonForTxMetadataValue mapping)

genJsonForTxMetadataValue :: TxMetadataJsonSchema -> Gen Aeson.Value
genJsonForTxMetadataValue TxMetadataJsonNoSchema = genJsonValue
  where
    genJsonValue :: Gen Aeson.Value
    genJsonValue =
      Gen.sized $ \sz ->
        Gen.frequency
          [ (1,         Aeson.toJSON <$> genJsonNumber)
          , (2,         Aeson.toJSON <$> genJsonText)
          , (fromIntegral (signum sz),
                        Aeson.toJSON <$> Gen.scale (`div` 2) genJsonList)
          , (fromIntegral (signum sz),
                        Aeson.object <$> Gen.scale (`div` 2) genJsonMap)
          ]

    genJsonNumber :: Gen Integer
    genJsonNumber = Gen.integral
                      (Range.linear
                        (-fromIntegral (maxBound :: Word64) :: Integer)
                        ( fromIntegral (maxBound :: Word64) :: Integer))

    genJsonText  :: Gen Text
    genJsonText = Gen.choice
                    [ Gen.ensure validText (genText 64)
                    , Gen.ensure validText ((bytesPrefix <>) <$> genText 62)
                    , genBytes
                    , Text.pack . show <$> genJsonNumber
                    ]
      where
        validText t = BS.length (Text.encodeUtf8 t) <= 64
        bytesPrefix = "0x"
        genText sz  = Text.pack <$> Gen.list (Range.linear 0 sz) Gen.alphaNum
        genBytes    = (bytesPrefix <>)
                    . Text.decodeUtf8
                    . Base16.encode
                    . BS.pack
                  <$> Gen.list (Range.linear 0 64)
                               (Gen.word8 Range.constantBounded)

    genJsonList :: Gen [Aeson.Value]
    genJsonList = Gen.sized $ \sz ->
                    Gen.list (Range.linear 0 (fromIntegral sz)) genJsonValue

    genJsonKey :: Gen Aeson.Key
    genJsonKey = fmap Aeson.fromText genJsonText

    genJsonMap :: Gen [(Aeson.Key, Aeson.Value)]
    genJsonMap = Gen.sized $ \sz ->
                   Gen.list (Range.linear 0 (fromIntegral sz)) $
                     (,) <$> genJsonKey <*> genJsonValue


genJsonForTxMetadataValue TxMetadataJsonDetailedSchema = genJsonValue
  where
    genJsonValue :: Gen Aeson.Value
    genJsonValue =
      Gen.sized $ \sz ->
        Gen.frequency
          [ (1,         singleFieldObject "int"    <$> genJsonNumber)
          , (1,         singleFieldObject "bytes"  <$> genJsonBytes)
          , (1,         singleFieldObject "string" <$> genJsonText)
          , (fromIntegral (signum sz),
                        singleFieldObject "list" <$>
                          Gen.scale (`div` 2) genJsonList)
          , (fromIntegral (signum sz),
                        singleFieldObject "map" <$>
                          Gen.scale (`div` 2) genJsonMap)
          ]

    singleFieldObject name v = Aeson.object [(name, v)]

    genJsonNumber :: Gen Aeson.Value
    genJsonNumber = toJSON <$>
                    Gen.integral
                      (Range.linear
                        (-fromIntegral (maxBound :: Word64) :: Integer)
                        ( fromIntegral (maxBound :: Word64) :: Integer))

    genJsonBytes :: Gen Aeson.Value
    genJsonBytes = toJSON
                 . Text.decodeLatin1
                 . Base16.encode
                 . BS.pack
               <$> Gen.list (Range.linear 0 64)
                            (Gen.word8 Range.constantBounded)

    genJsonText  :: Gen Aeson.Value
    genJsonText = fmap toJSON $
                    Gen.ensure validText $
                      Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum
      where
        validText t = BS.length (Text.encodeUtf8 t) <= 64

    genJsonList :: Gen Aeson.Value
    genJsonList = fmap toJSON $
                    Gen.sized $ \sz ->
                      Gen.list (Range.linear 0 (fromIntegral sz)) genJsonValue

    genJsonMap :: Gen Aeson.Value
    genJsonMap = fmap toJSON $
                   Gen.sized $ \sz ->
                     Gen.list (Range.linear 0 (fromIntegral sz)) $
                       mkKVPair <$> genJsonValue <*> genJsonValue
      where
        mkKVPair :: Aeson.Value -> Aeson.Value -> Aeson.Value
        mkKVPair k v = Aeson.object [ ("k", k), ("v", v) ]


genTxMetadata :: Gen TxMetadata
genTxMetadata =
    Gen.sized $ \sz ->
      TxMetadata . Map.fromList <$>
      Gen.list (Range.linear 0 (fromIntegral sz))
               ((,) <$> Gen.word64 Range.constantBounded
                    <*> genTxMetadataValue)

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
    Gen.sized $ \sz ->
      Gen.frequency
        [ (1,         TxMetaNumber <$> genTxMetaNumber)
        , (1,         TxMetaBytes  <$> genTxMetaBytes)
        , (1,         TxMetaText   <$> genTxMetaText)
        , (fromIntegral (signum sz),
                      TxMetaList   <$> Gen.scale (`div` 2) genTxMetaList)
        , (fromIntegral (signum sz),
                      TxMetaMap    <$> Gen.scale (`div` 2) genTxMetaMap)
        ]
  where
    genTxMetaNumber :: Gen Integer
    genTxMetaNumber = Gen.integral
                        (Range.linear
                          (-fromIntegral (maxBound :: Word64) :: Integer)
                          ( fromIntegral (maxBound :: Word64) :: Integer))

    genTxMetaBytes :: Gen ByteString
    genTxMetaBytes = BS.pack <$> Gen.list (Range.linear 0 64)
                                          (Gen.word8 Range.constantBounded)

    genTxMetaText  :: Gen Text
    genTxMetaText = Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum

    genTxMetaList :: Gen [TxMetadataValue]
    genTxMetaList = Gen.sized $ \sz ->
                      Gen.list (Range.linear 0 (fromIntegral sz))
                               genTxMetadataValue

    genTxMetaMap  :: Gen [(TxMetadataValue, TxMetadataValue)]
    genTxMetaMap = Gen.sized $ \sz ->
                      Gen.list (Range.linear 0 (fromIntegral sz)) $
                        (,) <$> genTxMetadataValue <*> genTxMetadataValue
