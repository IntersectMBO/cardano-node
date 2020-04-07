module Cardano.Api.View
  ( parseAddressView
  , parseKeyPairView
  , parsePublicKeyView
  , parseTxSignedView
  , parseTxUnsignedView

  , readAddress
  , readKeyPair
  , readPublicKey
  , readTxSigned
  , readTxUnsigned

  , renderAddressView
  , renderKeyPairView
  , renderPublicKeyView
  , renderTxSignedView
  , renderTxUnsignedView

  , writeAddress
  , writeKeyPair
  , writePublicKey
  , writeTxSigned
  , writeTxUnsigned

  -- Exported for testing.
  , rawToMultilineHex
  , unRawToMultilineHex
  ) where

import           Cardano.Api.CBOR
import           Cardano.Api.Types
import           Cardano.Api.Error

import           Cardano.Prelude

import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except.Extra (handleIOExceptT, hoistEither, runExceptT)

import qualified Data.Text as Text


parseAddressView :: ByteString -> Either ApiError Address
parseAddressView bs =
    case BS.lines bs of
      ("AddressByron" : rest) -> parseLines rest
      ("AddressShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parseAddressView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parseAddressView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError Address
    parseLines xs =
      addressFromCBOR =<< unRawToMultilineHex (BS.concat xs)

parseKeyPairView :: ByteString -> Either ApiError KeyPair
parseKeyPairView bs =
    case BS.lines bs of
      ("KeyPairByron" : rest) -> parseLines rest
      ("KeyPairShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parseKeyPairView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parseKeyPairView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError KeyPair
    parseLines xs =
      keyPairFromCBOR =<< unRawToMultilineHex (BS.concat xs)

parsePublicKeyView :: ByteString -> Either ApiError PublicKey
parsePublicKeyView bs =
    case BS.lines bs of
      ("PublicKeyByron" : rest) -> parseLines rest
      ("PublicKeyShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parsePublicKeyView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parsePublicKeyView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError PublicKey
    parseLines xs =
      publicKeyFromCBOR =<< unRawToMultilineHex (BS.concat xs)

parseTxSignedView :: ByteString -> Either ApiError TxSigned
parseTxSignedView bs =
    case BS.lines bs of
      ("TxSignedByron" : rest) -> parseLines rest
      ("TxSignedShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parseTxSignedView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parseTxSignedView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError TxSigned
    parseLines xs =
      txSignedFromCBOR =<< unRawToMultilineHex (BS.concat xs)

parseTxUnsignedView :: ByteString -> Either ApiError TxUnsigned
parseTxUnsignedView bs =
    case BS.lines bs of
      ("TxUnsignedByron" : rest) -> parseLines rest
      ("TxUnsignedShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parseTxUnsignedView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parseTxUnsignedView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError TxUnsigned
    parseLines xs =
      txUnsignedFromCBOR =<< unRawToMultilineHex (BS.concat xs)

renderAddressView :: Address -> ByteString
renderAddressView kp =
  BS.unlines $
    case kp of
      AddressByron {} -> "AddressByron" : xs
      AddressShelley {} -> "AddressShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ addressToCBOR kp

renderKeyPairView :: KeyPair -> ByteString
renderKeyPairView kp =
  BS.unlines $
    case kp of
      KeyPairByron {} -> "KeyPairByron" : xs
      KeyPairShelley {} -> "KeyPairShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ keyPairToCBOR kp

renderPublicKeyView :: PublicKey -> ByteString
renderPublicKeyView kp =
  BS.unlines $
    case kp of
      PubKeyByron {} -> "PublicKeyByron" : xs
      PubKeyShelley {} -> "PublicKeyShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ publicKeyToCBOR kp

renderTxSignedView :: TxSigned -> ByteString
renderTxSignedView tu =
  BS.unlines $
    case tu of
      TxSignedByron {} -> "TxSignedByron" : xs
      TxSignedShelley {} -> "TxSignedShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ txSignedToCBOR tu

renderTxUnsignedView :: TxUnsigned -> ByteString
renderTxUnsignedView tu =
  BS.unlines $
    case tu of
      TxUnsignedByron {} -> "TxUnsignedByron" : xs
      TxUnsignedShelley {} -> "TxUnsignedShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ txUnsignedToCBOR tu

-- -------------------------------------------------------------------------------------------------

readAddress :: FilePath -> IO (Either ApiError Address)
readAddress path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseAddressView bs

readKeyPair :: FilePath -> IO (Either ApiError KeyPair)
readKeyPair path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseKeyPairView bs

readPublicKey :: FilePath -> IO (Either ApiError PublicKey)
readPublicKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parsePublicKeyView bs

readTxSigned :: FilePath -> IO (Either ApiError TxSigned)
readTxSigned path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseTxSignedView bs

readTxUnsigned :: FilePath -> IO (Either ApiError TxUnsigned)
readTxUnsigned path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseTxUnsignedView bs

writeAddress :: FilePath -> Address -> IO (Either ApiError ())
writeAddress path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderAddressView kp)

writeKeyPair :: FilePath -> KeyPair -> IO (Either ApiError ())
writeKeyPair path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderKeyPairView kp)

writePublicKey :: FilePath -> PublicKey -> IO (Either ApiError ())
writePublicKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderPublicKeyView kp)

writeTxSigned :: FilePath -> TxSigned -> IO (Either ApiError ())
writeTxSigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxSignedView kp)

writeTxUnsigned :: FilePath -> TxUnsigned -> IO (Either ApiError ())
writeTxUnsigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxUnsignedView kp)

-- -------------------------------------------------------------------------------------------------

-- | Convert a raw ByteString to hexadecimal and then line wrap
rawToMultilineHex :: ByteString -> [ByteString]
rawToMultilineHex = chunksOf . Base16.encode

-- | Convert from multiline hexadecimal to a raw ByteString.
unRawToMultilineHex :: ByteString -> Either ApiError ByteString
unRawToMultilineHex bs =
  case Base16.decode (BS.concat $ BS.lines bs) of
    (raw, "") -> Right raw
    (_, err) -> Left $ ApiError ("unRawToMultilineHex: Unable to decode " <> textShow err)

chunksOf :: ByteString -> [ByteString]
chunksOf bs =
  case BS.splitAt 80 bs of
    (leading, "") -> [leading]
    (leading, trailing) -> leading : chunksOf trailing


textShow :: Show a => a -> Text
textShow = Text.pack . show
