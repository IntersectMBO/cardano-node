module Cardano.Api.Convert
  ( addressFromHex
  , addressToHex
  , parseTxIn
  , parseTxOut
  , renderTxIn
  , renderTxOut
  ) where

import           Cardano.Api.Types
import qualified Cardano.Binary as Binary
import           Cardano.Prelude

import           Control.Monad.Fail (fail)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16 as Base16
import           Data.Char (isAlphaNum)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto

import qualified Shelley.Spec.Ledger.Address as Shelley


addressFromHex :: Text -> Maybe Address
addressFromHex txt =
  case Base16.decode (Text.encodeUtf8 txt) of
    (raw, _) ->
      case Shelley.deserialiseAddr raw of
        Just addr -> Just $ AddressShelley addr
        Nothing -> either (const Nothing) (Just . AddressByron) $ Binary.decodeFull' raw

addressToHex :: Address -> Text
addressToHex addr =
  -- Text.decodeUtf8 theoretically can throw an exception but should never
  -- do so on Base16 encoded data.
  Text.decodeUtf8 . Base16.encode $
    case addr of
      AddressByron ba -> Binary.serialize' ba
      AddressShelley sa -> Shelley.serialiseAddr sa

parseTxIn :: Text -> Maybe TxIn
parseTxIn txt =
  case Atto.parseOnly pTxIn $ Text.encodeUtf8 txt of
    Left _Str -> Nothing
    Right txIn -> Just txIn

parseTxOut :: Text -> Maybe TxOut
parseTxOut =
  either (const Nothing) Just . Atto.parseOnly pTxOut . Text.encodeUtf8

renderTxIn :: TxIn -> Text
renderTxIn (TxIn (TxId txid) txix) =
  mconcat
    [ Text.decodeUtf8 (Crypto.getHashBytesAsHex txid)
    , "#"
    , Text.pack (show txix)
    ]

renderTxOut :: TxOut -> Text
renderTxOut (TxOut addr ll) =
  mconcat
    [ addressToHex addr
    , "$"
    , Text.pack (show ll)
    ]

pTxIn :: Parser TxIn
pTxIn = TxIn <$> pTxId <*> (Atto.char '#' *> Atto.decimal)

pTxId :: Parser TxId
pTxId = TxId <$> pCBlakeHash

pCBlakeHash :: Parser (Crypto.Hash Crypto.Blake2b_256 ())
pCBlakeHash =
  maybe (fail "pCBlakeHash") pure
    =<< Crypto.hashFromBytesAsHex <$> pHexToByteString

pTxOut :: Parser TxOut
pTxOut =
  TxOut <$> pAddress <* Atto.char '$' <*> pLovelace

pLovelace :: Parser Lovelace
pLovelace = Lovelace <$> Atto.decimal

pAddress :: Parser Address
pAddress =
  maybe (fail "pAddress") pure
    =<< addressFromHex . Text.decodeUtf8 <$> pHexToByteString

pHexToByteString :: Parser ByteString
pHexToByteString = Atto.takeWhile1 isAlphaNum
