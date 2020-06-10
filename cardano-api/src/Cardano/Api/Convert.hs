module Cardano.Api.Convert
  ( addressFromHex
  , addressToHex
  , renderTxId
  , parseTxIn
  , parseTxOut
  , renderTxIn
  , renderTxOut
  ) where

import           Cardano.Api.Types
import qualified Cardano.Binary as Binary
import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad.Fail (fail)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C8
import           Data.Char (isAlphaNum)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto

import qualified Shelley.Spec.Ledger.Address as Shelley


addressFromHex :: Text -> Either Binary.DecoderError Address
addressFromHex txt =
  case Base16.decode (Text.encodeUtf8 txt) of
    (raw, _) ->
      case Shelley.deserialiseAddr raw of
        Just addr -> Right $ AddressShelley addr
        Nothing ->  AddressByron <$> Binary.decodeFull' raw

addressToHex :: Address -> Text
addressToHex addr =
  -- Text.decodeUtf8 theoretically can throw an exception but should never
  -- do so on Base16 encoded data.
  Text.decodeUtf8 . Base16.encode $
    case addr of
      AddressByron ba -> Binary.serialize' ba
      AddressShelley sa -> Shelley.serialiseAddr sa
      AddressShelleyReward sRwdAcct -> Binary.serialize' sRwdAcct

renderTxId :: TxId -> Text
renderTxId (TxId txid) = Text.decodeUtf8 (Crypto.getHashBytesAsHex txid)

parseTxIn :: Text -> Either String TxIn
parseTxIn txt = Atto.parseOnly pTxIn $ Text.encodeUtf8 txt

parseTxOut :: Text -> Either String TxOut
parseTxOut tOut = Atto.parseOnly pTxOut $ Text.encodeUtf8 tOut

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
    , "+"
    , Text.pack (show ll)
    ]

pTxIn :: Parser TxIn
pTxIn = TxIn <$> pTxId <*> (Atto.char '#' *> Atto.decimal)

pTxId :: Parser TxId
pTxId = TxId <$> pCBlakeHash

pCBlakeHash :: Parser (Crypto.Hash Crypto.Blake2b_256 ())
pCBlakeHash = do
   potentialHex <- pAlphaNumToByteString
   resultHash <- return $ Crypto.hashFromBytesAsHex potentialHex
   case resultHash of
     Nothing -> handleHexParseFailure potentialHex $ Atto.parseOnly pAddress potentialHex
     Just hash -> return hash
  where
   -- We fail in both cases: 1) The input is not hex encoded 2) A user mistakenly enters an address
   handleHexParseFailure :: ByteString -> Either String Address -> Parser (Crypto.Hash Crypto.Blake2b_256 ())
   handleHexParseFailure input (Left _) = fail $ "Your input is either malformed or not hex encoded: " ++ C8.unpack input
   handleHexParseFailure _ (Right _) = fail $ " You have entered an address, please enter a tx input"

pTxOut :: Parser TxOut
pTxOut =
  TxOut <$> pAddress <* Atto.char '+' <*> pLovelace

pLovelace :: Parser Lovelace
pLovelace = Lovelace <$> Atto.decimal

pAddress :: Parser Address
pAddress = do
  potentialHex <- pAlphaNumToByteString
  case addressFromHex $ Text.decodeUtf8 potentialHex of
    Right addr -> return addr
    Left err -> fail $ "Error deserialising address: " <> (C8.unpack potentialHex)
                     <> " Error: " <> show err

pAlphaNumToByteString :: Parser ByteString
pAlphaNumToByteString = Atto.takeWhile1 isAlphaNum
