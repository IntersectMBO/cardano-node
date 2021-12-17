{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
module Cardano.Benchmarking.GeneratorTx.CLI.Parsers
  (module Cardano.Benchmarking.GeneratorTx.CLI.Parsers)
where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Cardano.Api hiding (lexPlausibleAddressString, parseAddressAny)
import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.NodeAddress
import qualified Control.Arrow as Arr
import           Control.Monad (fail)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Options.Applicative (Parser, auto, bashCompleter, completer, flag, help, long,
                   metavar, option, strOption)
import qualified Options.Applicative as Opt


lastly :: Parser a -> Parser (Last a)
lastly = (Last <$>) . optional

----------------------------------------------------------------

parseFlag :: String -> String -> Parser Bool
parseFlag = parseFlag' False True

parseFlag' :: a -> a -> String -> String -> Parser a
parseFlag' def active optname desc =
  flag def active $ long optname <> help desc

parseTargetNodeAddress :: String -> String -> Parser NodeIPv4Address
parseTargetNodeAddress optname desc =
  option
    ( uncurry NodeAddress
      . Arr.first parseHostAddress
      . Arr.second parsePort
      <$> auto
    )
    $ long optname
      <> metavar "(HOST,PORT)"
      <> help desc

parseHostAddress :: String -> NodeHostIPv4Address
parseHostAddress = NodeHostIPv4Address .
  maybe (panic "Bad host of target node") identity . readMaybe

parsePort :: Word16 -> PortNumber
parsePort = fromIntegral

parseFeePerTx :: String -> String -> Parser Lovelace
parseFeePerTx opt desc = quantityToLovelace . Quantity <$> parseIntegral opt desc

parseInitialTTL :: String -> String -> Parser SlotNo
parseInitialTTL opt desc = SlotNo <$> parseIntegral opt desc

parseSigningKeysFile :: String -> String -> Parser SigningKeyFile
parseSigningKeysFile opt desc = SigningKeyFile <$> parseFilePath opt desc

------------------------------------------------------------------

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseDouble :: String -> String -> Parser Double
parseDouble optname desc = option auto
  $ long optname <> metavar "DOUBLE" <> help desc

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    $ long optname
        <> metavar "FILEPATH"
        <> help desc
        <> completer (bashCompleter "file")

parseSocketPath :: String -> String -> Parser SocketPath
parseSocketPath optname desc =
  SocketPath <$> parseFilePath optname desc

parseConfigFile :: String -> String -> Parser FilePath
parseConfigFile = parseFilePath

parseGenesisPath :: Parser FilePath
parseGenesisPath =
  strOption
    ( long "genesis-file"
        <> metavar "FILEPATH"
        <> help "Path to the genesis yaml file."
    )

------------------------------------------------------------------
-- Sadly the following isn't exported from:
--   module Cardano.CLI.Shelley.Parsers
pTxIn :: Parser TxIn
pTxIn =
  Opt.option (readerFromAttoParser parseTxIn)
    (  Opt.long "tx-in"
    <> Opt.metavar "TX-IN"
    <> Opt.help "The input transaction as TxId#TxIx where TxId is the transaction hash and TxIx is the index."
    )

parseTxIn :: Atto.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Atto.char '#' *> parseTxIx)

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txid (TxIx txix)) =
  mconcat
    [ Text.decodeUtf8 (serialiseToRawBytesHex txid)
    , "#"
    , Text.pack (show txix)
    ]

parseTxId :: Atto.Parser TxId
parseTxId = do
  bstr <- Atto.takeWhile1 Char.isHexDigit
  case deserialiseFromRawBytesHex AsTxId bstr of
    Just addr -> return addr
    Nothing -> fail $ "Incorrect transaction id format:: " ++ show bstr

parseTxIx :: Atto.Parser TxIx
parseTxIx = toEnum <$> Atto.decimal

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

pTxOut :: Parser (TxOut CtxTx ShelleyEra)
pTxOut =
  Opt.option (readerFromAttoParser parseTxOut)
    (  Opt.long "tx-out"
    <> Opt.metavar "TX-OUT"
    <> Opt.help "The transaction output as Address+Lovelace where Address is \
                \the Bech32-encoded address followed by the amount in \
                \Lovelace."
    )
  where
    parseTxOut :: Atto.Parser (TxOut CtxTx ShelleyEra)
    parseTxOut =
      TxOut <$> parseAddressInEra
            <*  Atto.char '+'
            <*> (TxOutAdaOnly AdaOnlyInShelleyEra <$> parseLovelace)
            <*> pure TxOutDatumNone

parseAddressInEra :: IsCardanoEra era => Atto.Parser (AddressInEra era)
parseAddressInEra = do
    addr <- parseAddressAny
    case anyAddressInEra cardanoEra addr of
      Nothing -> fail "invalid address in the target era"
      Just a  -> pure a

parseAddressAny :: Atto.Parser AddressAny
parseAddressAny = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsAddressAny str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

parseLovelace :: Atto.Parser Lovelace
parseLovelace = quantityToLovelace . Quantity <$> Atto.decimal

lexPlausibleAddressString :: Atto.Parser Text
lexPlausibleAddressString =
    Text.decodeLatin1 <$> Atto.takeWhile1 isPlausibleAddressChar
  where
    -- Covers both base58 and bech32 (with constrained prefixes)
    isPlausibleAddressChar c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '_'
