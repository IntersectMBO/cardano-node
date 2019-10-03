module Cardano.Common.Parsers
  ( parseIP
  , parseSlotNumber
  ) where

import Cardano.Prelude hiding (check)
import Prelude (String)

import           Data.Char (digitToInt)
import           Data.IP
import qualified Data.IP as IP
import qualified Data.Text as T
import qualified Text.Appar.String as A

-------------------------------------------------------------------------------
-- IP parsers: Taken from http://hackage.haskell.org/package/iproute-1.7.7/docs/Data-IP.html
-------------------------------------------------------------------------------

parseIP :: String -> Either String IP
parseIP cs =
  case A.runParser ip4 cs of
    (Just ip,_) -> Right $ IPv4 ip
    (Nothing,_) -> case A.runParser ip6 cs of
                     (Just ip,_) -> Right $ IPv6 ip
                     (Nothing,_) -> Left $ "IP parsing error: " <> cs

parseSlotNumber :: String -> Either String Int
parseSlotNumber str =
  case A.runParser slotNumber str of
    (Nothing,rest) -> Left rest
    (Just [slotNum],_) -> Right slotNum
    (Just slotNums,_) -> Left $ "Slot number parse error:\n  "
                              <> "Parsed values: " <> show slotNums

-------------------------------------------------------------------------------
-- IPv4 Parser
-------------------------------------------------------------------------------

dig :: A.Parser Int
dig = 0 <$ A.char '0'
  <|> toInt <$> A.oneOf ['1'..'9'] <*> many A.digit
 where
  toInt :: Char -> String -> Int
  toInt n ns = foldl' (\x y -> x * 10 + y) 0 . map digitToInt $ n : ns

ip4 :: A.Parser IPv4
ip4 = skipSpaces >> IP.toIPv4 <$> ip4'

ip4' :: A.Parser [Int]
ip4' = do
  as <- dig `A.sepBy1` A.char '.'
  check as
  return as
 where
  test :: String -> Int -> A.MkParser String ()
  test errmsg adr = when (adr < 0 || 255 < adr) (panic $ T.pack errmsg)
  check :: [Int] -> A.MkParser String ()
  check as = do
    let errmsg :: String
        errmsg = "IPv4 address"
    when (length as /= 4) (panic $ T.pack errmsg)
    mapM_ (test errmsg) as

-------------------------------------------------------------------------------
-- IPv6 Parser (RFC 4291)
-------------------------------------------------------------------------------

hex :: A.Parser Int
hex = do
  ns <- some A.hexDigit
  check ns
  let ms = map digitToInt ns
      val = foldl' (\x y -> x * 16 + y) 0 ms
  return val
 where
  check :: String -> A.MkParser String ()
  check ns = when (length ns > 4) (panic $ T.pack "IPv6 address -- more than 4 hex")

colon2 :: A.Parser ()
colon2 = void $ A.string "::"

format :: [Int] -> [Int] -> A.Parser [Int]
format bs1 bs2 = do
  let len1 = length bs1
      len2 = length bs2
  when (len1 > 7) (panic $ T.pack "IPv6 address1")
  when (len2 > 7) (panic $ T.pack "IPv6 address2")
  let len = 8 - len1 - len2
  when (len <= 0) (panic $ T.pack "IPv6 address3")
  let spring :: [Int]
      spring = replicate len 0
  return $ bs1 ++ spring ++ bs2

ip6 :: A.Parser IPv6
ip6 = skipSpaces >> toIPv6 <$> ip6'

ip6' :: A.Parser [Int]
ip6' = ip4Embedded
  <|> do colon2
         bs <- A.option [] hexcolon
         format [] bs
  <|> A.try ( do rs <- hexcolon
                 check rs
                 return rs
            )
  <|> do bs1 <- hexcolon2
         bs2 <- A.option [] hexcolon
         format bs1 bs2
 where
  hexcolon :: A.Parser [Int]
  hexcolon = hex `A.sepBy1` A.char ':'
  hexcolon2 :: A.Parser [Int]
  hexcolon2 = A.manyTill (hex <* A.char ':') (A.char ':')
  check :: [Int] -> A.MkParser String ()
  check bs = when (length bs /= 8) (panic $ T.pack "IPv6 address4")

ip4Embedded :: A.Parser [Int]
ip4Embedded =
      A.try ( do colon2
                 bs <- beforeEmbedded
                 embedded <- ip4'
                 format [] (bs ++ ip4ToIp6 embedded)
            )
      -- matches 2001:db8::192.0.2.1
  <|> A.try ( do bs1 <- A.manyTill (A.try $ hex <* A.char ':') (A.char ':')
                 bs2 <- A.option [] beforeEmbedded
                 embedded <- ip4'
                 format bs1 $ bs2 ++ ip4ToIp6 embedded
            )
      -- matches 2001:db8:11e:c00:aa:bb:192.0.2.1
  <|> A.try ( do bs <- beforeEmbedded
                 embedded <- ip4'
                 let rs = bs ++ ip4ToIp6 embedded
                 check rs
                 return rs
            )
 where
  beforeEmbedded :: A.Parser [Int]
  beforeEmbedded = A.many $ A.try $ hex <* A.char ':'
  ip4ToIp6 :: [Int] -> [Int]
  ip4ToIp6 [a,b,c,d] = [ a `shiftL` 8 .|. b
                       , c `shiftL` 8 .|. d
                       ]
  ip4ToIp6 _ = panic $ T.pack "ip4ToIp6"
  check :: [Int] -> A.MkParser String ()
  check bs = when (length bs /= 8) (panic $ T.pack "IPv6 address4")

skipSpaces :: A.Parser ()
skipSpaces = void $ many (A.char ' ')

slotNumber :: A.Parser [Int]
slotNumber = do skipSpaces
                (map digitToInt <$> A.manyTill A.digit A.space)
