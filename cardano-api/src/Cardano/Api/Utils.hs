{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | Internal utils for the other Api modules
--
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  , (..=)
  , (..=?)
  , formatParsecError
  , failEither
  , failEitherWith
  , noInlineMaybeToStrictMaybe
  , note
  , parseFilePath
  , readFileBlocking
  , renderEra
  , runParsecParser
  , textShow
  , writeSecrets

    -- ** CLI option parsing
  , bounded
  ) where

import           Control.Exception (bracket)
import           Control.Monad (forM_, when)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.IO.Handle.FD (openFileBlocking)
import qualified Options.Applicative as Opt
import           System.FilePath ((</>))
import           System.IO (IOMode (ReadMode), hClose)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.ParserCombinators.Parsec.Error as Parsec
import           Text.Printf (printf)
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif

import           Cardano.Api.Eras
import           Data.Aeson (KeyValue, ToJSON, (.=))
import           Options.Applicative (ReadM)
import           Options.Applicative.Builder (eitherReader)
import qualified Text.Read as Read

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x

{-# NOINLINE noInlineMaybeToStrictMaybe #-}
noInlineMaybeToStrictMaybe :: Maybe a -> StrictMaybe a
noInlineMaybeToStrictMaybe Nothing = SNothing
noInlineMaybeToStrictMaybe (Just x) = SJust x

formatParsecError :: Parsec.ParseError -> String
formatParsecError err =
  Parsec.showErrorMessages "or" "unknown parse error"
    "expecting" "unexpected" "end of input"
    $ Parsec.errorMessages err

runParsecParser :: Parsec.Parser a -> Text -> Aeson.Parser a
runParsecParser parser input =
  case Parsec.parse (parser <* Parsec.eof) "" (Text.unpack input) of
    Right txin -> pure txin
    Left parseError -> fail $ formatParsecError parseError

failEither :: MonadFail m => Either String a -> m a
failEither = either fail pure

failEitherWith :: MonadFail m => (e -> String) -> Either e a -> m a
failEitherWith f = either (fail . f) pure

note :: MonadFail m => String -> Maybe a -> m a
note msg = \case
  Nothing -> fail msg
  Just a -> pure a

parseFilePath :: String -> String -> Opt.Parser FilePath
parseFilePath optname desc =
  Opt.strOption
    ( Opt.long optname
    <> Opt.metavar "FILEPATH"
    <> Opt.help desc
    <> Opt.completer (Opt.bashCompleter "file")
    )

writeSecrets :: FilePath -> [Char] -> [Char] -> (a -> BS.ByteString) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    BS.writeFile filename $ secretOp secret
#ifdef UNIX
    setFileMode    filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif

readFileBlocking :: FilePath -> IO BS.ByteString
readFileBlocking path = bracket
  (openFileBlocking path ReadMode)
  hClose
  (\fp -> do
    -- An arbitrary block size.
    let blockSize = 4096
    let go acc = do
          next <- BS.hGet fp blockSize
          if BS.null next
          then pure acc
          else go (acc <> Builder.byteString next)
    contents <- go mempty
    pure $ LBS.toStrict $ Builder.toLazyByteString contents)

textShow :: Show a => a -> Text
textShow = Text.pack . show

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"
renderEra (AnyCardanoEra AlonzoEra)  = "Alonzo"
renderEra (AnyCardanoEra BabbageEra) = "Babbage"
renderEra (AnyCardanoEra ConwayEra)  = "Conway"

bounded :: forall a. (Bounded a, Integral a, Show a) => String -> ReadM a
bounded t = eitherReader $ \s -> do
  i <- Read.readEither @Integer s
  when (i < fromIntegral (minBound @a)) $ Left $ t <> " must not be less than " <> show (minBound @a)
  when (i > fromIntegral (maxBound @a)) $ Left $ t <> " must not greater than " <> show (maxBound @a)
  pure (fromIntegral i)

-- | A key-value pair difference list for encoding a JSON object.
(..=) :: (KeyValue kv, ToJSON v) => Aeson.Key -> v -> [kv] -> [kv]
(..=) n v = (n .= v:)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(..=?) :: (KeyValue kv, ToJSON v) => Aeson.Key -> Maybe v -> [kv] -> [kv]
(..=?) n mv = case mv of
  Just v -> (n .= v:)
  Nothing -> id
