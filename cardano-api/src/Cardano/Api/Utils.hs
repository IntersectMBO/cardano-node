{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | Internal utils for the other Api modules
--
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  , formatParsecError
  , failEither
  , failEitherWith
  , noInlineMaybeToStrictMaybe
  , note
  , parseFilePath
  , runParsecParser
  , writeSecrets
  ) where

import           Prelude

import           Control.Monad (forM_)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import           Data.Maybe.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.ParserCombinators.Parsec.Error as Parsec
import           Text.Printf (printf)
import qualified Options.Applicative as Opt
import           System.FilePath ((</>))
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif

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
