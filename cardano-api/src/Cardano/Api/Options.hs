{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Options
  ( fileOption
  , inFileOption
  , outFileOption
  , parseFilePath
  , parseFile
  , parseFileIn
  , parseFileOut
  , parseDirectory
  ) where

import           Cardano.Api.IO (Directory, File (..), FileDirection (..))

import           Options.Applicative (OptionFields)
import qualified Options.Applicative as Opt
import           Options.Applicative.Builder (Mod)
import           Options.Applicative.Types (Parser)

fileOption :: forall direction. Mod OptionFields FilePath -> Parser (File direction)
fileOption = fmap File . Opt.strOption

inFileOption :: forall. Mod OptionFields FilePath -> Parser (File 'In)
inFileOption = fileOption

outFileOption :: forall. Mod OptionFields FilePath -> Parser (File 'Out)
outFileOption = fileOption

parseDirectory :: String -> String -> Opt.Parser Directory
parseDirectory optname desc =
  Opt.strOption $ mconcat
    [ Opt.long optname
    , Opt.metavar "DIRECTORY"
    , Opt.help desc
    , Opt.completer (Opt.bashCompleter "directory")
    ]

parseFilePath :: String -> String -> Opt.Parser FilePath
parseFilePath optname desc =
  Opt.strOption $ mconcat
    [ Opt.long optname
    , Opt.metavar "FILEPATH"
    , Opt.help desc
    , Opt.completer (Opt.bashCompleter "file")
    ]
-- {-# DEPRECATED parseFilePath "Use parseFile instead" #-}

parseFile :: String -> String -> Opt.Parser (File direction)
parseFile optname desc =
  Opt.strOption $ mconcat
    [ Opt.long optname
    , Opt.metavar "FILEPATH"
    , Opt.help desc
    , Opt.completer (Opt.bashCompleter "file")
    ]

parseFileIn :: String -> String -> Opt.Parser (File 'In)
parseFileIn = parseFile

parseFileOut :: String -> String -> Opt.Parser (File 'Out)
parseFileOut = parseFile
