{-# LANGUAGE ScopedTypeVariables #-}
module Parsers.Help
  ( HelpOptions(..)
  , cmdHelp
  , runHelpOptions
  ) where

import           Cardano.CLI.EraBased.Options.Common

import           Control.Monad (forM_)
import qualified Data.List as List
import           Options.Applicative
import           Options.Applicative.Help.Core
import           Options.Applicative.Help.Types (renderHelp)
import           Options.Applicative.Types (OptReader (..), Option (..), Parser (..))
import qualified System.IO as IO

data HelpOptions = HelpOptions
  deriving (Eq, Show)

optsHelp :: Parser HelpOptions
optsHelp = pure HelpOptions

helpAll :: ParserPrefs -> String -> [String] -> ParserInfo a -> IO ()
helpAll pprefs progn rnames parserInfo = do
  IO.putStrLn $ renderHelp 80 (usage_help parserInfo)
  IO.putStrLn ""
  go (infoParser parserInfo)
  where
    go :: Parser a -> IO ()
    go p = case p of
      NilP _ -> return ()
      OptP optP -> case optMain optP of
        CmdReader _ cs -> do
          forM_ cs $ \(c, subParserInfo) ->
            helpAll pprefs progn (c:rnames) subParserInfo
        _ -> return ()
      AltP pa pb -> go pa >> go pb
      MultP pf px -> go pf >> go px
      BindP pa _ -> go pa
    usage_help i =
      mconcat
      [ usageHelp (pure . parserUsage pprefs (infoParser i) . List.unwords $ progn : reverse rnames)
      , descriptionHelp (infoProgDesc i)
      ]

runHelpOptions :: ParserPrefs -> ParserInfo a -> HelpOptions -> IO ()
runHelpOptions pprefs allParserInfo HelpOptions {} = helpAll pprefs "cardano-testnet" [] allParserInfo

cmdHelp :: Mod CommandFields HelpOptions
cmdHelp = command' "help" "Show cardano-testnet help" optsHelp
