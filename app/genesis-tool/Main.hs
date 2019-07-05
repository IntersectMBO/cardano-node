{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}


module Main (main) where

import           Data.Semigroup ((<>))

import           Options.Applicative

import           Cardano.Prelude hiding (option)

import           CLI
import           Run


-- | The product type of all command line arguments
data ArgParser = ArgParser !CLI

-- | The product parser for all the CLI arguments.
commandLineParser :: Parser ArgParser
commandLineParser = ArgParser
    <$> parseCLI

-- | Top level parser with info.
opts :: ParserInfo ArgParser
opts = info (commandLineParser <**> helper)
    ( fullDesc
    <> progDesc "Cardano genesis tool."
    <> header "Cardano genesis tool."
    )

-- | Main function.
main :: IO ()
main = do
    ArgParser CLI{..} <- execParser opts
    runCommand (decideKeyMaterialOps systemVersion) mainCommand
