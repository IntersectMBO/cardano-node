{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant id" -}

module Cardano.Testnet.Test.Golden.Help
  ( golden_HelpAll
  , golden_HelpCmds
  ) where

import           Cardano.Testnet.Test.Golden.Util

import           Prelude hiding (lines)

import           Control.Monad (forM_, unless, (<=<))
import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Maybe (maybeToList)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.FilePath ((</>))
import           Text.Regex (Regex, mkRegex, subRegex)

import           Hedgehog (Property)
import qualified Hedgehog.Extras as H
import           Hedgehog.Extras.Stock.OS (isWin32)
import qualified Hedgehog.Extras.Test.Golden as H

ansiRegex :: Regex
ansiRegex = mkRegex "\\[[0-9]+m"

filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex stripped ""
  where stripped = filter (/= '\ESC') line

{- HLINT ignore "Use camelCase" -}

extractCmd :: Text -> [Text]
extractCmd = id
  . takeWhile nonSwitch
  . Text.split Char.isSpace
  . Text.strip
  where nonSwitch :: Text -> Bool
        nonSwitch s =
          case Text.unpack (Text.take 1 s) of
            (c:_) -> Char.isAlpha c
            [] -> False

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_HelpAll :: Property
golden_HelpAll =
  H.propertyOnce . H.moduleWorkspace "help" $ \_ -> do
    -- These tests are not run on Windows because the cardano-testnet usage
    -- output is slightly different on Windows.  For example it uses
    -- "cardano-testnet.exe" instead of "cardano-testnet".
    unless isWin32 $ do
      helpFp <- H.note "test/cardano-testnet-golden/files/golden/help.cli"

      help <- filterAnsi <$> execCardanoTestnet
        [ "help"
        ]

      H.diffVsGoldenFile help helpFp

second :: (a, b, c) -> b
second (_, b, _) = b

-- | Return the string with the prefix dropped if the prefix is present, otherwise return Nothing.
selectAndDropPrefix :: Text -> Text -> Maybe Text
selectAndDropPrefix prefix text =
  if Text.isPrefixOf prefix text
    then Just $ Text.drop (Text.length prefix) text
    else Nothing

deselectSuffix :: Text -> Text -> Maybe Text
deselectSuffix suffix text =
  if Text.isSuffixOf suffix text
    then Nothing
    else Just text

selectCmd :: Text -> Maybe Text
selectCmd = selectAndDropPrefix "Usage: cardano-testnet " <=< deselectSuffix " COMMAND"

golden_HelpCmds :: Property
golden_HelpCmds =
  H.propertyOnce . H.moduleWorkspace "help-commands" $ \_ -> do
    -- These tests are not run on Windows because the cardano-testnet usage
    -- output is slightly different on Windows.  For example it uses
    -- "cardano-testnet.exe" instead of "cardano-testnet".
    unless isWin32 $ do
      help <- filterAnsi <$> execCardanoTestnet
        [ "help"
        ]
      let lines = Text.lines $ Text.pack help
      let usages = List.filter (not . null) $ fmap extractCmd $ maybeToList . selectCmd =<< lines

      forM_ usages $ \usage -> do
        H.noteShow_ usage
        let expectedCmdHelpFp = "test/cardano-testnet-golden/files/golden/help" </> Text.unpack (Text.intercalate "_" usage) <> ".cli"

        cmdHelp <- filterAnsi . second <$> execDetailCardanoTestnet (fmap Text.unpack usage <> ["--help"])

        H.diffVsGoldenFile cmdHelp expectedCmdHelpFp
