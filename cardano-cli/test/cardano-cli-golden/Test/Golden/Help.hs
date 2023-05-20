{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant id" -}

module Test.Golden.Help
  ( helpTests
  ) where

import           Prelude hiding (lines)

import           Control.Monad (forM_, unless, (<=<))
import           Data.Maybe (maybeToList)
import           Data.Text (Text)
import           Hedgehog (Property)
import           Hedgehog.Extras.Stock.OS (isWin32)
import           System.FilePath ((</>))
import           Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce)
import           Text.Regex (Regex, mkRegex, subRegex)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H
import qualified Test.Cardano.CLI.Util as H

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
  propertyOnce . H.moduleWorkspace "help" $ \_ -> do
    -- These tests are not run on Windows because the cardano-cli usage
    -- output is slightly different on Windows.  For example it uses
    -- "cardano-cli.exe" instead of "cardano-cli".
    unless isWin32 $ do
      helpFp <- H.note "test/cardano-cli-golden/files/golden/help.cli"

      help <- filterAnsi <$> execCardanoCLI
        [ "help"
        ]

      H.diffVsGoldenFile help helpFp

third :: (a, b, c) -> c
third (_, _, c) = c

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
selectCmd = selectAndDropPrefix "Usage: cardano-cli " <=< deselectSuffix " COMMAND"

golden_HelpCmds :: Property
golden_HelpCmds =
  propertyOnce . H.moduleWorkspace "help-commands" $ \_ -> do
    -- These tests are not run on Windows because the cardano-cli usage
    -- output is slightly different on Windows.  For example it uses
    -- "cardano-cli.exe" instead of "cardano-cli".
    unless isWin32 $ do
      help <- filterAnsi <$> execCardanoCLI
        [ "help"
        ]

      let lines = Text.lines $ Text.pack help
      let usages = List.filter (not . null) $ fmap extractCmd $ maybeToList . selectCmd =<< lines

      forM_ usages $ \usage -> do
        H.noteShow_ usage
        let expectedCmdHelpFp = "test/cardano-cli-golden/files/golden/help" </> Text.unpack (Text.intercalate "_" usage) <> ".cli"

        cmdHelp <- filterAnsi . third <$> H.execDetailCardanoCli (fmap Text.unpack usage)

        H.diffVsGoldenFile cmdHelp expectedCmdHelpFp

helpTests :: IO Bool
helpTests =
  H.checkSequential $ H.Group "Help"
    [ ( "golden_HelpAll"
      , Test.Golden.Help.golden_HelpAll
      )
    , ( "golden_HelpCmds"
      , Test.Golden.Help.golden_HelpCmds
      )
    ]
