{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant id" -}

module Cardano.Testnet.Test.Golden.Help
  ( golden_HelpAll
  , golden_HelpCmds
  , golden_HelpReadme
  , golden_VersionCmd
  ) where

import           Cardano.Testnet.Test.Golden.Util

import           Prelude hiding (lines)

import           Control.Monad (forM, forM_, unless, (<=<))
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Maybe (maybeToList)
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath ((</>))
import           Text.Regex (Regex, mkRegex, subRegex)

import           Hedgehog (MonadTest, Property)
import qualified Hedgehog.Extras as H
import           Hedgehog.Extras.Stock.OS (isWin32)

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

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_HelpAll/"'@
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

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_HelpCmds/"'@
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

-- | A code block of the README that is annotated with a @checked-help@
-- marker comment:
--
-- > <!-- checked-help: cardano-testnet ARGS... -->
-- > <!-- checked-help: cardano-testnet ARGS... | only: --option1 --option2 -->
--
-- The lines between the code fences that follow the marker must match the
-- output of running @cardano-testnet ARGS...@, with ANSI codes and trailing
-- whitespace stripped. When the marker has an @only:@ list, the block must
-- match just the help entries of the listed options, in the given order.
data ReadmeHelpBlock = ReadmeHelpBlock
  { rhbArgs :: [Text]
    -- ^ Arguments to pass to the cardano-testnet binary
  , rhbOnly :: Maybe [Text]
    -- ^ When given, only the help entries of these options
  , rhbStart :: Int
    -- ^ Index in the README of the first line of the block
  , rhbEnd :: Int
    -- ^ Index in the README just past the last line of the block
  }

readmeHelpMarker :: Text
readmeHelpMarker = "<!-- checked-help:"

-- | Find the @checked-help@ blocks of the README. Malformed markers are an
-- error, so that a typo cannot silently disable a check.
parseReadmeHelpBlocks :: FilePath -> [Text] -> Either String [ReadmeHelpBlock]
parseReadmeHelpBlocks readmeFp readmeLines = go (List.zip [0 ..] readmeLines)
  where
    go :: [(Int, Text)] -> Either String [ReadmeHelpBlock]
    go [] = Right []
    go ((ix, line) : rest)
      | readmeHelpMarker `Text.isPrefixOf` Text.strip line = do
          (args, mOnly) <- parseMarker ix line
          (end, rest') <- takeFencedBlock ix rest
          restBlocks <- go rest'
          Right $ ReadmeHelpBlock args mOnly (ix + 2) end : restBlocks
      | otherwise = go rest

    atLine :: Int -> String -> String
    atLine ix msg = readmeFp <> ":" <> show (ix + 1) <> ": " <> msg

    parseMarker :: Int -> Text -> Either String ([Text], Maybe [Text])
    parseMarker ix line = do
      body <- case Text.stripPrefix readmeHelpMarker
                     =<< Text.stripSuffix "-->" (Text.strip line) of
        Nothing -> Left $ atLine ix "malformed checked-help marker: expected <!-- checked-help: cardano-testnet ... -->"
        Just body -> Right body
      let (cmdPart, onlyPart) = Text.breakOn "|" body
      args <- case Text.words cmdPart of
        "cardano-testnet" : args -> Right args
        _ -> Left $ atLine ix "the command of a checked-help marker must start with cardano-testnet"
      mOnly <- case Text.stripPrefix "|" onlyPart of
        Nothing -> Right Nothing
        Just afterPipe -> case Text.stripPrefix "only:" (Text.strip afterPipe) of
          Nothing -> Left $ atLine ix "expected 'only: --option...' after '|' in checked-help marker"
          Just optionsText ->
            let options = Text.words optionsText
            in if not (List.null options) && List.all ("-" `Text.isPrefixOf`) options
                 then Right (Just options)
                 else Left $ atLine ix "the 'only:' list of a checked-help marker must be options starting with -"
      Right (args, mOnly)

    takeFencedBlock :: Int -> [(Int, Text)] -> Either String (Int, [(Int, Text)])
    takeFencedBlock ix rest = case rest of
      (_, fenceLine) : afterFence
        | "```" `Text.isPrefixOf` Text.strip fenceLine ->
            case List.break (\(_, l) -> Text.strip l == "```") afterFence of
              (_, (closeIx, _) : rest') -> Right (closeIx, rest')
              (_, []) -> Left $ atLine (ix + 1) "unterminated code fence after checked-help marker"
      _ -> Left $ atLine ix "a checked-help marker must be immediately followed by a code fence"

-- | Split help output into option entries: an entry starts with a line like
-- @"  --option ..."@ and continues over the following further-indented lines.
helpOptionEntries :: [Text] -> [[Text]]
helpOptionEntries helpLines = case List.dropWhile (not . isEntryStart) helpLines of
  [] -> []
  entryStart : rest ->
    let (continuation, rest') = List.span isContinuation rest
    in (entryStart : continuation) : helpOptionEntries rest'
  where
    isEntryStart l = "  -" `Text.isPrefixOf` l
    isContinuation l = "   " `Text.isPrefixOf` l

-- | The help entry of the given option, e.g. @--epoch-length@.
entryForOption :: [[Text]] -> Text -> Either String [Text]
entryForOption entries option =
  case List.filter matches entries of
    [entry] -> Right entry
    [] -> Left $ "option " <> Text.unpack option <> " does not appear in the help output"
    _ -> Left $ "option " <> Text.unpack option <> " appears more than once in the help output"
  where
    matches [] = False
    matches (firstLine : _) = case Text.stripPrefix option (Text.stripStart firstLine) of
      Nothing -> False
      Just rest -> case Text.uncons rest of
        Nothing -> True
        Just (c, _) -> c == ' ' || c == ','

-- | What a checked-help block must contain: the output of running
-- cardano-testnet with the arguments of the marker, with ANSI codes and
-- trailing whitespace stripped, restricted to the requested option entries
-- when the marker has an @only:@ list.
expectedHelpBlock
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [Text] -> Maybe [Text] -> m [Text]
expectedHelpBlock args mOnly = GHC.withFrozenCallStack $ do
  (_, stdout, _) <- execDetailCardanoTestnet (fmap Text.unpack args)
  let helpLines = fmap Text.stripEnd . Text.lines . Text.pack $ filterAnsi stdout
  case mOnly of
    Nothing -> pure helpLines
    Just options ->
      case concat <$> traverse (entryForOption (helpOptionEntries helpLines)) options of
        Left err -> H.failMessage GHC.callStack $
          "cardano-testnet " <> Text.unpack (Text.unwords args) <> ": " <> err
        Right entryLines -> pure entryLines

-- | Checks that the help blocks embedded in cardano-testnet's README.md (the
-- code blocks annotated with a @checked-help@ comment, see 'ReadmeHelpBlock')
-- match the output of the cardano-testnet binary. The README as it should be
-- is diffed against the README on disk as its own golden file, so the line
-- numbers of a failure diff are real README line numbers, and running with
-- @RECREATE_GOLDEN_FILES=1@ refreshes the help blocks in place.
--
-- Not run on Windows (registered with 'Testnet.Property.Run.ignoreOnWindows'):
-- the cardano-testnet usage output is slightly different there, for example it
-- uses "cardano-testnet.exe" instead of "cardano-testnet".
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_HelpReadme/"'@
golden_HelpReadme :: Property
golden_HelpReadme =
  H.propertyOnce $ do
    base <- H.getProjectBase
    readmeFp <- H.note $ base </> "cardano-testnet/README.md"
    readmeLines <- Text.lines . Text.pack <$> H.readFile readmeFp

    blocks <- case parseReadmeHelpBlocks readmeFp readmeLines of
      Left err -> H.failMessage GHC.callStack err
      Right [] -> H.failMessage GHC.callStack $
        "no checked-help blocks found in " <> readmeFp <> ": the help shown in it is no longer checked"
      Right blocks -> pure blocks

    expected <- forM blocks $ \block -> expectedHelpBlock (rhbArgs block) (rhbOnly block)

    let newReadme = List.foldr splice readmeLines (List.zip blocks expected)
    H.diffVsGoldenFile (Text.unpack $ Text.unlines newReadme) readmeFp
  where
    splice (block, expectedContent) readme =
      List.take (rhbStart block) readme <> expectedContent <> List.drop (rhbEnd block) readme

-- | Filter out volatile lines from 'cardano-testnet version' output.
-- Removes the first two lines: The first one contains package version,
-- OS, arch, compiler and the second one is the git rev line.
filterVersionOutput :: String -> String
filterVersionOutput =
  unlines . drop 2 . map Text.unpack . Text.lines . Text.pack

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_VersionCmd/"'@
golden_VersionCmd :: Property
golden_VersionCmd =
  H.propertyOnce . H.moduleWorkspace "version-cmd" $ \_ -> do
    unless isWin32 $ do
      versionFp <- H.note "test/cardano-testnet-golden/files/golden/version_cmd.cli"

      versionOutput <- filterVersionOutput . filterAnsi <$> execCardanoTestnet
        [ "version"
        ]

      H.diffVsGoldenFile versionOutput versionFp
