{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude

import           Control.Monad (foldM, forM, when)

import           Data.Maybe (isNothing)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.ANSI (bold, green, red)

import           Prelude (String)

import           System.Directory (canonicalizePath, getCurrentDirectory, listDirectory)
import           System.Environment (setEnv, lookupEnv)
import           System.FilePath (joinPath, takeFileName, (</>))
import           System.Exit (ExitCode (..), exitFailure, exitSuccess)
import           System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import           System.Process (rawSystem)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering

  setExecutableEnvVar "CARDANO_CLI" "cardano-cli"

  tests <- List.sort . filter (`notElem` ["core", "data"]) <$> listDirectory "test/cli/"
  res <- forM tests $ \ t -> rawSystem (joinPath ["test", "cli", t, "run"]) []

  uncurry reportResult $ bimap length length $ List.partition (== ExitSuccess) res

-- -------------------------------------------------------------------------------------------------

reportResult :: Int -> Int -> IO ()
reportResult passCount failCount
  | failCount == 0 = do
      Text.putStrLn $
        green (mconcat [ Text.pack (show passCount), " tests passed, 0 failed.\n" ])
      exitSuccess
  | otherwise = do
      Text.putStrLn $
        mconcat
          [ green (mconcat [ Text.pack (show passCount), " tests passed, " ])
          , bold $ red (mconcat [ Text.pack (show failCount), " failed.\n" ])
          ]
      exitFailure

setExecutableEnvVar :: String -> FilePath -> IO ()
setExecutableEnvVar envName target = do
  -- If this is being run in Nix/CI then Nix sets the required environment variable.
  -- Set an environment variable for all the exectuables we want to test.
  mEnv <- lookupEnv "CARDANO_CLI"
  when (isNothing mEnv) $ do
    cwd <- getCurrentDirectory
    startDir <- canonicalizePath $ cwd </> "../dist-newstyle"

    xs <- listDirectoryRecursive startDir
    case filter match xs of
      [] -> panic "Unable to find cardano-cli binary"
      [x] -> setEnv envName x
      _ -> panic $ "Multiple binaries: " <> Text.pack (show xs)
 where
  match :: FilePath -> Bool
  match fp = takeFileName fp == target

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fpath = do
  xs <- fmap (fpath </>) <$> listDirectory fpath
  (files, dirs) <- foldM partitioner ([], []) xs
  rest <- concatMapM listDirectoryRecursive (dirs :: [FilePath])
  pure $ files ++ rest
 where
  partitioner :: ([FilePath], [FilePath]) -> FilePath -> IO ([FilePath], [FilePath])
  partitioner (files, dirs) fp = do
    st <- getFileStatus fp
    if
      | isRegularFile st -> pure (fp : files, dirs)
      | isDirectory st -> pure (files, fp : dirs)
      | otherwise -> pure (files, dirs)
