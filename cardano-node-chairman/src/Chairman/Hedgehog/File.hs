{-# LANGUAGE TypeApplications #-}

module Chairman.Hedgehog.File
  ( createDirectoryIfMissing
  , copyFile
  , renameFile
  , createFileLink
  , listDirectory

  , writeFile
  , openFile
  , readFile
  , lbsWriteFile
  , lbsReadFile

  , readJsonFile
  , rewriteJson

  , cat

  , assertIsJsonFile
  ) where

import           Chairman.Monad
import           Chairman.OS
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Semigroup
import           Data.String
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           System.IO (FilePath, Handle, IOMode)
import           Text.Show

import qualified Chairman.Hedgehog.Base as H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified System.IO as IO

-- | Create the 'filePath' directory if it is missing.
createDirectoryIfMissing :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
createDirectoryIfMissing filePath = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating directory if missing: " <> filePath
  H.evalIO $ IO.createDirectoryIfMissing True filePath

-- | Copy the contents of the 'src' file to the 'dst' file.
copyFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> m ()
copyFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalIO $ IO.copyFile src dst

-- | Rename the 'src' file to 'dst'.
renameFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> m ()
renameFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalIO $ IO.renameFile src dst

-- | Create a symbolic link from 'dst' to 'src'.
createFileLink :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> m ()
createFileLink src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating link from " <> show dst <> " to " <> show src
  if isWin32
    then H.evalIO $ IO.copyFile src dst
    else H.evalIO $ IO.createFileLink src dst

-- | List 'p' directory.
listDirectory :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [FilePath]
listDirectory p = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Listing directory: " <> p
  H.evalIO $ IO.listDirectory p

-- | Write 'contents' to the 'filePath' file.
writeFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> String -> m ()
writeFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ IO.writeFile filePath contents

-- | Open a handle to the 'filePath' file.
openFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> IOMode -> m Handle
openFile filePath mode = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Opening file: " <> filePath
  H.evalIO $ IO.openFile filePath mode

-- | Read the contents of the 'filePath' file.
readFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m String
readFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ IO.readFile filePath

-- | Write 'contents' to the 'filePath' file.
lbsWriteFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> LBS.ByteString -> m ()
lbsWriteFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ LBS.writeFile filePath contents

-- | Read the contents of the 'filePath' file.
lbsReadFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m LBS.ByteString
lbsReadFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ LBS.readFile filePath

-- | Read the 'filePath' file as JSON.
readJsonFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m (Either String Value)
readJsonFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading JSON file: " <> filePath
  H.evalIO $ eitherDecode @Value <$> LBS.readFile filePath

-- | Rewrite the 'filePath' JSON file using the function 'f'.
rewriteJson :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> (Value -> Value) -> m ()
rewriteJson filePath f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting JSON file: " <> filePath
  lbs <- forceM $ lbsReadFile filePath
  case eitherDecode lbs of
    Right iv -> lbsWriteFile filePath (encode (f iv))
    Left msg -> H.failMessage GHC.callStack msg

-- | Annotate the contents of the 'filePath' file.
cat :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
cat filePath = GHC.withFrozenCallStack $ do
  contents <- readFile filePath
  void . H.annotate $ L.unlines
    [ "━━━━ File: " <> filePath <> " ━━━━"
    , contents
    ]
  return ()

-- | Assert the 'filePath' can be parsed as JSON.
assertIsJsonFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertIsJsonFile fp = GHC.withFrozenCallStack $ do
  jsonResult <- readJsonFile fp
  case jsonResult of
    Right _ -> return ()
    Left msg -> H.failMessage GHC.callStack msg
