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

import           Chairman.Hedgehog.Base (Integration)
import           Chairman.Monad
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
import           System.IO (FilePath, Handle, IOMode)
import           Text.Show

import qualified Chairman.Hedgehog.Base as H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified System.Directory as IO
import qualified System.IO as IO


createDirectoryIfMissing :: HasCallStack => FilePath -> Integration ()
createDirectoryIfMissing filePath = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating directory if missing: " <> filePath
  H.evalIO $ IO.createDirectoryIfMissing True filePath

copyFile :: HasCallStack => FilePath -> FilePath -> Integration ()
copyFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalM . liftIO $ IO.copyFile src dst

renameFile :: HasCallStack => FilePath -> FilePath -> Integration ()
renameFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalM . liftIO $ IO.renameFile src dst

createFileLink :: HasCallStack => FilePath -> FilePath -> Integration ()
createFileLink src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating link from " <> show dst <> " to " <> show src
  H.evalM . liftIO $ IO.copyFile src dst

listDirectory :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m [FilePath]
listDirectory p = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Listing directory: " <> p
  H.evalIO $ IO.listDirectory p

writeFile :: (MonadIO m, HasCallStack) => FilePath -> String -> H.PropertyT m ()
writeFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ IO.writeFile filePath contents

openFile :: (MonadIO m, HasCallStack) => FilePath -> IOMode -> H.PropertyT m Handle
openFile filePath mode = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Opening file: " <> filePath
  H.evalIO $ IO.openFile filePath mode

readFile :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m String
readFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ IO.readFile filePath

lbsWriteFile :: (MonadIO m, HasCallStack) => FilePath -> LBS.ByteString -> H.PropertyT m ()
lbsWriteFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ LBS.writeFile filePath contents

lbsReadFile :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m LBS.ByteString
lbsReadFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ LBS.readFile filePath

readJsonFile :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m (Either String Value)
readJsonFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading JSON file: " <> filePath
  H.evalIO $ eitherDecode @Value <$> LBS.readFile filePath

rewriteJson :: (MonadIO m, HasCallStack) => FilePath -> (Value -> Value) -> H.PropertyT m ()
rewriteJson filePath f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting JSON file: " <> filePath
  lbs <- forceM $ lbsReadFile filePath
  case eitherDecode lbs of
    Right iv -> lbsWriteFile filePath (encode (f iv))
    Left msg -> H.failMessage GHC.callStack msg

cat :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m ()
cat filePath = GHC.withFrozenCallStack $ do
  contents <- readFile filePath
  void . H.annotate $ L.unlines
    [ "━━━━ File: " <> filePath <> " ━━━━"
    , contents
    ]
  return ()

assertIsJsonFile :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m ()
assertIsJsonFile fp = GHC.withFrozenCallStack $ do
  jsonResult <- readJsonFile fp
  case jsonResult of
    Right _ -> return ()
    Left msg -> H.failMessage GHC.callStack msg
