{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IO
  ( writeByteStringFileWithOwnerPermissions
  , writeByteStringFile
  , writeByteStringOutput

  , writeLazyByteStringFileWithOwnerPermissions
  , writeLazyByteStringFile
  , writeLazyByteStringOutput

  , writeTextFileWithOwnerPermissions
  , writeTextFile
  , writeTextOutput

  , File(..)
  , MapFile(..)
  , HasFileMode(..)
  , Directory(..)
  , FileDirection(..)
  ) where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

#ifdef UNIX
import           Control.Exception (IOException, bracket, bracketOnError, try)
import           System.Directory ()
import           System.Posix.Files (ownerModes, setFdOwnerAndGroup)
import           System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, fdToHandle, openFd)
import           System.Posix.User (getRealUserID)
#else
import           Control.Exception (bracketOnError)
import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
#endif

import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as LBSC
import           Data.Kind (Type)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified System.IO as IO
import           System.IO (Handle)

import           Cardano.Api.Error (FileError (..))

data FileDirection
  = In
  -- ^ Indicate the file is to be used for reading.
  | Out
  -- ^ Indicate the file is to be used for writing.
  | InOut
  -- ^ Indicate the file is to be used for both reading and writing.

-- | A file path with additional type information to indicate whether it is to
-- be used for reading or writing.
newtype File (direction :: FileDirection) = File
  { unFile :: FilePath
  } deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | A directory path.
newtype Directory = Directory
  { unDirectory :: FilePath
  } deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

handleFileForWritingWithOwnerPermission
  :: File 'Out
  -> (Handle -> IO ())
  -> IO (Either (FileError ()) ())
handleFileForWritingWithOwnerPermission path f = do
#ifdef UNIX
  -- On a unix based system, we grab a file descriptor and set ourselves as owner.
  -- Since we're holding the file descriptor at this point, we can be sure that
  -- what we're about to write to is owned by us if an error didn't occur.
  user <- getRealUserID
  ownedFile <- try $
    -- We only close the FD on error here, otherwise we let it leak out, since
    -- it will be immediately turned into a Handle (which will be closed when
    -- the Handle is closed)
    bracketOnError
      (openFd (unFile path) WriteOnly (Just ownerModes) defaultFileFlags)
      closeFd
      (\fd -> setFdOwnerAndGroup fd user (-1) >> pure fd)
  case ownedFile of
    Left (err :: IOException) -> do
      pure $ Left $ FileIOError (unFile path) err
    Right fd -> do
      bracket
        (fdToHandle fd)
        IO.hClose
        (runExceptT . handleIOExceptT (FileIOError (unFile path)) . f)
#else
  -- On something other than unix, we make a _new_ file, and since we created it,
  -- we must own it. We then place it at the target location. Unfortunately this
  -- won't work correctly with pseudo-files.
  bracketOnError
    (IO.openTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, h) -> do
      IO.hClose h >> IO.removeFile tmpPath
      return . Left $ FileErrorTempFile path tmpPath h)
    (\(tmpPath, h) -> do
        f h
        IO.hClose h
        renameFile tmpPath path
        return $ Right ())
  where
    (targetDir, targetFile) = splitFileName path
#endif

writeByteStringFile :: MonadIO m => File 'Out -> ByteString -> m (Either (FileError ()) ())
writeByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ BS.writeFile (unFile fp) bs

writeByteStringFileWithOwnerPermissions
  :: File 'Out
  -> BS.ByteString
  -> IO (Either (FileError ()) ())
writeByteStringFileWithOwnerPermissions fp bs =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    BS.hPut h bs

writeByteStringOutput :: MonadIO m => Maybe (File 'Out) -> ByteString -> m (Either (FileError ()) ())
writeByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ BS.writeFile (unFile fp) bs
    Nothing -> liftIO $ BSC.putStr bs

writeLazyByteStringFile :: MonadIO m => File 'Out -> LBS.ByteString -> m (Either (FileError ()) ())
writeLazyByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ LBS.writeFile (unFile fp) bs

writeLazyByteStringFileWithOwnerPermissions
  :: File 'Out
  -> LBS.ByteString
  -> IO (Either (FileError ()) ())
writeLazyByteStringFileWithOwnerPermissions fp lbs =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    LBS.hPut h lbs

writeLazyByteStringOutput :: MonadIO m => Maybe (File 'Out) -> LBS.ByteString -> m (Either (FileError ()) ())
writeLazyByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ LBS.writeFile (unFile fp) bs
    Nothing -> liftIO $ LBSC.putStr bs

writeTextFile :: MonadIO m => File 'Out -> Text -> m (Either (FileError ()) ())
writeTextFile fp t = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ Text.writeFile (unFile fp) t

writeTextFileWithOwnerPermissions
  :: File 'Out
  -> Text
  -> IO (Either (FileError ()) ())
writeTextFileWithOwnerPermissions fp t =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    Text.hPutStr h t

writeTextOutput :: MonadIO m => Maybe (File 'Out) -> Text -> m (Either (FileError ()) ())
writeTextOutput mOutput t = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ Text.writeFile (unFile fp) t
    Nothing -> liftIO $ Text.putStr t

class MapFile a where
  mapFile :: (FilePath -> FilePath) -> a -> a

instance MapFile (File direction) where
  mapFile f = File . f . unFile

class HasFileMode (f :: FileDirection -> Type) where
  usingIn :: f 'InOut -> f 'In
  usingOut :: f 'InOut -> f 'Out

instance HasFileMode File where
  usingIn :: File 'InOut -> File 'In
  usingIn = File . unFile

  usingOut :: File 'InOut -> File 'Out
  usingOut = File . unFile
