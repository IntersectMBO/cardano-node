{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , FileDirection(..)

  , mapFile
  , onlyIn
  , onlyOut

  , intoFile
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
import qualified System.Directory as IO
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

-- | A file path with additional type information to indicate what the file is meant to
-- contain and whether it is to be used for reading or writing.
newtype File content (direction :: FileDirection) = File
  { unFile :: FilePath
  } deriving newtype (Eq, Ord, Read, Show, IsString, FromJSON, ToJSON)

handleFileForWritingWithOwnerPermission
  :: FilePath
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
      (openFd path WriteOnly (Just ownerModes) defaultFileFlags)
      closeFd
      (\fd -> setFdOwnerAndGroup fd user (-1) >> pure fd)
  case ownedFile of
    Left (err :: IOException) -> do
      pure $ Left $ FileIOError path err
    Right fd -> do
      bracket
        (fdToHandle fd)
        IO.hClose
        (runExceptT . handleIOExceptT (FileIOError path) . f)
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
        IO.renameFile tmpPath path
        return $ Right ())
  where
    (targetDir, targetFile) = splitFileName path
#endif

writeByteStringFile :: ()
  => MonadIO m
  => File content Out
  -> ByteString
  -> m (Either (FileError ()) ())
writeByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ BS.writeFile (unFile fp) bs

writeByteStringFileWithOwnerPermissions
  :: FilePath
  -> BS.ByteString
  -> IO (Either (FileError ()) ())
writeByteStringFileWithOwnerPermissions fp bs =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    BS.hPut h bs

writeByteStringOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> ByteString
  -> m (Either (FileError ()) ())
writeByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ BS.writeFile (unFile fp) bs
    Nothing -> liftIO $ BSC.putStr bs

writeLazyByteStringFile :: ()
  => MonadIO m
  => File content Out
  -> LBS.ByteString
  -> m (Either (FileError ()) ())
writeLazyByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ LBS.writeFile (unFile fp) bs

writeLazyByteStringFileWithOwnerPermissions
  :: File content Out
  -> LBS.ByteString
  -> IO (Either (FileError ()) ())
writeLazyByteStringFileWithOwnerPermissions fp lbs =
  handleFileForWritingWithOwnerPermission (unFile fp) $ \h ->
    LBS.hPut h lbs

writeLazyByteStringOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> LBS.ByteString
  -> m (Either (FileError ()) ())
writeLazyByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ LBS.writeFile (unFile fp) bs
    Nothing -> liftIO $ LBSC.putStr bs

writeTextFile :: ()
  => MonadIO m
  => File content Out
  -> Text
  -> m (Either (FileError ()) ())
writeTextFile fp t = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ Text.writeFile (unFile fp) t

writeTextFileWithOwnerPermissions
  :: File content Out
  -> Text
  -> IO (Either (FileError ()) ())
writeTextFileWithOwnerPermissions fp t =
  handleFileForWritingWithOwnerPermission (unFile fp) $ \h ->
    Text.hPutStr h t

writeTextOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> Text
  -> m (Either (FileError ()) ())
writeTextOutput mOutput t = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ Text.writeFile (unFile fp) t
    Nothing -> liftIO $ Text.putStr t

mapFile :: (FilePath -> FilePath) -> File content direction -> File content direction
mapFile f = File . f . unFile

onlyIn :: File content InOut -> File content In
onlyIn = File . unFile

onlyOut :: File content InOut -> File content Out
onlyOut = File . unFile

-- | Given a way to serialise a value and a way to write the stream to a file, serialise
-- a value into a stream, and write it to a file.
--
-- Whilst it is possible to call the serialisation and writing functions separately,
-- doing so means the compiler is unable to match the content type of the file with
-- the type of the content being serialised.
--
-- Using this function ensures that the content type of the file always matches with the
-- content value and prevents any type mismatches.
intoFile :: ()
  => File content 'Out
  -> content
  -> (File content 'Out -> stream -> result)
  -> (content -> stream)
  -> result
intoFile fp content write serialise = write fp (serialise content)
