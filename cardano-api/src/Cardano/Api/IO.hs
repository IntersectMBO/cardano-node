{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IO
  ( OutputFile(..)

  , writeByteStringFile
  , writeByteStringOutput

  , writeLazyByteStringFile
  , writeLazyByteStringOutput

  , writeTextFile
  , writeTextOutput

  , writeLazyByteStringFileWithOwnerPermissions
  ) where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

#ifdef UNIX
import           Control.Exception (IOException, bracket, bracketOnError, try)
import           System.Directory ()
import           System.IO (hClose)
import           System.Posix.Files (ownerModes, setFdOwnerAndGroup)
import           System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, fdToHandle, openFd)
import           System.Posix.User (getRealUserID)
#else
import           Control.Exception (bracketOnError)
import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
import           System.IO (hClose, openTempFile)
#endif

import           Cardano.Api.Error (FileError (..))

import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as LBSC
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           GHC.Generics (Generic)
import           System.IO (Handle)

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
        hClose
        (runExceptT . handleIOExceptT (FileIOError path) . f)
#else
  -- On something other than unix, we make a _new_ file, and since we created it,
  -- we must own it. We then place it at the target location. Unfortunately this
  -- won't work correctly with pseudo-files.
  bracketOnError
    (openTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, h) -> do
      hClose h >> removeFile tmpPath
      return . Left $ FileErrorTempFile path tmpPath h)
    (\(tmpPath, h) -> do
        f h
        hClose h
        renameFile tmpPath path
        return $ Right ())
  where
    (targetDir, targetFile) = splitFileName path
#endif

newtype OutputFile = OutputFile
  { unOutputFile :: FilePath
  }
  deriving Generic
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON)

writeByteStringFile :: MonadIO m => FilePath -> ByteString -> m (Either (FileError ()) ())
writeByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError fp) $ BS.writeFile fp bs

writeByteStringOutput :: MonadIO m => Maybe FilePath -> ByteString -> m (Either (FileError ()) ())
writeByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError fp) $ BS.writeFile fp bs
    Nothing -> liftIO $ BSC.putStr bs

writeLazyByteStringFile :: MonadIO m => FilePath -> LBS.ByteString -> m (Either (FileError ()) ())
writeLazyByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError fp) $ LBS.writeFile fp bs

writeLazyByteStringOutput :: MonadIO m => Maybe FilePath -> LBS.ByteString -> m (Either (FileError ()) ())
writeLazyByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError fp) $ LBS.writeFile fp bs
    Nothing -> liftIO $ LBSC.putStr bs

writeTextFile :: MonadIO m => FilePath -> Text -> m (Either (FileError ()) ())
writeTextFile fp t = runExceptT $
  handleIOExceptT (FileIOError fp) $ Text.writeFile fp t

writeTextOutput :: MonadIO m => Maybe FilePath -> Text -> m (Either (FileError ()) ())
writeTextOutput mOutput t = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError fp) $ Text.writeFile fp t
    Nothing -> liftIO $ Text.putStr t

writeLazyByteStringFileWithOwnerPermissions
  :: FilePath
  -> LBS.ByteString
  -> IO (Either (FileError ()) ())
writeLazyByteStringFileWithOwnerPermissions fp lbs =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    LBS.hPut h lbs
