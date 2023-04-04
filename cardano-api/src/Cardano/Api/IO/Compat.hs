{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IO.Compat
  ( writeFileWithOwnerPermissions
  ) where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.Api.Error (FileError (..))

import           Control.Monad.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import qualified Data.ByteString.Lazy as LBS

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

writeFileWithOwnerPermissions
  :: FilePath
  -> LBS.ByteString
  -> IO (Either (FileError ()) ())
#ifdef UNIX
-- On a unix based system, we grab a file descriptor and set ourselves as owner.
-- Since we're holding the file descriptor at this point, we can be sure that
-- what we're about to write to is owned by us if an error didn't occur.
writeFileWithOwnerPermissions path a = do
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
          (\handle -> runExceptT $ handleIOExceptT (FileIOError path) $ LBS.hPut handle a)
#else
-- On something other than unix, we make a _new_ file, and since we created it,
-- we must own it. We then place it at the target location. Unfortunately this
-- won't work correctly with pseudo-files.
writeFileWithOwnerPermissions targetPath a =
    bracketOnError
      (openTempFile targetDir $ targetFile <.> "tmp")
      (\(tmpPath, fHandle) -> do
        hClose fHandle >> removeFile tmpPath
        return . Left $ FileErrorTempFile targetPath tmpPath fHandle)
      (\(tmpPath, fHandle) -> do
          LBS.hPut fHandle a
          hClose fHandle
          renameFile tmpPath targetPath
          return $ Right ())
  where
    (targetDir, targetFile) = splitFileName targetPath
#endif
