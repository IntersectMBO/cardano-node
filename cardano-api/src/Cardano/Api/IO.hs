{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api.IO
  ( OutputFile(..)

  , writeByteStringFile
  , writeByteStringOutput

  , writeLazyByteStringFile
  , writeLazyByteStringOutput

  , writeTextFile
  , writeTextOutput

  , writeFileWithOwnerPermissions
  ) where

import           Cardano.Api.Error (FileError (..))
import           Cardano.Api.IO.Compat (writeFileWithOwnerPermissions)

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
