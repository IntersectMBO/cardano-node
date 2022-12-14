{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.Search
  ( searchMessagesInLogs
  ) where

import           Control.Monad (forM)
import           Control.Monad.Extra (whenM)
import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import           System.Directory (doesDirectoryExist, makeAbsolute)
import           System.Directory.Extra (listDirectories)

import           Cardano.Logging (TraceObject (..))

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types

-- | Search messages in the corresponding log(s).
--   TODO: extend search criteria!
searchMessagesInLogs
  :: TracerEnv
  -> [NodeName]
  -> Text
  -> IO [TraceObject]
searchMessagesInLogs _ [] _  = return []
searchMessagesInLogs _ _  "" = return []
searchMessagesInLogs TracerEnv{teConfig} _nodesToSearch _whatToSearch = do
  _ <-
    forM loggingParamsForFiles $ \LoggingParams{logRoot{-, logFormat-}} -> do
      logRootAbs <- makeAbsolute logRoot
      whenM (doesDirectoryExist logRootAbs) $ do
        _logsSubDirs <- listDirectories logRootAbs
        return ()
  return []
 where
  loggingParamsForFiles = nub . NE.filter filesOnly $ logging teConfig
  filesOnly LoggingParams{logMode} = logMode == FileMode
