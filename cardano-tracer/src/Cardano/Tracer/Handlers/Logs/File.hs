{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Control.Monad (unless)
import           Control.Monad.Extra (ifM, unlessM)
import           Data.Aeson (ToJSON, (.=), object, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))

import           Cardano.Logging

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (createLogAndSymLink, doesSymLinkValid,
                                                   symLinkName)
import           Cardano.Tracer.Types

-- | Log files structure can be represented like this (for example, with json format):
--
--   rootDir/
--     nodeFullId/
--       node-*-1.json
--       node-*-2.json
--       node-*-3.json
--       ...
--       node.json -> node-*-3.json
--
-- So 'TraceObject's received from the node will be stored in the logs
-- saved in the corresponding subdirectory inside of 'rootDir'.
writeTraceObjectsToFile
  :: NodeId
  -> Text
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile _ _ _ _ [] = return ()
writeTraceObjectsToFile nodeId nodeName rootDir format traceObjects = do
  createDirectoryIfMissing True subDirForLogs

  ifM (doesFileExist pathToCurrentLog)
    (unlessM (doesSymLinkValid pathToCurrentLog) $ do
      removeFile pathToCurrentLog
      createLogAndSymLink subDirForLogs format)
    (createLogAndSymLink subDirForLogs format)

  writeTraceObjects pathToCurrentLog $ formatter format
 where
  subDirForLogs = rootDir </> nodeFullId
  nodeFullId = if T.null nodeName
                 then show nodeId
                 else T.unpack nodeName <> "-" <> show nodeId
  -- This is a symlink to the current log file, please see rotation parameters.
  pathToCurrentLog = subDirForLogs </> symLinkName format

  formatter ForHuman   = traceObjectToText
  formatter ForMachine = traceObjectToJSON

  writeTraceObjects logPath formatIt = do
    let itemsToWrite = mapMaybe formatIt traceObjects
    unless (null itemsToWrite) $
      -- It's much more efficiently to encode 'Text' explicitly and
      -- then perform 'ByteString'-level 'IO' than perform 'Text'-level 'IO'.
      LBS.appendFile logPath . encodeUtf8 . TL.concat $ itemsToWrite

nl :: TL.Text
#if defined(mingw32_HOST_OS)
nl = "\r\n"
#else
nl = "\n"
#endif

traceObjectToText :: TraceObject -> Maybe TL.Text
traceObjectToText TraceObject{..} =
  case toHuman of
    Nothing -> Nothing
    Just msgForHuman -> Just $
      "[" <> host <> ":" <> name <> ":" <> sev <> ":" <> thId <> "] [" <> time <> "] "
      <> TL.fromStrict msgForHuman
      <> nl
 where
  host = TL.pack toHostname
  name = mkName toNamespace
  sev  = TL.pack $ show toSeverity
  thId = TL.fromStrict $ T.filter isDigit toThreadId
  time = TL.pack $ formatTime defaultTimeLocale "%F %T%2Q %Z" toTimestamp

mkName :: Namespace -> TL.Text
mkName []    = "noname"
mkName names = TL.fromStrict $ T.intercalate "." names

data TraceObjectForJSON = TraceObjectForJSON
  { jAt   :: !UTCTime
  , jNS   :: !T.Text
  , jData :: !T.Text
  , jHost :: !T.Text
  , jSev  :: !T.Text
  , jTId  :: !T.Text
  }

instance ToJSON TraceObjectForJSON where
  toJSON TraceObjectForJSON{..} =
    object [ "at"     .= formatTime defaultTimeLocale "%FT%T%2Q%Z" jAt
           , "ns"     .= jNS
           , "data"   .= jData
           , "host"   .= jHost
           , "sev"    .= jSev
           , "thread" .= jTId
           ]

traceObjectToJSON :: TraceObject -> Maybe TL.Text
traceObjectToJSON TraceObject{..} =
  case toMachine of
    Nothing -> Nothing
    Just msgForMachine -> Just . TL.append nl . encodeToLazyText $
      TraceObjectForJSON
        { jAt   = toTimestamp
        , jNS   = TL.toStrict $ mkName toNamespace
        , jData = msgForMachine
        , jHost = T.pack toHostname
        , jSev  = T.pack $ show toSeverity
        , jTId  = T.filter isDigit toThreadId
        }
